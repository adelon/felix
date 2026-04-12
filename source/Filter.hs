module Filter where


import Base
import Syntax.Internal

import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Map qualified as Map
import GHC.Float (int2Float)
import Bound.Scope


convergence :: Float
convergence = 2.8


passmark :: Float
passmark = 0.4


filterTask :: Task -> Task
filterTask Task{taskDirectness = directness, taskConjectureLabel = label, taskConjecture = conjecture, taskHypotheses = hypotheses, taskLocation = loc} =
    let
        motive = case directness of
            Indirect formerConjecture -> formerConjecture
            Direct -> conjecture
        relevant = relevantFacts passmark motive (Set.fromList hypotheses)
        filteredHypos = if length hypotheses < 20
            then hypotheses
            else List.filter (`Map.member` relevant) hypotheses
    in Task
        { taskDirectness = directness
        , taskConjecture = conjecture
        , taskHypotheses = filteredHypos
        , taskConjectureLabel = label
        , taskLocation = loc
        }


data Feature
    = FeatureSymbol Symbol
    | FeatureStruct StructSymbol
    deriving (Show, Eq, Ord)


relevantFacts :: Float -> ExprOf a -> Set Hypothesis -> Map Hypothesis Float
relevantFacts p conjecture cs = relevantClausesNaiveFeatures p (features conjecture) cs Map.empty


relevantFactsWeighted :: Float -> ExprOf a -> Set Hypothesis -> Map Hypothesis Float
relevantFactsWeighted p conjecture cs =
    let
        ftab = Map.unionsWith (+) (featureTable . hypothesisFormula <$> Set.toList cs)
        cfeatures = Map.fromSet (features . hypothesisFormula) cs
    in relevantClauses p ftab (features conjecture) cfeatures Map.empty


relevantClauses
    :: Float -- ^ Pass mark
    -> Map Feature Int -- ^ Feature frequencies in the candidate facts
    -> Set Feature -- ^ Relevant features
    -> Map Hypothesis (Set Feature) -- ^ Working irrelevant facts and their features
    -> Map Hypothesis Float -- ^ Accumulator of relevant facts
    -> Map Hypothesis Float -- ^ Final relevant facts
relevantClauses p ftab rs cs a =
    let ms = Map.map (\fs -> featureClauseMark rs fs ftab) cs
        rels = Map.filter (p <=) ms
        relKeys = Map.keysSet rels
        cs' = Map.difference cs rels
        p' = p + (1 - p) / convergence
        a' = a `Map.union` rels
        rs' = Set.unions (Map.elems (Map.restrictKeys cs relKeys)) `Set.union` rs
    in
        if Map.null rels
            then a
            else relevantClauses p' ftab rs' cs' a'


relevantClausesNaive
    :: Float -- ^ Pass mark
    -> Set Symbol -- ^ Relevant symbols
    -> Set Hypothesis -- ^ working irrelevant facts
    -> Map Hypothesis Float -- ^ Accumulator of relevant facts
    -> Map Hypothesis Float -- ^ Final relevant facts
relevantClausesNaive p rs = relevantClausesNaiveFeatures p (Set.map FeatureSymbol rs)


relevantClausesNaiveFeatures
    :: Float -- ^ Pass mark
    -> Set Feature -- ^ Relevant features
    -> Set Hypothesis -- ^ working irrelevant facts
    -> Map Hypothesis Float -- ^ Accumulator of relevant facts
    -> Map Hypothesis Float -- ^ Final relevant facts
relevantClausesNaiveFeatures p rs cs a =
    let ms = Map.fromSet (clauseMarkNaiveFeatures rs) cs
        rels = Map.filter (p <=) ms
        cs' = Map.keysSet (Map.difference ms rels)
        p' = p + (1 - p) / convergence
        a' = a `Map.union` rels
        rs' = Set.unions (Set.map (features . hypothesisFormula) (Map.keysSet rels)) `Set.union` rs
    in
        if Map.null rels
            then a
            else relevantClausesNaiveFeatures p' rs' cs' a'


clauseMarkNaive
    :: Set Symbol
    -> Hypothesis
    -> Float
clauseMarkNaive rs = clauseMarkNaiveFeatures (Set.map FeatureSymbol rs)


clauseMarkNaiveFeatures
    :: Set Feature
    -> Hypothesis
    -> Float
clauseMarkNaiveFeatures rs c =
    let cs = features (hypothesisFormula c)
        r = cs `Set.intersection` rs
        ir = cs `Set.difference` r
    in ratio (int2Float (Set.size r)) (int2Float (Set.size r + Set.size ir))


clauseMark :: Set Symbol -> ExprOf a -> Map Symbol Int -> Float
clauseMark rs c ftab =
    featureClauseMark
        (Set.map FeatureSymbol rs)
        (features c)
        (Map.mapKeys FeatureSymbol ftab)


featureClauseMark :: Set Feature -> Set Feature -> Map Feature Int -> Float
featureClauseMark rs cs ftab =
    let
        r = cs `Set.intersection` rs
        ir = cs `Set.difference` r
        m = sum (Set.map (ftab `featureWeight`) r)
    in ratio m (m + int2Float (Set.size ir))


funWeight :: Map Symbol Int -> Symbol -> Float
funWeight ftab f = weightFromFrequency (Map.lookup f ftab ?? 0)


featureWeight :: Map Feature Int -> Feature -> Float
featureWeight ftab f = weightFromFrequency (Map.lookup f ftab ?? 0)


weightFromFrequency :: Int -> Float
weightFromFrequency n | n <= 0 = 0
weightFromFrequency n = 1 + 2 / log (int2Float n + 1)


ratio :: Float -> Float -> Float
ratio _ 0 = 0
ratio n d = n / d


features :: ExprOf a -> Set Feature
features = \case
    TermVar{} -> Set.empty
    TermSymbol _pos sym es -> Set.insert (FeatureSymbol sym) (Set.unions (fmap features es))
    TermSymbolStruct sym e -> Set.insert (FeatureStruct sym) (foldMap features e)
    Apply e es -> features e `Set.union` Set.unions (fmap features (toList es))
    TermSep _ e scope -> features e `Set.union` features (fromScope scope)
    ReplacePred _ _ e scope -> features e `Set.union` features (fromScope scope)
    ReplaceFun es scope cond -> (Set.unions (fmap (features . snd) es)) `Set.union` features (fromScope scope) `Set.union` features (fromScope cond)
    Connected _ e1 e2 -> features e1 `Set.union` features e2
    Lambda scope -> features (fromScope scope)
    Quantified _ scope -> features (fromScope scope)
    PropositionalConstant{} -> Set.empty
    Not _pos e -> features e


symbols :: ExprOf a -> Set Symbol
symbols = Set.fromList . mapMaybe featureSymbol . Set.toList . features


featureSymbol :: Feature -> Maybe Symbol
featureSymbol = \case
    FeatureSymbol sym -> Just sym
    FeatureStruct{} -> Nothing


featureTable :: ExprOf a -> Map Feature Int
featureTable = \case
    TermVar{} -> Map.empty
    TermSymbol _pos sym es -> insert (FeatureSymbol sym) 1 (unions (fmap featureTable es))
    TermSymbolStruct sym e -> insert (FeatureStruct sym) 1 (foldMap featureTable e)
    Apply e es -> featureTable e `union` unions (fmap featureTable (toList es))
    TermSep _ e scope -> featureTable e `union` featureTable (fromScope scope)
    ReplacePred _ _ e scope -> featureTable e `union` featureTable (fromScope scope)
    ReplaceFun es scope cond -> (unions (fmap (featureTable . snd) (toList es))) `union` featureTable (fromScope scope) `union` featureTable (fromScope cond)
    Connected _ e1 e2 -> featureTable e1 `union` featureTable e2
    Lambda scope -> featureTable (fromScope scope)
    Quantified _ scope -> featureTable (fromScope scope)
    PropositionalConstant{} -> Map.empty
    Not _pos e -> featureTable e
    where
        union = Map.unionWith (+)
        unions = Map.unionsWith (+)
        insert = Map.insertWith (+)

symbolTable :: ExprOf a -> Map Symbol Int
symbolTable = Map.fromListWith (+) . mapMaybe go . Map.toList . featureTable
    where
        go (FeatureSymbol sym, n) = Just (sym, n)
        go (FeatureStruct{}, _) = Nothing
