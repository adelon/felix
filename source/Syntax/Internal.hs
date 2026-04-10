{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Data types for the internal (semantic) syntax tree.
module Syntax.Internal
    ( module Syntax.Internal
    , module Syntax.Abstract
    , module Syntax.LexicalPhrase
    , module Syntax.Token
    ) where


import Base
import Syntax.Lexicon (pattern PairSymbol, pattern ConsSymbol)
import Syntax.LexicalPhrase (unsafeReadPhrase, unsafeReadPhraseSgPl)
import Syntax.Token (Token(..))
import Report.Location

import Syntax.Abstract
    ( Chain(..)
    , Associativity(..)
    , Connective(..)
    , VarSymbol(..)
    , pattern NamedVar
    , pattern FreshVar
    , FunctionSymbol
    , SymbolPattern(..)
    , MixfixItem(..)
    , Pattern(..)
    , LexicalItem
    , LexicalItemSgPl
    , RelationSymbol(..)
    , StructSymbol (..)
    , Relation
    , PropositionalConstant(..)
    , StructPhrase
    , Justification(..)
    , Marker(..)
    , markerFromToken
    , lexicalItemMarker
    , lexicalItemSgPlMarker
    , mkLexicalItem
    , mkLexicalItemSgPl
    , relationSymbolMarker
    , relationSymbolToken
    , mixfixMarker
    , mkMixfixItem
    , pattern CarrierSymbol, pattern ConsSymbol, pattern ElementSymbol
    , pattern NotElementSymbol, pattern EqSymbol, pattern NeqSymbol, pattern SubseteqSymbol
    )

import Bound
import Bound.Scope
import Data.Deriving (deriveShow1, deriveEq1, deriveOrd1)
import Data.Hashable.Lifted
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import TextBuilder (TextBuilder)

-- | 'Symbol's can be used as function and relation symbols.
data Symbol
    = SymbolMixfix FunctionSymbol
    | SymbolFun LexicalItemSgPl
    | SymbolInteger Int
    | SymbolPredicate Predicate
    deriving (Show, Eq, Ord, Generic, Hashable)


data Predicate
    = PredicateAdj LexicalItem
    | PredicateVerb LexicalItemSgPl
    | PredicateNoun LexicalItemSgPl -- ^ /@\<...\> is a \<...\>@/.
    | PredicateRelation RelationSymbol
    | PredicateSymbol Text
    | PredicateNounStruct LexicalItemSgPl -- ^ /@\<...\> is a \<...\>@/.
    deriving (Show, Eq, Ord, Generic, Hashable)


data Quantifier
    = Universally
    | Existentially
    deriving (Show, Eq, Ord, Generic, Hashable)

type Formula = Term
type Term = Expr
type Expr = ExprOf VarSymbol


-- | Internal higher-order expressions.
data ExprOf a
    = TermVar a
    -- ^ Fresh constants disjoint from all user-named identifiers.
    -- These can be used to eliminate higher-order constructs.
    --
    | TermSymbol Location Symbol [ExprOf a]
    -- ^ Application of a symbol (including function and predicate symbols).
    | TermSymbolStruct StructSymbol (Maybe (ExprOf a))
    --
    | Apply (ExprOf a) (NonEmpty (ExprOf a))
    -- ^ Higher-order application.
    --
    | TermSep VarSymbol (ExprOf a) (Scope () ExprOf a)
    -- ^ Set comprehension using seperation, e.g.: /@{ x ∈ X | P(x) }@/.
    --
    | ReplacePred VarSymbol VarSymbol (ExprOf a) (Scope ReplacementVar ExprOf a)
    -- ^ Replacement for single-valued predicates. The concrete syntax for these
    -- syntactically requires a bounded existential quantifier in the condition:
    --
    -- /@$\\{ y | \\exists x\\in A. P(x,y) \\}$@/
    --
    -- In definitions the single-valuedness of @P@ becomes a proof obligation.
    -- In other cases we could instead add it as constraint
    --
    -- /@$b\\in \\{ y | \\exists x\\in A. P(x,y) \\}$@/
    -- /@iff@/
    -- /@$\\exists x\\in A. P(x,y)$ and $P$ is single valued@/
    --
    --
    | ReplaceFun (NonEmpty (VarSymbol, ExprOf a)) (Scope VarSymbol ExprOf a) (Scope VarSymbol ExprOf a)
    -- ^ Set comprehension using functional replacement,
    -- e.g.: /@{ f(x, y) | x ∈ X; y ∈ Y; P(x, y) }@/.
    -- The list of pairs gives the domains, the integers in the scope point to list indices.
    -- The first scope is the lhs, the optional scope can be used for additional constraints
    -- on the variables (i.e. implicit separation over the product of the domains).
    -- An out-of-bound index is an error, since otherwise replacement becomes unsound.
    --
    | Connected Connective (ExprOf a) (ExprOf a)
    | Lambda (Scope VarSymbol ExprOf a)
    | Quantified Quantifier (Scope VarSymbol ExprOf a)
    | PropositionalConstant PropositionalConstant
    | Not Location (ExprOf a)
    deriving (Functor, Foldable, Traversable)

data ReplacementVar = ReplacementDomVar | ReplacementRangeVar deriving (Show, Eq, Ord, Generic, Hashable)

makeBound ''ExprOf

deriveShow1 ''ExprOf
deriveEq1   ''ExprOf
deriveOrd1  ''ExprOf

deriving instance Show a => Show (ExprOf a)
deriving instance Eq   a => Eq   (ExprOf a)
deriving instance Ord  a => Ord  (ExprOf a)

deriving instance Generic (ExprOf a)
deriving instance Generic1 ExprOf

deriving instance Hashable1 ExprOf

deriving instance Hashable a =>  Hashable (ExprOf a)

abstractVarSymbol :: VarSymbol -> ExprOf VarSymbol -> Scope VarSymbol ExprOf VarSymbol
abstractVarSymbol x = abstract (\y -> if x == y then Just x else Nothing)

abstractVarSymbols :: Foldable t => t VarSymbol -> ExprOf VarSymbol -> Scope VarSymbol ExprOf VarSymbol
abstractVarSymbols xs = abstract (\y -> if y `elem` xs then Just y else Nothing)


forgetLocation :: forall a. ExprOf a -> ExprOf a
forgetLocation = \case
      TermVar a ->
        TermVar a

      TermSymbol _loc symb args ->
        TermSymbol Nowhere symb (map forgetLocation args)

      TermSymbolStruct ss me ->
        TermSymbolStruct ss (forgetLocation <$> me)

      Apply f args ->
        Apply (forgetLocation f) (forgetLocation <$> args)

      TermSep v dom sc ->
        TermSep v (forgetLocation dom) (hoistScope forgetLocation sc)

      ReplacePred v1 v2 dom sc ->
        ReplacePred v1 v2 (forgetLocation dom) (hoistScope forgetLocation sc)

      ReplaceFun doms lhs rhs ->
        ReplaceFun
          (fmap (fmap forgetLocation) doms)
          (hoistScope forgetLocation lhs)
          (hoistScope forgetLocation rhs)

      Connected c e1 e2 ->
        Connected c (forgetLocation e1) (forgetLocation e2)

      Lambda sc ->
        Lambda (hoistScope forgetLocation sc)

      Quantified q sc ->
        Quantified q (hoistScope forgetLocation sc)

      PropositionalConstant pc ->
        PropositionalConstant pc

      Not _loc e ->
        Not Nowhere (forgetLocation e)


equivalent :: Eq a => ExprOf a -> ExprOf a -> Bool
equivalent e1 e2 = forgetLocation e1 == forgetLocation e2

-- | Use the given set of in scope structures to cast them to their carriers
-- when occurring on the rhs of the element relation.
-- Use the given 'Map' to annotate (unannotated) structure operations
-- with the most recent inscope appropriate label.
annotateWith :: Set VarSymbol -> HashMap StructSymbol VarSymbol -> Formula -> Formula
annotateWith = go
    where
        go :: (Ord a) => Set a -> HashMap StructSymbol a -> ExprOf a -> ExprOf a
        go labels ops = \case
            TermSymbolStruct symb Nothing ->
                -- TODO error if symbol is not instantiated, but only in theorems?
                TermSymbolStruct symb (TermVar <$> HM.lookup symb ops)
            TermSymbolStruct symb (Just e) ->
                TermSymbolStruct symb (Just (go labels ops e))
            IsElementOf loc1 a (TermVar x) | x `Set.member` labels ->
                IsElementOf loc1 (go labels ops a) (TermSymbolStruct CarrierSymbol (Just (TermVar x)))
            Not loc a ->
                Not loc (go labels ops a)
            Connected conn a b ->
                Connected conn (go labels ops a) (go labels ops b)
            Quantified quant body ->
                Quantified quant (toScope (go (Set.map F labels) (F <$> ops) (fromScope body)))
            e@TermVar{} -> e
            TermSymbol loc symb args ->
                TermSymbol loc symb (go labels ops <$> args)
            Apply e1 args ->
                Apply (go labels ops e1) (go labels ops <$> args)
            TermSep vs e scope ->
                TermSep vs (go labels ops e) (toScope (go (Set.map F labels) (F <$> ops) (fromScope scope)))
            ReplacePred y x xB scope ->
                ReplacePred y x (go labels ops xB) (toScope (go (Set.map F labels) (F <$> ops) (fromScope scope)))
            ReplaceFun bounds ap cond ->
                ReplaceFun
                    (fmap (\(x, e) -> (x, go labels ops e)) bounds)
                    (toScope (go (Set.map F labels) (F <$> ops) (fromScope ap)))
                    (toScope (go (Set.map F labels) (F <$> ops) (fromScope cond)))
            Lambda body ->
                Lambda (toScope (go (Set.map F labels) (F <$> ops) (fromScope body)))
            e@PropositionalConstant{} -> e

containsHigherOrderConstructs :: ExprOf a -> Bool
containsHigherOrderConstructs = \case
    TermSep {} -> True
    ReplacePred{}-> True
    ReplaceFun{}-> True
    Lambda{} -> True
    Apply{} -> False -- FIXME: this is a lie in general; we need to add sortchecking to determine this.
    TermVar{} -> False
    PropositionalConstant{} -> False
    TermSymbol _loc _s es -> any containsHigherOrderConstructs es
    Not _loc e -> containsHigherOrderConstructs e
    Connected _ e1 e2 -> containsHigherOrderConstructs e1 || containsHigherOrderConstructs e2
    Quantified _ scope -> containsHigherOrderConstructs (fromScope scope)
    TermSymbolStruct _ _ -> False

pattern TermOp :: Location -> FunctionSymbol -> [ExprOf a] -> ExprOf a
pattern TermOp loc op es = TermSymbol loc (SymbolMixfix op) es

pattern TermConst :: Location -> Token -> ExprOf a
pattern TermConst loc c <- TermOp loc (MixfixItem (TokenCons c End) _ NonAssoc) []
    where
        TermConst loc c =
            TermOp loc (MixfixItem (TokenCons c End) (markerFromToken c) NonAssoc) []

pattern TermPair :: Location -> ExprOf a -> ExprOf a -> ExprOf a
pattern TermPair loc e1 e2 = TermOp loc PairSymbol [e1, e2]

pattern Atomic :: Location -> Predicate -> [ExprOf a] -> ExprOf a
pattern Atomic loc symbol args = TermSymbol loc (SymbolPredicate symbol) args


pattern FormulaAdj :: Location -> ExprOf a -> LexicalItem -> [ExprOf a] -> ExprOf a
pattern FormulaAdj loc e adj es = Atomic loc (PredicateAdj adj) (e:es)

pattern FormulaVerb :: Location -> ExprOf a -> LexicalItemSgPl -> [ExprOf a] -> ExprOf a
pattern FormulaVerb loc e verb es = Atomic loc (PredicateVerb verb) (e:es)

pattern FormulaNoun :: Location -> ExprOf a -> LexicalItemSgPl -> [ExprOf a] -> ExprOf a
pattern FormulaNoun loc e noun es = Atomic loc (PredicateNoun noun) (e:es)

relationNoun :: Location -> Expr -> Formula
relationNoun loc arg = FormulaNoun loc arg (mkLexicalItemSgPl (unsafeReadPhraseSgPl "relation[/s]") "relation") []

rightUniqueAdj :: Location -> Expr -> Formula
rightUniqueAdj loc arg = FormulaAdj loc arg (mkLexicalItem (unsafeReadPhrase "right-unique") "rightunique") []

-- | Untyped quantification.
pattern Forall, Exists :: Scope VarSymbol ExprOf a -> ExprOf a
pattern Forall scope = Quantified Universally  scope
pattern Exists scope = Quantified Existentially scope

makeForall, makeExists :: Foldable t => t VarSymbol -> Formula -> Formula
makeForall xs e = Quantified Universally (abstractVarSymbols xs e)
makeExists xs e = Quantified Existentially (abstractVarSymbols xs e)

instantiateSome :: Bool -> NonEmpty VarSymbol -> Scope VarSymbol ExprOf VarSymbol -> ExprOf VarSymbol
instantiateSome sameLength =
  if sameLength
  then instantiateSomeHelp id id id
  else instantiateSomeHelp (Forall . toScope) F B

instantiateSomeHelp :: (ExprOf a -> ExprOf VarSymbol) -> (VarSymbol -> a) -> (VarSymbol -> a) -> NonEmpty VarSymbol -> Scope VarSymbol ExprOf VarSymbol -> ExprOf VarSymbol
instantiateSomeHelp complete f b xs scope = complete $ instantiateEither inst scope
    where
        inst (Left x) | x `elem` xs = TermVar (f x)
        inst (Left y) = TermVar (b y)
        inst (Right fv) = TermVar (f fv)

-- | Bind all free variables not occuring in the given set universally
forallClosure :: Set VarSymbol -> Formula -> Formula
forallClosure xs phi = if isClosed phi
    then phi
    else Quantified Universally (abstract isNamedVar phi)
        where
            isNamedVar :: VarSymbol -> Maybe VarSymbol
            isNamedVar x = if x `Set.member` xs then Nothing else Just x

freeVars :: ExprOf VarSymbol -> Set VarSymbol
freeVars = Set.fromList . toList

pattern And :: ExprOf a -> ExprOf a -> ExprOf a
pattern And e1 e2 = Connected Conjunction e1 e2

pattern Or :: ExprOf a -> ExprOf a -> ExprOf a
pattern Or e1 e2 = Connected Disjunction e1 e2

pattern Implies :: ExprOf a -> ExprOf a -> ExprOf a
pattern Implies e1 e2 = Connected Implication e1 e2

pattern Iff :: ExprOf a -> ExprOf a -> ExprOf a
pattern Iff e1 e2 = Connected Equivalence e1 e2

pattern Xor :: ExprOf a -> ExprOf a -> ExprOf a
pattern Xor e1 e2 = Connected ExclusiveOr e1 e2


pattern Bottom :: ExprOf a
pattern Bottom = PropositionalConstant IsBottom

pattern Top :: ExprOf a
pattern Top = PropositionalConstant IsTop


pattern Relation :: Location -> RelationSymbol -> [ExprOf a] -> ExprOf a
pattern Relation loc rel es = Atomic loc (PredicateRelation rel) es

-- | Membership.
pattern IsElementOf :: Location -> ExprOf a -> ExprOf a -> ExprOf a
pattern IsElementOf loc e1 e2 = Relation loc ElementSymbol (e1 : [e2])

isElementOf :: ExprOf a -> ExprOf a -> ExprOf a
isElementOf e1 e2 = Relation Nowhere ElementSymbol (e1 : [e2])

-- | Membership.
isNotElementOf :: Location -> ExprOf a -> ExprOf a -> ExprOf a
isNotElementOf loc e1 e2 = Not loc (IsElementOf loc e1 e2)

-- | Subset relation (non-strict).
pattern IsSubsetOf :: Location -> ExprOf a -> ExprOf a -> ExprOf a
pattern IsSubsetOf loc e1 e2 = Atomic loc (PredicateRelation SubseteqSymbol) (e1 : [e2])

ordinalNoun :: LexicalItemSgPl
ordinalNoun = mkLexicalItemSgPl (unsafeReadPhraseSgPl "ordinal[/s]") "ordinal"

isOrdinalNoun :: LexicalItemSgPl -> Bool
isOrdinalNoun noun = noun == ordinalNoun

-- | Ordinal predicate.
pattern IsOrd :: Location -> ExprOf a -> ExprOf a
pattern IsOrd loc e1 <- Atomic loc (PredicateNoun (isOrdinalNoun -> True)) [e1]
    where
        IsOrd loc e1 = Atomic loc (PredicateNoun ordinalNoun) [e1]

-- | Equality.
pattern Equals :: Location -> ExprOf a -> ExprOf a -> ExprOf a
pattern Equals loc e1 e2 = Atomic loc (PredicateRelation EqSymbol) (e1 : [e2])

equals :: ExprOf a -> ExprOf a -> ExprOf a
equals e1 e2 = Atomic Nowhere (PredicateRelation EqSymbol) (e1 : [e2])

-- | Disequality.
pattern NotEquals :: Location -> ExprOf a -> ExprOf a -> ExprOf a
pattern NotEquals loc e1 e2 = Atomic loc (PredicateRelation NeqSymbol) (e1 : [e2])

pattern EmptySet :: Location -> ExprOf a
pattern EmptySet loc =
    TermSymbol loc
        (SymbolMixfix (MixfixItem (TokenCons (Command "emptyset") End) "emptyset" NonAssoc))
        []

makeConjunction :: [ExprOf a] -> ExprOf a
makeConjunction = \case
    [] -> Top
    es -> List.foldl1' And es

makeDisjunction :: [ExprOf a] -> ExprOf a
makeDisjunction = \case
    [] -> Bottom
    es -> List.foldl1' Or es

makeIff :: [ExprOf a] -> ExprOf a
makeIff = \case
    [] -> Bottom
    es -> List.foldl1' Iff es

makeXor :: [ExprOf a] -> ExprOf a
makeXor = \case
    [] -> Bottom
    es -> List.foldl1' Xor es

finiteSet :: NonEmpty (ExprOf a) -> ExprOf a
finiteSet = foldr cons (EmptySet Nowhere)
    where
        cons x y = TermSymbol Nowhere (SymbolMixfix ConsSymbol) [x, y]

isPositive :: ExprOf a -> Bool
isPositive = \case
    Not _ _ -> False
    _ -> True

dual :: ExprOf a -> ExprOf a
dual = \case
    Not _loc f -> f
    f -> Not Nowhere f



-- | Local assumptions.
data Asm
    = Asm Formula
    | AsmStruct VarSymbol StructPhrase


deriving instance Show Asm
deriving instance Eq   Asm
deriving instance Ord  Asm

data StructAsm
    = StructAsm VarSymbol StructPhrase



data Axiom = Axiom [Asm] Formula

deriving instance Show Axiom
deriving instance Eq   Axiom
deriving instance Ord  Axiom


data Lemma = Lemma [Asm] Formula

deriving instance Show Lemma
deriving instance Eq   Lemma
deriving instance Ord  Lemma


data Defn
    = DefnPredicate [Asm] Predicate (NonEmpty VarSymbol) Formula
    | DefnFun [Asm] LexicalItemSgPl [VarSymbol] Term
    | DefnOp FunctionSymbol [VarSymbol] Term

deriving instance Show Defn
deriving instance Eq   Defn
deriving instance Ord  Defn

data Inductive = Inductive
    { inductiveSymbol :: FunctionSymbol
    , inductiveParams :: [VarSymbol]
    , inductiveDomain :: Expr
    , inductiveIntros :: NonEmpty IntroRule
    }
    deriving (Show, Eq, Ord)

data IntroRule = IntroRule
    { introConditions :: [Formula] -- The inductively defined set may only appear as an argument of monotone operations on the rhs.
    , introResult :: Formula -- TODO Refine.
    }
    deriving (Show, Eq, Ord)

data CalcQuantifier
    = CalcForall (NonEmpty VarSymbol) (Maybe Formula)
    | CalcUnquantified
    deriving (Show, Eq, Ord)

data Proof
    = Omitted
    -- ^ Ends a proof without further verification.
    -- This results in a “gap” in the formalization.
    | Qed {mloc :: Maybe Location, by :: Justification}
    -- ^ Ends of a proof, leaving automation to discharge the current goal using the given justification.
    | ByContradiction Location Proof
    -- ^ Take the dual of the current goal as an assumption and
    -- set the goal to absurdity.
    | BySetInduction Location (Maybe Term) Proof
    -- ^ ∈-induction.
    | ByOrdInduction Location Proof
    -- ^ Transfinite induction for ordinals.
    | Assume Location Formula Proof
    -- ^ Simplify goals that are implications or disjunctions.
    | Fix Location (NonEmpty VarSymbol) Formula Proof
    -- ^ Simplify universal goals (with an optional bound or such that statement)
    | Take Location (NonEmpty VarSymbol) Formula Justification Proof
    -- ^ Use existential assumptions.
    | Suffices Location Formula Justification Proof
    | ByCase Location [Case]
    -- ^ Proof by case. Disjunction of the case hypotheses 'Case'
    -- must hold for this step to succeed. Each case starts a subproof,
    -- keeping the same goal but adding the case hypothesis as an assumption.
    -- Often this will be a classical split between /@P@/ and /@not P@/, in
    -- which case the proof that /@P or not P@/ holds is easy.
    --
    | Have Location Formula Justification Proof
    -- ^ An affirmation, e.g.: /@We have \<stmt\> by \<ref\>@/.
    --
    | Calc Location CalcQuantifier Calc Proof
    | Subclaim Location Formula Proof Proof
    -- ^ A claim is a sublemma with its own proof:
    --
    -- /@Show \<goal stmt\>. \<steps\>. \<continue other proof\>.@/
    --
    -- A successful first proof adds the claimed formula as an assumption
    -- for the remaining proof.
    --
    | Define Location VarSymbol Term Proof
    | DefineFunction Location VarSymbol VarSymbol Term Term Proof

    | DefineFunctionLocal Location VarSymbol VarSymbol VarSymbol Term (NonEmpty (Term, Formula)) Proof

deriving instance Show Proof
deriving instance Eq   Proof
deriving instance Ord  Proof



-- | A case of a case split.
data Case = Case
    { caseOf :: Formula
    , caseProof :: Proof
    }

deriving instance Show Case
deriving instance Eq   Case
deriving instance Ord  Case

-- | See 'Syntax.Abstract.Calc'.
data Calc
    = Equation Term (NonEmpty (Term, Justification))
    | Biconditionals Term (NonEmpty (Term, Justification))

deriving instance Show Calc
deriving instance Eq   Calc
deriving instance Ord  Calc

calcQuant :: CalcQuantifier -> (Formula -> Formula)
calcQuant = \case
    CalcUnquantified -> id
    CalcForall xs maySuchThat -> case maySuchThat of
        Nothing -> makeForall xs
        Just suchThat -> \phi -> makeForall xs (suchThat `Implies` phi)

calcResult :: CalcQuantifier -> Calc -> ExprOf VarSymbol
calcResult quant = \case
    Equation e eqns -> calcQuant quant (Equals Nowhere e (fst (NonEmpty.last eqns)))
    Biconditionals phi phis -> calcQuant quant (phi `Iff` fst (NonEmpty.last phis))

calculation :: CalcQuantifier -> Calc -> [(ExprOf VarSymbol, Justification)]
calculation quant = \case
    Equation e1 eqns@((e2, jst) :| _) -> (calcQuant quant (Equals Nowhere e1 e2), jst) : collectEquations quant (toList eqns)
    Biconditionals p1 ps@((p2, jst) :| _) -> (calcQuant quant (p1 `Iff` p2), jst) : collectBiconditionals quant (toList ps)


collectEquations :: CalcQuantifier -> [(Formula, j)] -> [(Formula, j)]
collectEquations quant = \case
    (e1, _) : eqns'@((e2, jst) : _) -> (calcQuant quant (Equals Nowhere e1 e2), jst) : collectEquations quant eqns'
    _ -> []

collectBiconditionals :: CalcQuantifier -> [(Formula, j)] -> [(Formula, j)]
collectBiconditionals quant = \case
    (p1, _) : ps@((p2, jst) : _) -> (calcQuant quant (p1 `Iff` p2), jst) : collectBiconditionals quant ps
    _ -> []


data Datatype
    = Datatype
        { datatypeHead :: SymbolPattern
        , datatypeClauses :: NonEmpty DatatypeClause
        }
    deriving (Show, Eq, Ord)

data DatatypeClause = DatatypeClause
    { datatypeClauseConstructor :: SymbolPattern
    , datatypeClausePremises :: [(VarSymbol, Expr)]
    }
    deriving (Show, Eq, Ord)


data Signature
    = SignaturePredicate Predicate (NonEmpty VarSymbol)
    | SignatureFormula Formula -- TODO: Reconsider, this is pretty lossy.

deriving instance Show Signature
deriving instance Eq   Signature
deriving instance Ord  Signature

data StructDefn = StructDefn
    { structPhrase :: StructPhrase
    -- ^ The noun phrase naming the structure, e.g.: @partial order@ or @abelian group@.
    , structParents :: Set StructPhrase
    , structDefnLabel :: VarSymbol
    , structDefnFixes :: Set StructSymbol
    -- ^ List of commands representing operations,
    -- e.g.: @\\contained@ or @\\inv@. These are used as default operation names
    -- in instantiations such as @Let $G$ be a group@.
    -- The commands should be set up to handle an optional struct label
    -- which would typically be rendered as a sub- or superscript, e.g.:
    -- @\\contained[A]@ could render as ”⊑ᴬ“.
    --    --
    , structDefnAssumes :: [(Marker, Formula)]
    -- ^ The assumption or axioms of the structure.
    -- To be instantiate with the @structFixes@ of a given structure.
    }

deriving instance Show StructDefn
deriving instance Eq   StructDefn
deriving instance Ord  StructDefn


data Abbreviation
    = Abbreviation Symbol (Scope Int ExprOf Void)
    deriving (Show, Eq, Ord)

data Block
    = BlockAxiom Location Marker Axiom
    | BlockLemma Location Marker Lemma
    | BlockProof Location Location Proof
    | BlockDefn Location Marker Defn
    | BlockAbbr Location Marker Abbreviation
    | BlockStruct Location Marker StructDefn
    | BlockInductive Location Marker Inductive
    | BlockSig Location Marker [Asm] Signature
    | BlockData Location Marker Datatype
    deriving (Show, Eq, Ord)


data Task = Task
    { taskDirectness :: Directness
    , taskHypotheses :: [Hypothesis] -- ^ No guarantees on order.
    , taskConjectureLabel :: Marker
    , taskLocation :: Location
    , taskConjecture :: Formula
    } deriving (Show, Eq, Generic, Hashable)

data Hypothesis = Hypothesis
    { hypothesisMarker :: Marker
    , hypothesisFormula :: Formula
    , hypothesisEncoded :: TextBuilder
    , hypothesisLine :: TextBuilder
    }

instance Show Hypothesis where
    show (Hypothesis marker formula _ _) =
        "Hypothesis " <> show marker <> " " <> show formula

instance Eq Hypothesis where
    Hypothesis m f _ _ == Hypothesis m' f' _ _ = (m, f) == (m', f')

instance Ord Hypothesis where
    compare (Hypothesis m f _ _) (Hypothesis m' f' _ _) =
        compare (m, f) (m', f')

instance Hashable Hypothesis where
    hashWithSalt s (Hypothesis m f _ _) = hashWithSalt s (m, f)


-- | Indicates whether a given proof is direct or indirect.
-- An indirect proof (i.e. a proof by contradiction) may
-- cause an ATP to emit a warning about contradictory axioms.
-- When we know that the proof is indirect, we want to ignore
-- this warning. For relevance filtering we also want to know
-- what our actual goal is, so we keep the original conjecture.
data Directness
    = Indirect Formula -- ^ The former conjecture.
    | Direct
    deriving (Show, Eq, Generic, Hashable)

isIndirect :: Task -> Bool
isIndirect task = case taskDirectness task of
    Indirect _ -> True
    Direct -> False


-- | Full boolean contraction.
contraction :: ExprOf a -> ExprOf a
contraction = \case
    Connected conn f1 f2  -> atomicContraction (Connected conn (contraction f1) (contraction f2))
    Quantified quant scope -> atomicContraction (Quantified quant (hoistScope contraction scope))
    Not loc f -> Not loc (contraction f)
    f -> f


-- | Atomic boolean contraction.
atomicContraction :: ExprOf a -> ExprOf a
atomicContraction = \case
    Top    `Iff` f      -> f
    Bottom `Iff` f      -> Not Nowhere f
    f      `Iff` Top    -> f
    f      `Iff` Bottom -> Not Nowhere f

    Top    `Implies` f      -> f
    Bottom `Implies` _      -> Top
    _      `Implies` Top    -> Top
    f      `Implies` Bottom -> Not Nowhere f

    Top    `And` f      -> f
    Bottom `And` _      -> Bottom
    f      `And` Top    -> f
    _      `And` Bottom -> Bottom

    phi@(Quantified _quant scope) -> case unscope scope of
        Top -> Top
        Bottom -> Bottom
        _ -> phi

    Not _ Top    -> Bottom
    Not _ Bottom -> Top

    f -> f
