{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}


module Meaning where


import Base
import Serial
import Syntax.Abstract (Sign(..))
import Syntax.Abstract qualified as Raw
import Syntax.Internal (VarSymbol(..), pattern FreshVar)
import Syntax.Internal qualified as Sem
import Syntax.LexicalPhrase (unsafeReadPhrase)
import Syntax.Lexicon
import Report.Location

import Bound
import Bound.Scope (abstractEither)
import Control.Monad.Except
import Control.Monad.State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Set qualified as Set
import Control.Exception (Exception)


-- | The 'Gloss' monad. Basic elaboration, desugaring, and validation
-- computations take place in this monad, using 'ExceptT' to log
-- validation errors and 'State' to keep track of the surrounding context.
type Gloss = ExceptT GlossError (State GlossState)
-- This monad previously used 'ValidationT' for validation so that multiple
-- validation errors could be reported. Using only 'ExceptT' we fail immediately
-- on the first error. If we ever swich back to 'ValidateT' for error reporting,
-- then we should re-enable {-# OPTIONS_GHC -foptimal-applicative-do #-},
-- as 'ValidateT' can report more errors when used with applicative combinators.

-- | Errors that can be detected during glossing.
data GlossError
    = GlossDefnError Location DefnError Sem.Marker
    | GlossInductionError Location
    | GlossRelationExprWithParams Location
    deriving (Eq, Ord)

instance Exception GlossError
instance Show GlossError where show = explainGlossError

explainGlossError :: GlossError -> String
explainGlossError = \case
    GlossDefnError loc defnError marker ->
        "Definition error at " <> prettyLocation loc <> " (in " <> show marker <> "): " <> case defnError of
            DefnWarnLhsFree xs ->
                "The variables " <> show xs <> " in the pattern being defined (definiendum) do not occur in the body of the definition (definiens). Remove them or use them in the body."
            DefnErrorLhsNotLinear ->
                "The left-hand side of the definition is not linear (a variable occurs multiple times)."
            DefnErrorLhsTypeFree ->
                "The defintion contains variables with no typing constraints or assumptions placed on them."
            DefnErrorRhsFree xs ->
                "The variables " <> show xs <> " on the right-hand side of the definition do not occurring on the left-hand side."
    GlossInductionError loc ->
        "Error at " <> prettyLocation loc <> ": Induction over a non-variable is not supported."
    GlossRelationExprWithParams loc ->
        "Error at " <> prettyLocation loc <> ": A relation defined by an expression cannot have parameters."

-- | Specialization of 'traverse' to 'Gloss'.
each :: (Traversable t) => (a -> Gloss b) -> t a -> Gloss (t b)
explain `each` as = traverse explain as
infix 7 `each` -- In particular, 'each' has precedence over '(<$>)'.

-- | Wellformedness check for definitions.
-- The following conditions need to be met.
--
-- * Variables occurring in the lexical phrases on the left side must be linear,
--   i.e. each variable can only occur once.
-- * The arguments of the lexical phrases must be variables, not complex terms.
--   This is statically guaranteed by the grammar.
-- * The optional typing noun may not have any free variables.
-- * The rhs side may not have any free variables not occurring on the lhs.
-- * If a variable on the lhs does not occur on the rhs, a warning should we issued.
--
isWellformedDefn :: Sem.Defn -> Either DefnError Sem.Defn
isWellformedDefn defn =
    if  | ls' /= ls -> Left DefnErrorLhsNotLinear
        | not (null rdiff) -> Left (DefnErrorRhsFree (toList rdiff))
        | not (null ldiff) -> case defn of
            Sem.DefnPredicate{} -> Left (DefnWarnLhsFree (toList ldiff))
            _ -> Right defn
        | otherwise -> Right defn
    where
        ls = lhsVars defn
        ls' = nubOrd ls
        rs = rhsVars defn
        (ldiff, rdiff) = symmetricDifferenceDecompose (Set.fromList ls') rs


lhsVars :: Sem.Defn -> [VarSymbol]
lhsVars = \case
    Sem.DefnPredicate _ _ vs _ -> toList vs
    Sem.DefnFun _ _ vs _ -> vs
    Sem.DefnOp _ vs _ -> vs

rhsVars :: Sem.Defn -> Set VarSymbol
rhsVars = \case
    Sem.DefnPredicate _ _ _ f -> Sem.freeVars f
    Sem.DefnFun _ _ _ e -> Sem.freeVars e
    Sem.DefnOp _ _ e -> Sem.freeVars e


-- | Validation errors for top-level definitions.
data DefnError
    = DefnWarnLhsFree [VarSymbol]
    | DefnErrorLhsNotLinear
    | DefnErrorLhsTypeFree
    | DefnErrorRhsFree [VarSymbol]
    deriving (Show, Eq, Ord)


-- | Context for 'Gloss' computations.
data GlossState = GlossState
    { varCount :: Int
    -- ^ Counter for generating variables names for the output.
    , lemmaCount :: Serial
    -- ^ Counter for generating names for unlabelled lemmas.
    , lexicon :: Lexicon
    , pretypings :: Map VarSymbol Sem.Formula
    -- ^ Keeps track of variable constraints. We lookup and insert constraints
    -- when quantifying. Each variable maps to a predicate that the variables
    -- must (implicitly) satisfy. Multiple constraints are represented as
    -- a conjunction.
    -- CONDITION: For convenience the keys are 'VarSymbol's, but variable constraints
    -- should not be used for 'FreshVar's.
    } deriving (Show, Eq)


freshLemmaId :: Gloss Serial
freshLemmaId = do
    i <- gets lemmaCount
    modify $ \s -> s {lemmaCount = Serial.next (lemmaCount s)}
    pure i

freshVar :: Gloss VarSymbol
freshVar = do
    i <- gets varCount
    modify $ \s -> s {varCount = varCount s + 1}
    pure $ FreshVar i

initialGlossState :: GlossState
initialGlossState = GlossState
    { varCount = 0
    , lemmaCount = Serial.start
    , lexicon = builtins
    , pretypings = mempty
    }

glossStep :: GlossState -> Raw.Block -> Either GlossError (Sem.Block, GlossState)
glossStep glossState block = case runState (runExceptT (glossBlock block)) glossState of
    (Left err, _nextGlossState) -> Left err
    (Right glossedBlock, nextGlossState) -> Right (glossedBlock, nextGlossState)

meaning :: [Raw.Block] -> Either GlossError [Sem.Block]
meaning blocks = evalState (runExceptT (glossBlocks blocks)) initialGlossState

glossExpr :: Raw.Expr -> Gloss (Sem.ExprOf VarSymbol)
glossExpr = \case
    Raw.ExprVar v ->
        pure $ Sem.TermVar v
    Raw.ExprInteger loc n ->
        pure $ Sem.TermSymbol loc (Sem.SymbolInteger n) []
    Raw.ExprOp loc f es ->
        Sem.TermSymbol loc <$> pure (Sem.SymbolMixfix f) <*> (glossExpr `each` es)
    Raw.ExprStructOp _loc tok maybeLabel -> do
        maybeLabel' <- traverse glossExpr maybeLabel
        pure $ Sem.TermSymbolStruct tok maybeLabel'
    Raw.ExprSep _loc x t phi -> do
        t' <- glossExpr t
        phi' <- glossStmt phi
        pure (Sem.TermSep x t' (abstract1 x phi'))
    Raw.ExprReplacePred _loc y x xBound stmt -> do
        xBound' <- glossExpr xBound
        stmt' <- glossStmt stmt
        let toReplacementVar z = if
                | z == x -> Just Sem.ReplacementDomVar
                | z == y -> Just Sem.ReplacementRangeVar
                | otherwise -> Nothing
        let scope = abstract toReplacementVar stmt'
        pure (Sem.ReplacePred y x xBound' scope)
    Raw.ExprReplace _loc e bounds phi -> do
        e' <- glossExpr e
        bounds' <- glossReplaceBound `each` bounds
        let xs = fst <$> bounds'
        phi'' <- case phi of
            Just phi' -> glossStmt phi'
            Nothing -> pure Sem.Top
        let abstractBoundVars = abstract (\x -> List.find (== x) (toList xs))
        pure $ Sem.ReplaceFun bounds' (abstractBoundVars e') (abstractBoundVars phi'')
        where
            glossReplaceBound :: (VarSymbol, Raw.Expr) -> Gloss (VarSymbol, Sem.Term)
            glossReplaceBound (x, b) = (x,) <$> glossExpr b
    Raw.ExprFiniteSet loc es -> do
        es' <- glossExpr `each` es
        pure (foldr cons (Sem.EmptySet loc) es')
        where
            cons x y = Sem.TermSymbol loc (Sem.SymbolMixfix Raw.ConsSymbol) [x, y]


glossFormula :: Raw.Formula -> Gloss (Sem.ExprOf VarSymbol)
glossFormula = \case
    Raw.FormulaChain ch ->
        glossChain ch
    Raw.Connected _loc conn phi psi ->
        glossConnective conn <*> glossFormula phi <*> glossFormula psi
    Raw.FormulaNeg loc f ->
        Sem.Not loc <$> glossFormula f
    Raw.FormulaPredicate loc predi _marker es ->
        Sem.Atomic loc <$> glossPrefixPredicate predi <*> glossExpr `each` toList es
    Raw.PropositionalConstant _loc c ->
        pure $ Sem.PropositionalConstant c
    Raw.FormulaQuantified _loc quantifier xs bound phi -> do
        bound' <- glossBound bound
        phi' <- glossFormula phi
        quantify <- glossQuantifier quantifier
        pure (quantify xs (bound' (toList xs)) phi')

glossChain :: Sem.Chain -> Gloss (Sem.ExprOf VarSymbol)
glossChain ch = Sem.makeConjunction <$> makeRels (conjuncts (splat ch))
    where
        -- | Separate each link of the chain into separate triples.
        splat :: Raw.Chain -> [(NonEmpty Raw.Expr, Sign, Raw.Relation, NonEmpty Raw.Expr)]
        splat = \case
            Raw.ChainBase es sign rel es'
                -> [(es, sign, rel, es')]
            Raw.ChainCons es sign rel ch'@(Raw.ChainBase es' _ _ _)
                -> (es, sign, rel, es') : splat ch'
            Raw.ChainCons es sign rel ch'@(Raw.ChainCons es' _ _ _)
                -> (es, sign, rel, es') : splat ch'

        -- | Take each triple and combine the lhs/rhs to make all the conjuncts.
        conjuncts :: [(NonEmpty Raw.Expr, Sign, Raw.Relation, NonEmpty Raw.Expr)] -> [(Sign, Raw.Relation, Raw.Expr, Raw.Expr)]
        conjuncts triples = do
            (e1s, sign, rel, e2s) <- triples
            e1 <- toList e1s
            e2 <- toList e2s
            pure (sign, rel, e1, e2)

        makeRels :: [(Sign, Raw.Relation, Raw.Expr, Raw.Expr)] -> Gloss [Sem.Formula]
        makeRels triples = for triples makeRel

        makeRel :: (Sign, Raw.Relation, Raw.Expr, Raw.Expr) -> Gloss Sem.Formula
        makeRel (sign, rel, e1, e2) = do
            e1' <- glossExpr e1
            e2' <- glossExpr e2
            case rel of
                Raw.Relation loc rel' params -> do
                    params' <- glossExpr `each` params
                    pure $ sign' loc $ Sem.Relation loc rel' (params' <> [e1',e2'])
                Raw.RelationExpr loc e -> do
                            e' <- glossExpr e
                            pure (sign' loc (Sem.IsElementOf loc (Sem.TermPair loc e1' e2') e'))
            where
                sign' = case sign of
                    Positive -> \_ -> id
                    Negative -> Sem.Not


glossPrefixPredicate :: Raw.PrefixPredicate -> Gloss Sem.Predicate
glossPrefixPredicate (Raw.PrefixPredicate symb _ar) = pure (Sem.PredicateSymbol symb)


glossNPNonEmpty :: Raw.NounPhrase NonEmpty -> Gloss (NonEmpty VarSymbol, Sem.Formula)
glossNPNonEmpty (Raw.NounPhrase leftAdjs noun vars rightAdjs maySuchThat) = do
    -- We interpret the noun as a predicate.
    noun' <- glossNoun noun
    -- Now we turn the noun and all its modifiers into statements.
    let typings = (\v' -> noun' (Sem.TermVar v')) <$> vars
    leftAdjs' <- forEach (toList vars) <$> glossAdjL `each` leftAdjs
    rightAdjs' <- forEach (toList vars) <$> glossAdjR `each` rightAdjs
    suchThat <- maybeToList <$> glossStmt `each` maySuchThat
    let constraints = toList typings <> leftAdjs' <> rightAdjs' <> suchThat
    pure (vars, Sem.makeConjunction constraints)


-- | If needed, we introduce a fresh variable to reduce this to the case @NounPhrase NonEmpty@.
glossNPList :: Raw.NounPhrase [] -> Gloss (NonEmpty VarSymbol, Sem.Formula)
glossNPList (Raw.NounPhrase leftAdjs noun vars rightAdjs maySuchThat) = do
    vars' <- case vars of
        [] -> (:| []) <$> freshVar
        v:vs -> pure (v :| vs)
    glossNPNonEmpty $ Raw.NounPhrase leftAdjs noun vars' rightAdjs maySuchThat

-- Returns a predicate for a term (the constraints) and the optional such-that clause.
-- We treat suchThat separately since multiple terms can share the same such-that clause.
glossNPMaybe ::  Raw.NounPhrase Maybe -> Gloss (Sem.Term -> Sem.Formula, Maybe Sem.Formula)
glossNPMaybe (Raw.NounPhrase leftAdjs noun mayVar rightAdjs maySuchThat) = do
    case mayVar of
        Nothing -> do
            glossNP leftAdjs noun rightAdjs maySuchThat
        Just v' -> do
            -- Next we desugar all the modifiers into statements.
            leftAdjs'    <- apply v' <$> glossAdjL `each` leftAdjs
            rightAdjs'   <- apply v' <$> glossAdjR `each` rightAdjs
            maySuchThat' <- glossStmt `each` maySuchThat
            let constraints = leftAdjs' <> rightAdjs'
            -- Finally we translate the noun itself.
            noun' <- glossNoun noun
            pure case constraints of
                [] -> (\t -> noun' t, maySuchThat')
                _  -> (\t -> noun' t `Sem.And` Sem.makeConjunction (eq t v' : constraints), maySuchThat')
            where
                eq t v = Sem.Equals Nowhere t (Sem.TermVar v)
                apply :: VarSymbol -> [Sem.Term -> Sem.Formula] -> [Sem.Formula]
                apply v stmts = [stmt (Sem.TermVar v) | stmt <- stmts]

-- | Gloss a noun without a variable name.
-- Returns a predicate for a term (the constraints) and the optional such-that clause.
-- We treat suchThat separately since multiple terms can share the same such-that clause.
glossNP :: [Raw.AdjL] -> Raw.Noun -> [Raw.AdjR] -> Maybe Raw.Stmt -> Gloss (Sem.Term -> Sem.ExprOf VarSymbol, Maybe Sem.Formula)
glossNP leftAdjs noun rightAdjs maySuchThat = do
    noun' <- glossNoun noun
    leftAdjs' <- glossAdjL `each` leftAdjs
    rightAdjs' <-  glossAdjR `each` rightAdjs
    maySuchThat' <- glossStmt `each` maySuchThat
    let constraints = [noun'] <> leftAdjs' <> rightAdjs'
    pure (\t -> Sem.makeConjunction (flap constraints t), maySuchThat')


-- | If we have a plural noun with multiple variables, then we need to desugar
-- adjectives to apply to each individual variable.
forEach :: Applicative t => t VarSymbol -> t (Sem.Term -> a) -> t a
forEach vs'' stmts = do
    v <- vs''
    stmt <- stmts
    pure $ stmt (Sem.TermVar v)


glossAdjL :: Raw.AdjL -> Gloss (Sem.Term -> Sem.Formula)
glossAdjL (Raw.AdjL loc pat es) = do
    (es', quantifies) <- unzip <$> glossTerm `each` es
    let quantify = compose $ reverse quantifies
    pure $ \t -> quantify $ Sem.FormulaAdj loc t pat es'


-- | Since we need to be able to remove negation in verb phrases,
-- we need to have 'Sem.Stmt' as the target. We do not yet have
-- the term representing the subject, hence the parameter 'Sem.Expr'.
glossAdjR :: Raw.AdjR -> Gloss (Sem.Term -> Sem.Formula)
glossAdjR = \case
    Raw.AdjR _loc pat [e] | pat == Raw.mkLexicalItem (unsafeReadPhrase "equal to ?") "eq" -> do
        (e', quantify) <- glossTerm e
        pure $ \t -> quantify $ Sem.Equals Nowhere t e'
    Raw.AdjR _loc pat es -> do
        (es', quantifies) <- unzip <$> glossTerm `each` es
        let quantify = compose $ reverse quantifies
        pure $ \t -> quantify $ Sem.FormulaAdj Nowhere t pat es'
    Raw.AttrRThat vp -> glossVP vp


glossAdj :: Raw.AdjOf Raw.Term -> Gloss (Sem.ExprOf VarSymbol -> Sem.Formula)
glossAdj adj = case adj of
    Raw.Adj loc pat [e] | pat == Raw.mkLexicalItem (unsafeReadPhrase "equal to ?") "eq" -> do
        (e', quantify) <- glossTerm e
        pure $ \t -> quantify $ Sem.Equals loc  t e'
    Raw.Adj loc pat es -> do
        (es', quantifies) <- unzip <$> glossTerm `each` es
        let quantify = compose $ reverse quantifies
        pure $ \t -> quantify $ Sem.FormulaAdj loc t pat es'

glossVP :: Raw.VerbPhrase -> Gloss (Sem.Term -> Sem.Formula)
glossVP = \case
    Raw.VPVerb verb -> glossVerb verb
    Raw.VPAdj adjs -> do
        mkAdjs <- glossAdj `each` toList adjs
        pure (\x -> Sem.makeConjunction [mkAdj x | mkAdj <- mkAdjs])
    Raw.VPVerbNot verb -> (Sem.Not Nowhere  .) <$> glossVerb verb
    Raw.VPAdjNot adjs -> (Sem.Not Nowhere .) <$> glossVP (Raw.VPAdj adjs)


glossVerb :: Raw.Verb -> Gloss (Sem.Term -> Sem.Formula)
glossVerb (Raw.Verb loc pat es) = do
    (es', quantifies) <- unzip <$> glossTerm `each` es
    let quantify = compose $ reverse quantifies
    pure $ \ t -> quantify $ Sem.FormulaVerb loc t pat es'


glossNoun :: Raw.Noun -> Gloss (Sem.Term -> Sem.Formula)
glossNoun (Raw.Noun loc pat es) = do
    (es', quantifies) <- unzip <$> glossTerm `each` es
    let quantify = compose $ reverse quantifies
    pure case Raw.sg (Raw.lexicalItemSgPlPhrase pat) of
        -- Everything is a set
        [Just (Sem.Word "set")] -> const Sem.Top
        _ -> \e' -> quantify (Sem.FormulaNoun loc e' pat es')


glossFun :: Raw.Fun -> Gloss (Sem.Term, Sem.Formula -> Sem.Formula)
glossFun (Raw.Fun loc phrase es) = do
    (es', quantifies) <- unzip <$> glossTerm `each` es
    let quantify = compose $ reverse quantifies
    pure (Sem.TermSymbol loc (Sem.SymbolFun phrase) es', quantify)


glossTerm :: Raw.Term -> Gloss (Sem.Term, Sem.Formula -> Sem.Formula)
glossTerm = \case
    Raw.TermExpr e ->
        (, id) <$> glossExpr e
    Raw.TermFun f ->
        glossFun f
    Raw.TermIota _loc _x stmt -> do
        _stmt' <- glossStmt stmt
        _TODO "glossTerm TermIota"
        --pure (Sem.Iota x (abstract1 x stmt'), id)
    Raw.TermQuantified quantifier _loc np -> do
        quantify <- glossQuantifier quantifier
        (mkConstraint, maySuchThat) <- glossNPMaybe np
        v <- freshVar
        let e = Sem.TermVar v
        let constraints = [mkConstraint e]
        pure (e, quantify (v:|[]) (maybeToList maySuchThat <> constraints))



glossStmt :: Raw.Stmt -> Gloss Sem.Formula
glossStmt = \case
    Raw.StmtFormula f -> glossFormula f
    Raw.StmtNeg loc s -> Sem.Not loc <$> glossStmt s
    Raw.StmtVerbPhrase ts vp -> do
        (ts', quantifies) <- NonEmpty.unzip <$> glossTerm `each` ts
        vp' <- glossVP vp
        let phi = Sem.makeConjunction (vp' <$> toList ts')
        pure (compose quantifies phi)
    Raw.StmtNoun ts np -> do
        (ts', quantifies) <- NonEmpty.unzip <$> glossTerm `each` ts
        (np', maySuchThat) <- glossNPMaybe np
        let andSuchThat phi = case maySuchThat of
                Just suchThat -> phi `Sem.And` suchThat
                Nothing -> phi
            psi = Sem.makeConjunction (andSuchThat . np' <$> toList ts')
        pure (compose quantifies psi)
    Raw.StmtStruct t sp -> do
        (t', quantify) <- glossTerm t
        pure (quantify (Sem.TermSymbol (locate t) (Sem.SymbolPredicate (Sem.PredicateNounStruct sp)) [t']))
    Raw.StmtConnected conn _mpos s1 s2 -> glossConnective conn <*> glossStmt s1 <*> glossStmt s2
    Raw.StmtQuantPhrase _loc (Raw.QuantPhrase quantifier np) f -> do
        (vars, constraints) <- glossNPList np
        f' <- glossStmt f
        quantify <- glossQuantifier quantifier
        pure (quantify vars [constraints] f')
    Raw.StmtExists _loc np -> do
        (vars, constraints) <- glossNPList np
        pure (Sem.makeExists vars constraints)
    Raw.SymbolicQuantified _loc quant vs bound suchThat have -> do
        quantify <- glossQuantifier quant
        bound' <- glossBound bound
        suchThatConstraints <- maybeToList <$> glossStmt `each` suchThat
        let boundConstraints = bound' (toList vs)
        have' <- glossStmt have
        pure (quantify vs (boundConstraints <> suchThatConstraints) have')

-- | A bound applies to all listed variables. Note the use of '<**>'.
--
-- >>> ([1, 2, 3] <**> [(+ 10)]) == [11, 12, 13]
--
glossBound :: Raw.Bound -> Gloss ([VarSymbol] -> [Sem.Formula])
glossBound = \case
    Raw.Unbounded -> pure (const [])
    Raw.Bounded loc sign rel term -> do
        term' <- glossExpr term
        let sign' = case sign of
                Positive -> id
                Negative -> Sem.Not loc
        bound <- case rel of
            Raw.Relation loc' rel' params -> do
                params' <- glossExpr `each` params
                pure $ \v -> sign' $
                    Sem.Relation loc' rel' (params' <> [Sem.TermVar v, term'])
            Raw.RelationExpr loc' e -> do
                e' <- glossExpr e
                pure $ \v -> sign' $
                    Sem.IsElementOf loc' (Sem.TermPair loc' (Sem.TermVar v) term') e'
        pure \vs -> vs <**> [bound]


gatherGuards :: Traversable t => t VarSymbol -> Gloss (Maybe (t Sem.Formula))
gatherGuards vs = do
    info <- gets pretypings
    pure $ for vs $ \v -> Map.lookup v info


glossConnective :: Raw.Connective -> Gloss (Sem.Formula -> Sem.Formula -> Sem.Formula)
glossConnective conn = pure (Sem.Connected conn)


glossAsm :: Raw.Asm -> Gloss [Sem.Asm]
glossAsm = \case
    Raw.AsmSuppose s -> do
        s' <- glossStmt s
        pure [Sem.Asm s']
    Raw.AsmLetNoun vs np -> do
        (np', maySuchThat) <- glossNPMaybe np
        let f v = Sem.Asm (np' (Sem.TermVar v) )
        let suchThat = Sem.Asm <$> maybeToList maySuchThat
        pure (suchThat <> fmap f (toList vs))
    Raw.AsmLetIn vs e -> do
        e' <- glossExpr e
        let f v = Sem.Asm (Sem.IsElementOf Nowhere (Sem.TermVar v) e')
        pure $ fmap f (toList vs)
    Raw.AsmLetStruct structLabel structPhrase ->
        pure [Sem.AsmStruct structLabel structPhrase]
    Raw.AsmLetThe _ _ ->
        _TODO "glossAsm AsmLetThe"
    Raw.AsmLetEq x e -> do
        e' <- glossExpr e
        pure (Sem.Asm (Sem.Equals Nowhere (Sem.TermVar x) e') : [])


-- | A quantifier is interpreted as a quantification function that takes a nonempty list of variables,
-- a list of formulas expressing the constraints, and the formula to be quantified as arguments.
-- It then returns the quantification with the correct connective for the constraints.
glossQuantifier
    :: (Foldable t, Applicative f)
    => Raw.Quantifier
    -> f (t VarSymbol
    -> [Sem.ExprOf VarSymbol]
    -> Sem.Formula
    -> Sem.Formula)
glossQuantifier quantifier = pure quantify
    where
        quantify vs constraints f = case (quantifier, constraints) of
            (Raw.Universally, []) ->
                Sem.makeForall vs f
            (Raw.Existentially, []) ->
                Sem.makeExists vs f
            (Raw.Nonexistentially, []) ->
                Sem.Not Nowhere (Sem.makeExists vs f)
            (Raw.Universally, _) ->
                Sem.makeForall vs (Sem.makeConjunction constraints `Sem.Implies` f)
            (Raw.Existentially, _) ->
                Sem.makeExists vs (Sem.makeConjunction constraints `Sem.And` f)
            (Raw.Nonexistentially, _) ->
                Sem.Not Nowhere (Sem.makeExists vs (Sem.makeConjunction constraints `Sem.And` f))


glossAsms :: [Raw.Asm] -> Gloss [Sem.Asm]
glossAsms asms = do
    asms' <- glossAsm `each` asms
    pure $ concat asms'


glossAxiom :: Raw.Axiom -> Gloss Sem.Axiom
glossAxiom (Raw.Axiom asms f) = Sem.Axiom <$> glossAsms asms <*> glossStmt f


glossLemma :: Raw.Claim -> Gloss Sem.Lemma
glossLemma (Raw.Claim asms f) = Sem.Lemma <$> glossAsms asms <*> glossStmt f


glossDefn :: Raw.Defn -> Gloss Sem.Defn
glossDefn = \case
    Raw.Defn asms h f -> glossDefnHead h <*> glossAsms asms <*> glossStmt f
    Raw.DefnFun asms (Raw.Fun _loc fun vs) _ e -> do
        asms' <- glossAsms asms
        e' <- case e of
            -- TODO improve error handling or make grammar stricter
            Raw.TermQuantified _ p _ -> error $ "Quantified term in definition at" <> prettyLocation p <> " : " <> show e
            _ -> fst <$> glossTerm e
        pure $ Sem.DefnFun asms' fun vs e'
    Raw.DefnOp (Raw.SymbolPattern op vs) e ->
        Sem.DefnOp op vs <$> glossExpr e


-- | A definition head is interpreted as a builder of a definition,
-- depending on a previous assumptions and on a rhs.
glossDefnHead :: Raw.DefnHead -> Gloss ([Sem.Asm] -> Sem.Formula -> Sem.Defn)
glossDefnHead = \case
    -- TODO add info from NP.
    Raw.DefnAdj _mnp v (Raw.Adj _loc adj vs) -> do
        pure $ \asms f -> Sem.DefnPredicate asms (Sem.PredicateAdj adj) (v :| vs) f
        --mnp' <- glossNPMaybe `each` mnp
        --pure $ case mnp' of
        --    Nothing  -> \asms f -> Sem.DefnPredicate asms (Sem.PredicateAdj adj') (v :| vs) f
        --    Just np' -> \asms f -> Sem.DefnPredicate asms (Sem.PredicateAdj adj') (v :| vs) (Sem.FormulaAnd (np' v) f)
    Raw.DefnVerb _mnp v (Raw.Verb _loc verb vs) ->
        pure $ \asms f -> Sem.DefnPredicate asms (Sem.PredicateVerb verb) (v :| vs) f
    Raw.DefnNoun v (Raw.Noun _loc noun vs) ->
        pure $ \asms f -> Sem.DefnPredicate asms (Sem.PredicateNoun noun) (v :| vs) f
    Raw.DefnRel v1 rel params v2 ->
        pure \asms f ->
            let args = case params of
                    p : ps -> p :| (ps <> [v1, v2])
                    [] -> v1 :| [v2]
            in Sem.DefnPredicate asms (Sem.PredicateRelation rel) args f
    Raw.DefnSymbolicPredicate (Raw.PrefixPredicate symb _ar) _marker vs ->
        pure $ \asms f -> Sem.DefnPredicate asms (Sem.PredicateSymbol symb) vs f


glossProof :: Raw.Proof -> Gloss Sem.Proof
glossProof = \case
    Raw.Omitted ->
        pure Sem.Omitted
    Raw.Qed loc by ->
        pure (Sem.Qed loc by)
    Raw.ByContradiction loc proof ->
        Sem.ByContradiction loc <$> glossProof proof
    Raw.BySetInduction loc mt proof ->
        Sem.BySetInduction loc <$> mmt' <*> glossProof proof
            where
                mmt' = case mt of
                    Nothing -> pure Nothing
                    Just (Raw.TermExpr (Raw.ExprVar x)) -> pure (Just (Sem.TermVar x))
                    Just _t -> throwError (GlossInductionError loc)
    Raw.ByOrdInduction loc proof ->
        Sem.ByOrdInduction loc <$> glossProof proof
    Raw.ByCase loc cases -> Sem.ByCase loc <$> glossCase `each` cases
    Raw.Have loc _ms s by proof -> case s of
        -- Pragmatics: an existential @Have@ implicitly
        -- introduces the witness and is interpreted as a @Take@ construct.
        Raw.SymbolicExists _loc vs bound suchThat -> do
            bound' <- glossBound bound
            suchThat' <- glossStmt suchThat
            proof' <- glossProof proof
            pure (Sem.Take loc vs (Sem.makeConjunction (suchThat' : bound' (toList vs))) by proof')
        _otherwise ->
            Sem.Have loc <$> glossStmt s <*> pure by <*> glossProof proof
    Raw.Assume loc stmt proof ->
        Sem.Assume loc <$> glossStmt stmt <*> glossProof proof
    Raw.FixSymbolic loc xs bound proof -> do
        bound' <- glossBound bound
        proof' <- glossProof proof
        pure (Sem.Fix loc xs (Sem.makeConjunction (bound' (toList xs))) proof')
    Raw.FixSuchThat loc xs stmt proof -> do
        stmt' <- glossStmt stmt
        proof' <- glossProof proof
        pure (Sem.Fix loc xs stmt' proof')
    Raw.TakeVar loc vs bound suchThat by proof -> do
        bound' <- glossBound bound
        suchThat' <- glossStmt suchThat
        proof' <- glossProof proof
        pure (Sem.Take loc vs (Sem.makeConjunction (suchThat' : bound' (toList vs))) by proof')
    Raw.TakeNoun loc np by proof -> do
        (vs, constraints) <- glossNPList np
        proof' <- glossProof proof
        pure $ Sem.Take loc vs constraints by proof'
    Raw.Subclaim loc subclaim subproof proof ->
        Sem.Subclaim loc <$> glossStmt subclaim <*> glossProof subproof <*> glossProof proof
    Raw.Suffices loc reduction by proof ->
        Sem.Suffices loc <$> glossStmt reduction <*> pure by <*> glossProof proof
    Raw.Define loc var term proof ->
        Sem.Define loc var <$> glossExpr term <*> glossProof proof
    Raw.DefineFunction loc funVar argVar valueExpr domVar domExpr proof ->
        if domVar == argVar
            then Sem.DefineFunction loc funVar argVar <$> glossExpr valueExpr <*> glossExpr domExpr <*> glossProof proof
            else error "mismatched variables in function definition."

    Raw.DefineFunctionLocal loc funVar domVar ranExpr funVar2 argVar definitions proof -> do
        if funVar == funVar2
            then Sem.DefineFunctionLocal loc funVar argVar domVar <$> glossExpr ranExpr <*> (glossLocalFunctionExprDef `each` definitions) <*> glossProof proof
            else error "missmatched function names"
    Raw.Calc loc calcQuant calc proof ->
        Sem.Calc loc <$> glossCalcQuantifier calcQuant <*> glossCalc calc <*> glossProof proof

glossCalcQuantifier :: Maybe Raw.CalcQuantifier -> Gloss Sem.CalcQuantifier
glossCalcQuantifier Nothing = pure Sem.CalcUnquantified
glossCalcQuantifier (Just (Raw.CalcQuantifier xs bound maySuchThat)) = do
    bound' <- glossBound bound
    maySuchThat' <- glossStmt `each` maySuchThat
    let maySuchThat'' = case (bound', maySuchThat') of
            _ -> Nothing
    pure (Sem.CalcForall xs maySuchThat'')

glossLocalFunctionExprDef :: (Raw.Expr, Raw.Formula) -> Gloss (Sem.Term, Sem.Formula)
glossLocalFunctionExprDef (definingExpression, localDomain) = do
    e <- glossExpr definingExpression
    d <- glossFormula localDomain
    pure (e,d)


glossCase :: Raw.Case -> Gloss Sem.Case
glossCase (Raw.Case caseOf proof) = Sem.Case <$> glossStmt caseOf <*> glossProof proof

glossCalc :: Raw.Calc -> Gloss Sem.Calc
glossCalc = \case
    Raw.Equation e eqns -> do
        e' <- glossExpr e
        eqns' <- (\(ei, ji) -> (,ji) <$> glossExpr ei) `each` eqns
        pure (Sem.Equation e' eqns')
    Raw.Biconditionals p ps -> do
        p' <- glossFormula p
        ps' <- (\(pi, ji) -> (,ji) <$> glossFormula pi) `each` ps
        pure (Sem.Biconditionals p' ps')

glossSignature :: Raw.Signature -> Gloss Sem.Signature
glossSignature sig = case sig of
    Raw.SignatureAdj v (Raw.Adj _loc adj vs) ->
        pure $ Sem.SignaturePredicate (Sem.PredicateAdj adj) (v :| vs)
    Raw.SignatureVerb v (Raw.Verb _loc verb vs) ->
        pure $ Sem.SignaturePredicate (Sem.PredicateVerb verb) (v :| vs)
    Raw.SignatureNoun v (Raw.Noun _loc noun vs) ->
        pure $ Sem.SignaturePredicate (Sem.PredicateNoun noun) (v :| vs)
    Raw.SignatureSymbolic (Raw.SymbolPattern op vs) np -> do
        (np', maySuchThat) <- glossNPMaybe np
        let andSuchThat phi = case maySuchThat of
                Just suchThat -> phi `Sem.And` suchThat
                Nothing -> phi
        let op' = Sem.TermOp Nowhere op (Sem.TermVar <$> vs)
        v <-  freshVar
        let v' = Sem.TermVar v
        pure $ Sem.SignatureFormula $ Sem.makeForall [v] ((Sem.Equals Nowhere  v' op') `Sem.Implies` andSuchThat (np' v'))


glossStructDefn :: Raw.StructDefn -> Gloss Sem.StructDefn
glossStructDefn (Raw.StructDefn phrase base carrier fixes assumes) = do
    assumes' <- (\(m, stmt) -> (m,) <$> glossStmt stmt) `each` assumes
    -- We substitute occurrences of the bare label with the builtin symbol @\carrier@.
    -- let assumes'' = fmap (annotateCarrierFormula carrier) assumes'
    let assumes'' = [(m, annotateCarrierFormula carrier phi) |(m, phi) <- assumes']
    let base' = Set.fromList base
    let fixes' = Set.fromList fixes
    pure $ Sem.StructDefn phrase base' carrier fixes' assumes''

-- | Replace free variables corresponding to the label of a structure
-- with the abstract carrier symbol.
annotateCarrierFormula :: Sem.VarSymbol -> Sem.Term -> Sem.Term
annotateCarrierFormula lbl = \case
    Sem.IsElementOf loc a (Sem.TermVar x) | x == lbl -> Sem.IsElementOf loc a (Sem.TermSymbolStruct CarrierSymbol (Just (Sem.TermVar lbl)))
    x -> x


glossAbbreviation :: Raw.Abbreviation -> Gloss Sem.Abbreviation
glossAbbreviation = \case
    Raw.AbbreviationAdj x (Raw.Adj _loc adj xs) stmt ->
        makeAbbrStmt (Sem.SymbolPredicate (Sem.PredicateAdj adj)) (x : xs) stmt
    Raw.AbbreviationVerb x (Raw.Verb _loc verb xs) stmt ->
        makeAbbrStmt (Sem.SymbolPredicate (Sem.PredicateVerb verb)) (x : xs) stmt
    Raw.AbbreviationNoun x (Raw.Noun _loc noun xs) stmt ->
        makeAbbrStmt (Sem.SymbolPredicate (Sem.PredicateNoun noun)) (x : xs) stmt
    Raw.AbbreviationRel x rel params y stmt ->
        makeAbbrStmt (Sem.SymbolPredicate (Sem.PredicateRelation rel)) (params <> [x, y]) stmt
    Raw.AbbreviationFun (Raw.Fun _loc fun xs) t ->
        makeAbbrTerm (Sem.SymbolFun fun) xs t
    Raw.AbbreviationEq (Raw.SymbolPattern op xs) e ->
        makeAbbrExpr (Sem.SymbolMixfix op) xs e


makeAbbrStmt :: Sem.Symbol -> [VarSymbol] -> Raw.Stmt -> Gloss (Sem.Abbreviation)
makeAbbrStmt symbol zs stmt = do
    stmt' <- glossStmt stmt
    let aux y = case y `List.elemIndex` zs of
            Nothing -> error ("free variable " <> show y <> " in abbreviation " <> show symbol)
            Just k -> Left k
    let scope = abstractEither aux stmt' :: Scope Int Sem.ExprOf Void
    pure (Sem.Abbreviation symbol scope)

makeAbbrTerm :: Sem.Symbol -> [VarSymbol] -> Raw.Term -> Gloss (Sem.Abbreviation)
makeAbbrTerm symbol zs t = do
    (t', _quantify) <- glossTerm t
    -- TODO replace "glossTerm t" with a more specific interpretation function
    -- that checks if no indefinite terms are part of the term (erroring out if the term is indefinite).
    let aux y = case y `List.elemIndex` zs of
            Nothing -> error ("free variable " <> show y <> " in abbreviation " <> show symbol)
            Just k -> Left k
    let scope = abstractEither aux t' :: Scope Int Sem.ExprOf Void
    pure (Sem.Abbreviation symbol scope)

makeAbbrExpr :: Sem.Symbol -> [VarSymbol] -> Raw.Expr -> Gloss (Sem.Abbreviation)
makeAbbrExpr symbol zs e = do
    e' <- glossExpr e
    -- TODO replace "glossTerm t" with a more specific interpretation function
    -- that checks if no indefinite terms are part of the term (erroring out if the term is indefinite).
    let aux y = case y `List.elemIndex` zs of
            Nothing -> error ("free variable " <> show y <> " in abbreviation " <> show symbol)
            Just k -> Left k
    let scope = abstractEither aux e' :: Scope Int Sem.ExprOf Void
    pure (Sem.Abbreviation symbol scope)


glossInductive :: Raw.Inductive -> Gloss Sem.Inductive
glossInductive (Raw.Inductive (Raw.SymbolPattern symbol args) domain rules) =
    Sem.Inductive symbol args <$> glossExpr domain <*> (glossRule `each` rules)
    where
        glossRule (Raw.IntroRule phis psi) = Sem.IntroRule <$> (glossFormula `each` phis) <*> glossFormula psi

glossBlock :: Raw.Block -> Gloss Sem.Block
glossBlock = \case
    Raw.BlockAxiom loc _title marker axiom ->
        Sem.BlockAxiom loc marker <$> glossAxiom axiom
    Raw.BlockClaim _claimKind loc _title marker lemma ->
        Sem.BlockLemma loc marker <$> glossLemma lemma
    Raw.BlockProof startLoc proof endLoc ->
        Sem.BlockProof startLoc endLoc <$> glossProof proof
    Raw.BlockDefn loc _title marker defn -> do
        defn' <- glossDefn defn
        whenLeft (isWellformedDefn defn') (\err -> throwError (GlossDefnError loc err marker))
        pure $ Sem.BlockDefn loc marker defn'
    Raw.BlockAbbr loc _title marker abbr ->
        Sem.BlockAbbr loc marker <$> glossAbbreviation abbr
    Raw.BlockSig loc _title marker asms sig ->
        Sem.BlockSig loc marker <$> glossAsms asms <*> glossSignature sig
    Raw.BlockStruct loc _title m structDefn ->
        Sem.BlockStruct loc m <$> glossStructDefn structDefn
    Raw.BlockData _pos _ ->
        _TODO "glossBlock datatype definitions"
    Raw.BlockInductive loc _title marker ind ->
        Sem.BlockInductive loc marker <$> glossInductive ind


glossBlocks :: [Raw.Block] -> Gloss [Sem.Block]
glossBlocks blocks = glossBlock `each` blocks
