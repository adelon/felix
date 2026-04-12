{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module Checking where


import Base hiding (locally)
import StructGraph
import Syntax.Internal
import Syntax.Lexicon
import Encoding
import Tptp.UnsortedFirstOrder qualified as Tptp
import Report.Location

import Bound.Scope
import Bound.Var (Var(..), unvar)
import Control.Exception (Exception)
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.InsOrdMap (InsOrdMap)
import Data.InsOrdMap qualified as InsOrdMap
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.FilePath.Posix
import UnliftIO.Directory

type Checking = CheckingM ()
type CheckingM = StateT CheckingState IO

-- | Like 'Base.locally', but preserves the hypothesis counter so labels stay unique
-- across nested local proof blocks.
locally :: CheckingM a -> CheckingM a
locally ma = do
    st <- get
    a <- ma
    st' <- get
    put st{checkingHypothesisCounter = checkingHypothesisCounter st'}
    pure a

check :: WithDumpPremselTraining -> [Block] -> IO [Task]
check dumpPremselTraining blocks = do
    tasksRef <- newIORef []
    checkWith dumpPremselTraining blocks (\task -> modifyIORef' tasksRef (task :))
    reverse <$> readIORef tasksRef

initialCheckingState :: WithDumpPremselTraining -> (Task -> IO ()) -> CheckingState
initialCheckingState dumpPremselTraining emitTask = CheckingState
    { checkingAssumptions = []
    , checkingDumpPremselTraining = dumpPremselTraining
    , checkingGoals = []
    , checkingFacts = mempty
    , checkingDirectness = Direct
    , checkingAbbreviations = initAbbreviations
    , checkingPredicateDefinitions = mempty
    , checkingStructs = initCheckingStructs
    , checkingStructContext = mempty
    , definedMarkers = HS.empty
    , blockLabel = Marker ""
    , stepLocation = Nowhere
    , blockEndLocation = Nowhere
    , localVars = mempty
    , checkingHypothesisCounter = 0
    , checkingEmitTask = emitTask
    }

checkWith :: WithDumpPremselTraining -> [Block] -> (Task -> IO ()) -> IO ()
checkWith dumpPremselTraining blocks emitTask =
    void (runStateT (checkBlocks blocks) (initialCheckingState dumpPremselTraining emitTask))

runCheckingBlocks :: [Block] -> CheckingState -> IO CheckingState
runCheckingBlocks blocks checkingState =
    snd <$> runStateT (checkBlocks blocks) checkingState

data WithDumpPremselTraining = WithoutDumpPremselTraining | WithDumpPremselTraining

-- | The checking state manages contextual information while checking and
-- emits generated proof tasks through a callback.
-- INVARIANT: All formulas in the checking state that eventually
-- get exported should have all their abbreviations resolved.
data CheckingState = CheckingState
    { checkingDumpPremselTraining :: WithDumpPremselTraining

    , checkingAssumptions :: [Hypothesis]
    -- ^ Local assumptions (cached encoding).
    --
    , checkingGoals :: [Formula]
    -- ^ The current goals. INVARIANT: these should always be canonicalized and have all abbreviations resolved.
    --
    , checkingFacts :: InsOrdMap Marker Hypothesis
    -- ^ Axioms and proven results (cached encoding).
    --
    --
    , checkingDirectness :: Directness
    -- ^ E can detect contradictory axioms and warns about them.
    -- In an indirect proof (e.g. a proof by contradiction) we want
    -- to ignore that warning.
    --
    , checkingAbbreviations :: HashMap Symbol (Scope Int ExprOf Void)
    -- ^ Abbreviations are definitions that automatically get expanded.
    -- They are given by a closed rhs (hence the 'Void' indicating no free variables).
    -- INVARIANT: The bound 'Int' values must be lower than the arity of the symbol.
    --
    , checkingPredicateDefinitions :: HashMap Predicate [Scope Int ExprOf Void]
    -- ^ Definitions of predicate that we can match against in assumptions.
    -- Axioms and theorems that have the shape of a definition can be added as alternate definitions.
    --
    , checkingStructs :: StructGraph
    -- ^ Graph of structs defined so far.
    --
    , checkingStructContext :: StructContext
    -- ^ Structure labels and operations in scope for carrier and operation annotation.
    --
    , localVars :: Set VarSymbol
    -- ^ Keeps track of the variables that are locally constant.
    --
    , definedMarkers :: HashSet Marker
    -- ^ Markers for toplevel sections need to be unique. This keeps track of the
    -- markers used thus far.
    --
    , blockLabel :: Marker -- ^ Label/marker of the current block
    , stepLocation :: Location -- ^ Location of the current proof step
    , blockEndLocation :: Location -- ^ Ending of the current proof block, useful for error messages for implicit QEDs.
    , checkingHypothesisCounter :: Int -- ^ Counter for labeling local hypotheses within a proof.
    , checkingEmitTask :: Task -> IO () -- ^ Callback for emitting proof tasks.
    }

data StructContext = StructContext
    { structContextLabels :: Set VarSymbol
    , structContextOps :: HashMap StructSymbol VarSymbol
    }

instance Semigroup StructContext where
    StructContext labels ops <> StructContext labels' ops' =
        StructContext (labels <> labels') (ops <> ops')

instance Monoid StructContext where
    mempty = StructContext mempty mempty

initCheckingStructs :: StructGraph
initCheckingStructs = StructGraph.insert
    _Onesorted
    mempty -- no parents
    (Set.singleton CarrierSymbol) -- used for casting X -> \carrier[X]
    mempty -- empty graph

initAbbreviations :: HashMap Symbol (Scope Int ExprOf Void)
initAbbreviations = HM.fromList
    [ (SymbolPredicate (PredicateRelation NotElementSymbol), toScope (isNotElementOf Nowhere (TermVar (B 0)) (TermVar (B 1))))
    , (SymbolPredicate (PredicateVerb (mkLexicalItemSgPl (unsafeReadPhraseSgPl "equal[s/] ?") "eq")), toScope (Equals Nowhere (TermVar (B 0))( TermVar (B 1))))
    , (SymbolPredicate (PredicateNoun (mkLexicalItemSgPl (unsafeReadPhraseSgPl "element[/s] of ?") "elem")), toScope (isElementOf (TermVar (B 0)) (TermVar (B 1))))
    ]

data CheckingError
    = DuplicateMarker Location Marker
    | ByContradictionOnMultipleGoals Location Marker
    | BySetInductionSyntacticMismatch Location Marker
    | ProofWithoutPrecedingTheorem Location Marker
    | CouldNotEliminateHigherOrder FunctionSymbol Term Location Marker
    | AmbiguousInductionVar Location Marker
    | MismatchedSetExt [Formula] Location Marker
    | MismatchedAssume Formula Formula Location Marker
    | CheckingError Text Location Marker
    deriving (Show, Eq)

instance Exception CheckingError

throwWithLocationAndMarker :: (Location -> Marker -> CheckingError) -> CheckingM a
throwWithLocationAndMarker err = do
    m <- gets blockLabel
    loc <- gets stepLocation
    throwIO (err loc m)

throwWithMarker :: (Marker -> CheckingError) -> CheckingM a
throwWithMarker err = do
    m <- gets blockLabel
    throwIO (err m)

assume :: [Asm] -> Checking
assume asms = traverse_ go asms
    where
        go :: Asm -> Checking
        go = \case
            Asm phi -> do
                ctx <- structContextFromAssertion phi
                addStructContextAndRefreshGoals ctx
                assumeFormula phi
            AsmStruct x sp ->
                instantiateStruct x sp

assumeFormula :: Formula -> Checking
assumeFormula phi = do
    phi' <- canonicalize phi
    let phiContracted = contraction phi'
    case phiContracted of
        Top -> skip
        _ -> do
            marker <- nextHypothesisMarker
            let hypo = encodeHypothesisContracted marker phi' phiContracted
            modify \st ->
                st{checkingAssumptions = hypo : checkingAssumptions st}

instantiateStruct :: VarSymbol -> StructPhrase -> Checking
instantiateStruct x sp = do
    ctx <- structContextFor x sp
    addStructContextAndRefreshGoals ctx
    assumeFormula (structPredicate x sp)

structPredicate :: VarSymbol -> StructPhrase -> Formula
structPredicate x sp =
    TermSymbol Nowhere (SymbolPredicate (PredicateNounStruct sp)) [TermVar x]

structContextFromAssertion :: Formula -> CheckingM StructContext
structContextFromAssertion = \case
    TermSymbol _ (SymbolPredicate (PredicateNounStruct sp)) [TermVar x] ->
        structContextFor x sp
    _ ->
        pure mempty

structContextFor :: VarSymbol -> StructPhrase -> CheckingM StructContext
structContextFor x sp = do
    structGraph <- gets checkingStructs
    let struct = lookupStruct structGraph sp
    pure (structContextFromSymbols x (StructGraph.structSymbols struct structGraph))

structContextFromSymbols :: VarSymbol -> Set StructSymbol -> StructContext
structContextFromSymbols x symbols =
    StructContext (Set.singleton x) (HM.fromList [(op, x) | op <- Set.toList symbols])

addStructContext :: StructContext -> Checking
addStructContext ctx =
    modify \st -> st
        { checkingStructContext = ctx <> checkingStructContext st
        }

addStructContextAndRefreshGoals :: StructContext -> Checking
addStructContextAndRefreshGoals ctx
    | structContextIsEmpty ctx = skip
    | otherwise = do
        addStructContext ctx
        goals <- gets checkingGoals
        setGoals goals

structContextIsEmpty :: StructContext -> Bool
structContextIsEmpty StructContext{..} =
    Set.null structContextLabels && HM.null structContextOps

registerAssumptionStructContexts :: [Asm] -> Checking
registerAssumptionStructContexts asms = do
    ctx <- assumptionStructContext asms
    addStructContext ctx

assumptionStructContext :: [Asm] -> CheckingM StructContext
assumptionStructContext asms = do
    contexts <- traverse asmStructContext asms
    pure (foldl' (\older newer -> newer <> older) mempty contexts)

asmStructContext :: Asm -> CheckingM StructContext
asmStructContext = \case
    Asm phi ->
        structContextFromAssertion phi
    AsmStruct x sp ->
        structContextFor x sp


lookupStruct :: StructGraph -> StructPhrase -> Struct
lookupStruct structGraph struct = case StructGraph.lookup struct structGraph of
    Just result -> result
    Nothing -> error $ "lookup of undefined structure: " <> show struct

setLocation :: Location -> Checking
setLocation loc = modify \st -> st{stepLocation = loc}


-- | Replace all current goals with a new goal. Use with care!
setGoals :: [Formula] -> Checking
setGoals goals = do
    goals <- traverse canonicalize goals
    modify $ \st -> st{checkingGoals = goals}

nextHypothesisMarker :: CheckingM Marker
nextHypothesisMarker = do
    st <- get
    let next = checkingHypothesisCounter st + 1
    let marker = Marker ("_local_" <> Text.pack (show next))
    put st{checkingHypothesisCounter = next}
    pure marker

registerLemmaLocalVars :: [Asm] -> Formula -> Checking
registerLemmaLocalVars asms goal =
    addLocalVars (Set.unions (freeVars goal : (asmLocalVars <$> asms)))

asmLocalVars :: Asm -> Set VarSymbol
asmLocalVars = \case
    Asm phi ->
        freeVars phi
    AsmStruct x _ ->
        Set.singleton x

addLocalVars :: Set VarSymbol -> Checking
addLocalVars vars =
    modify \st -> st{localVars = vars <> localVars st}

checkFreshLocalVars :: Text -> [VarSymbol] -> Checking
checkFreshLocalVars context vars = do
    let duplicates = duplicateVars vars
    unless (Set.null duplicates) do
        throwCheckingError (context <> " introduces the same variable more than once: " <> formatVars duplicates)
    inScope <- gets localVars
    let shadowed = Set.fromList vars `Set.intersection` inScope
    unless (Set.null shadowed) do
        throwCheckingError (context <> " tries to introduce variable(s) already in scope: " <> formatVars shadowed)

checkBinderVarsFresh :: Text -> [VarSymbol] -> Checking
checkBinderVarsFresh context vars = do
    let duplicates = duplicateVars vars
    unless (Set.null duplicates) do
        throwCheckingError (context <> " uses the same binder variable more than once: " <> formatVars duplicates)
    inScope <- gets localVars
    let shadowed = Set.fromList vars `Set.intersection` inScope
    unless (Set.null shadowed) do
        throwCheckingError (context <> " binder variable(s) shadow local variable(s): " <> formatVars shadowed)

assertKnownFormulaVars :: Text -> Formula -> Checking
assertKnownFormulaVars context phi = do
    known <- gets localVars
    assertKnownFormulaVarsWith context known phi

assertKnownFormulaVarsWith :: Text -> Set VarSymbol -> Formula -> Checking
assertKnownFormulaVarsWith context known phi = do
    let unknown = freeVars phi `Set.difference` known
    unless (Set.null unknown) do
        throwCheckingError (context <> " mentions variable(s) that are not in scope: " <> formatVars unknown)

assertKnownTermVars :: Text -> Term -> Checking
assertKnownTermVars = assertKnownFormulaVars

assertKnownTermVarsWith :: Text -> Set VarSymbol -> Term -> Checking
assertKnownTermVarsWith = assertKnownFormulaVarsWith

throwCheckingError :: Text -> CheckingM a
throwCheckingError msg =
    throwWithLocationAndMarker (CheckingError msg)

duplicateVars :: [VarSymbol] -> Set VarSymbol
duplicateVars =
    snd . foldl' step (Set.empty, Set.empty)
    where
        step (seen, duplicates) x
            | x `Set.member` seen = (seen, Set.insert x duplicates)
            | otherwise = (Set.insert x seen, duplicates)

formatVars :: Set VarSymbol -> Text
formatVars vars =
    Text.intercalate ", " (formatVar <$> Set.toList vars)

formatVar :: VarSymbol -> Text
formatVar = \case
    NamedVar x -> x
    FreshVar n -> "_" <> Text.pack (show n)


-- | Create (and add) tasks based on facts, local assumptions, and goals.
tellTasks :: Checking
tellTasks = do
    goals <- gets checkingGoals
    m <- gets blockLabel
    facts <- gets checkingFacts
    assumptions <- gets checkingAssumptions
    directness <- gets checkingDirectness
    loc <- gets stepLocation
    emitTask <- gets checkingEmitTask
    let hypos = (snd <$> InsOrdMap.toList facts) <> assumptions
    liftIO (traverse_ emitTask (Task directness hypos m loc <$> goals))

-- | Make a fact available to all future paragraphs.
addFact :: Formula -> Checking
addFact phi = do
    phi' <- canonicalize phi
    m <- gets blockLabel
    let hypo = encodeHypothesis m phi'
    modify $ \st -> st{checkingFacts = InsOrdMap.insert m hypo (checkingFacts st)}


-- | Make a fact available to all future paragraphs.
addFacts :: InsOrdMap Marker Formula -> Checking
addFacts phis = do
    phis' <- forM (InsOrdMap.toList phis) \(m, phi) -> do
        phi' <- canonicalize phi
        let hypo = encodeHypothesis m phi'
        pure (m, hypo)
    modify $ \st -> st{checkingFacts = InsOrdMap.fromList phis' <> (checkingFacts st)}



addFactWithAsms :: [Asm] -> Formula -> Checking
addFactWithAsms asms stmt = do
    (asms', structContext) <- mergeAssumptions asms
    asms'' <- traverse (canonicalizeWith structContext) asms'
    stmt' <- canonicalizeWith structContext stmt
    m <- gets blockLabel
    modify $ \st ->
        let phi = case asms'' of
                [] -> forallClosure mempty stmt'
                _ -> forallClosure mempty (makeConjunction asms'' `Implies` stmt')
            hypo = encodeHypothesis m phi
        in st{checkingFacts = InsOrdMap.insert m hypo (checkingFacts st)}


-- | Mark a proof as indirect. Intended to be used in a @locally do@ block.
byContradiction :: Checking
byContradiction = do
    st <- get
    case checkingGoals st of
        [goal] -> put st{checkingGoals = [Bottom], checkingDirectness = Indirect goal}
        goals -> error ("multiple or no goal in proof by contradiction" <> show goals)


unabbreviateWith :: (forall a. HashMap Symbol (Scope Int ExprOf a)) -> (forall b. ExprOf b -> ExprOf b)
unabbreviateWith abbrs = unabbr
    where
        unabbr :: ExprOf b -> ExprOf b
        unabbr = \case
            TermSymbol loc sym es ->
                let es' = unabbr <$> es
                in case HM.lookup sym abbrs of
                        Nothing -> TermSymbol loc sym es'
                        Just scope -> unabbr (instantiate (\k -> nth k es ?? error "unabbreviateWith: incorrect index") scope)
            Not loc e ->
                Not loc (unabbr e)
            Apply e es ->
                Apply (unabbr e) (unabbr <$> es)
            TermSep vs e scope ->
                TermSep vs (unabbr e) (hoistScope unabbr scope)
            ReplacePred y x xB scope ->
                ReplacePred y x (unabbr xB) (hoistScope unabbr scope)
            ReplaceFun bounds scope cond ->
                ReplaceFun ((\(x, e) -> (x, unabbr e)) <$> bounds) (hoistScope unabbr scope) (hoistScope unabbr cond)
            Connected con e1 e2 ->
                Connected con (unabbr e1) (unabbr e2)
            Lambda scope ->
                Lambda  (hoistScope unabbr scope)
            Quantified quant scope      ->
                Quantified quant (hoistScope unabbr scope)
            e@PropositionalConstant{} ->
                e
            e@TermVar{} ->
                e
            TermSymbolStruct symb e ->
                TermSymbolStruct symb (unabbr <$> e)

-- | Unroll supported comprehensions in equations and reject leftover ones.
-- E.g. /@B = \\{f(a) | a\\in A \\}@/ turns into
-- /@\\forall b. b\\in B \\iff \\exists a\\in A. b = f(a)@/.
desugarComprehensionsA :: ExprOf a -> CheckingM (ExprOf a)
desugarComprehensionsA = \case
    -- We only desugar comprehensions directly under equations.
    -- Any remaining comprehension is currently unsupported.
    e@TermSep{} ->
        reject e
    e@ReplacePred{} ->
        reject e
    e@ReplaceFun{} ->
        reject e
    e@TermVar{} ->
        pure e
    e@PropositionalConstant{} ->
        pure e
    e@TermSymbolStruct{} ->
        pure e
    --
    Equals _pos e (TermSep x bound scope) ->
        pure (desugarSeparation e x bound scope)
    Equals _pos (TermSep x bound scope) e ->
        pure (desugarSeparation e x bound scope)
    --
    Equals _pos e (ReplaceFun bounds scope cond) ->
        pure (makeReplacementIff (F <$> e) bounds scope cond)
    Equals _pos (ReplaceFun bounds scope cond) e ->
        pure (makeReplacementIff (F <$> e) bounds scope cond)
    --
    Apply e es ->
        Apply <$> desugarComprehensionsA e <*> traverse desugarComprehensionsA es
    Not loc e ->
        Not loc <$> desugarComprehensionsA e
    TermSymbol loc sym es ->
        TermSymbol loc sym <$> traverse desugarComprehensionsA es
    Connected conn e1 e2 ->
        Connected conn <$> desugarComprehensionsA e1 <*> desugarComprehensionsA e2
    Lambda scope ->
        Lambda <$> transverseScope desugarComprehensionsA scope
    Quantified quant scope ->
        Quantified quant <$> transverseScope desugarComprehensionsA scope
    where
        reject :: ExprOf a -> CheckingM b
        reject _ =
            throwWithLocationAndMarker (CheckingError "Could not eliminate set comprehensions in this step. Nested comprehensions are not supported yet.")

        desugarSeparation :: ExprOf a -> VarSymbol -> (ExprOf a) -> (Scope () ExprOf a) -> ExprOf a
        desugarSeparation e x bound scope =
            let phi = isElementOf (TermVar (B x)) (F <$> e)
                psi = isElementOf (TermVar (B x)) (F <$> bound)
                rho = fromScope (mapBound (const x) scope)
            in  Quantified Universally (toScope (phi `Iff` (psi `And` rho)))




checkBlocks :: [Block] -> Checking
checkBlocks = \case
    BlockAxiom loc marker axiom : blocks -> do
        withLabel loc marker (checkAxiom axiom)
        checkBlocks blocks
    BlockDefn loc marker defn : blocks -> do
        withLabel loc marker (checkDefn loc defn)
        checkBlocks blocks
    BlockAbbr loc marker abbr : blocks -> do
        withLabel loc marker  (checkAbbr abbr)
        checkBlocks blocks
    BlockLemma loc marker lemma : BlockProof _startLoc endLoc proof : blocks -> do
        modify \st -> st{blockEndLocation = endLoc}
        withLabel loc marker (checkLemmaWithProof lemma proof)
        checkBlocks blocks
    BlockLemma loc marker  lemma : blocks -> do
        withLabel loc marker (checkLemma lemma)
        checkBlocks blocks
    BlockProof startLoc _endLoc _proof : _ ->
        throwWithMarker (ProofWithoutPrecedingTheorem startLoc)
    BlockSig loc marker asms sig : blocks -> do
        withLabel loc marker (checkSig asms sig)
        checkBlocks blocks
    BlockInductive loc marker inductiveDefn : blocks -> do
        withLabel loc marker (checkInductive inductiveDefn)
        checkBlocks blocks
    BlockStruct loc marker structDefn : blocks -> do
        withLabel loc marker (checkStructDefn structDefn)
        checkBlocks blocks
    [] -> skip

-- | Add the given label to the set of in-scope markers and set it as the current label for error reporting.
withLabel :: Location -> Marker -> CheckingM a -> CheckingM a
withLabel loc marker ma = do
    -- Add a new marker to the set. It is a checking error if the marker has already been used.
    st <- get
    let markers = definedMarkers st
    if HS.member marker markers
        then throwIO (DuplicateMarker loc marker)
        else put st{definedMarkers = HS.insert marker markers}
    -- Set the marker as the label of the current block and upate the step location.
    modify \st -> st{blockLabel = marker, stepLocation = loc, checkingHypothesisCounter = 0}
    ma

-- | Verification of a lemma with a proof.
-- We skip omitted proofs and treat them as proper gaps of the formalization.
-- This is useful when developing a formalization, as it makes it easy to
-- leave difficult proofs for later.
checkLemmaWithProof :: Lemma -> Proof -> Checking
checkLemmaWithProof (Lemma asms goal) proof = do
    locally do
        registerLemmaLocalVars asms goal
        registerAssumptionStructContexts asms
        assume asms
        setGoals [goal]
        checkProof proof
    addFactWithAsms asms goal


-- | Verification of a lemma without a proof.
checkLemma :: Lemma -> Checking
checkLemma (Lemma asms goal) = do
    locally do
        registerLemmaLocalVars asms goal
        registerAssumptionStructContexts asms
        assume asms
        setGoals [goal]
        tellTasks
    -- Make fact from asms and stmt (take universal closure).
    addFactWithAsms asms goal


checkAxiom :: Axiom -> Checking
checkAxiom (Axiom asms axiom) = addFactWithAsms asms axiom

checkProof :: Proof -> Checking
checkProof = \case
    Qed mloc j -> do
        loc <- case mloc of
                Just loc -> pure loc
                Nothing -> do
                    gets blockEndLocation
        setLocation loc
        justify j
    ByContradiction loc proof -> do
        setLocation loc
        goals <- gets checkingGoals
        case goals of
            [goal] -> do
                assume [Asm (Not loc goal)]
                byContradiction
                checkProof proof
            _ -> throwWithLocationAndMarker (ByContradictionOnMultipleGoals)
    ByCase loc splits -> do
        setLocation loc
        for_ splits \(Case split _) ->
            assertKnownFormulaVars "case split" split
        for_ splits checkCase
        setGoals [makeDisjunction (caseOf <$> splits)]
        tellTasks
    BySetInduction loc mx continue -> do
        setLocation loc
        goals <- gets checkingGoals
        case goals of
            Forall scope : goals' -> do
                let zs = nubOrd (bindings scope)
                z <- case mx of
                    Nothing -> case zs of
                        [z'] -> pure z'
                        _ -> throwWithMarker (AmbiguousInductionVar loc)
                    Just (TermVar z') -> pure z'
                    _ -> throwWithMarker (AmbiguousInductionVar loc)
                checkFreshLocalVars "set induction" [z]
                let y = NamedVar "IndAntecedent"
                let ys = List.delete z zs
                let anteInst bv = if bv == z then TermVar y else TermVar bv
                let antecedent = makeForall (y : ys) ((isElementOf (TermVar y) (TermVar z)) `Implies` instantiate anteInst scope)
                addLocalVars (Set.singleton z)
                assume [Asm antecedent]
                let consequent = instantiate TermVar scope
                setGoals (consequent : goals')
                checkProof continue
            _ -> throwWithMarker (BySetInductionSyntacticMismatch loc)
    ByOrdInduction loc continue -> do
        setLocation loc
        goals <- gets checkingGoals
        case goals of
            Forall scope : goals' -> case fromScope scope of
                Implies (IsOrd loc' (TermVar (B bz))) rhs -> do
                    let zs = nubOrd (bindings scope)
                    z <- case zs of
                            [z'] | z' == bz -> pure z'
                            [_] -> error "induction variable does not match the variable with ordinal guard"
                            _ -> throwWithMarker (AmbiguousInductionVar loc')
                    checkFreshLocalVars "ordinal induction" [z]
                    -- LATER: this is kinda sketchy:
                    -- we now use the induction variable in two ways:
                    -- we assume the induction hypothesis, where we recycle the induction variable both as a bound variable and a free variable
                    -- we then need to show that under that hypothesis the claim holds for the free variable...
                    let hypo = Forall (toScope (Implies (isElementOf (TermVar (B z)) (TermVar (F z))) rhs))
                    addLocalVars (Set.singleton z)
                    assume [Asm (IsOrd loc' (TermVar z)), Asm hypo]
                    let goal' = unvar id id <$> rhs -- we "instantiate" the single bound variable on the rhs
                    setGoals (goal' : goals')
                    checkProof continue
                _ -> error ("could not match transfinite induction with syntactic structure of the first goal: " <> show goals)
            _ -> error ("the first goal must be universally quantifier to apply transfinite induction: " <> show goals)
    Assume loc phi continue -> do
        setLocation loc
        assertKnownFormulaVars "assumption step" phi
        goals' <- matchAssumptionWithGoal loc phi
        assume [Asm phi]
        setGoals goals'
        checkProof continue
    Fix loc xs suchThat continue -> do
        setLocation loc
        checkFreshLocalVars "fix step" (toList xs)
        fixing xs
        addLocalVars (Set.fromList (toList xs))
        unless (suchThat == Top) do
            assertKnownFormulaVars "fix constraint" suchThat
        checkProof case suchThat of
            Top -> continue
            _ ->  Assume loc suchThat continue
    Subclaim loc subclaim subproof continue -> do
        setLocation loc
        assertKnownFormulaVars "subclaim" subclaim
        locally (checkLemmaWithProof (Lemma [] subclaim) subproof)
        assume [Asm subclaim]
        checkProof continue
    Omitted -> do
        setGoals []
    Suffices loc reduction by proof -> do
        setLocation loc
        assertKnownFormulaVars "suffices step" reduction
        goals <- gets checkingGoals
        setGoals [reduction `Implies` makeConjunction goals]
        justify by
        setGoals [reduction]
        checkProof proof
    Take loc _witnesses _suchThat JustificationSetExt _continue ->
        error $ "Error at " <> show loc <> "\nCannot justify existential statement with setext"
    Take loc witnesses suchThat by continue -> locally do
        setLocation loc
        checkFreshLocalVars "take step" (toList witnesses)
        known <- gets localVars
        assertKnownFormulaVarsWith "take witness statement" (known <> Set.fromList (toList witnesses)) suchThat
        goals <- gets checkingGoals
        setGoals [makeExists witnesses suchThat]
        justify by
        addLocalVars (Set.fromList (toList witnesses))
        assume [Asm suchThat]
        setGoals goals
        checkProof continue
    Have loc claim (JustificationRef ms) continue -> locally do
        setLocation loc
        assertKnownFormulaVars "claim" claim
        goals <- gets checkingGoals
        setGoals [claim]
        byRef ms -- locally prove things with just refs and local assumptions
        assume [Asm claim]
        setGoals goals
        checkProof continue
    Have loc claim JustificationLocal continue -> locally do
        setLocation loc
        assertKnownFormulaVars "claim" claim
        goals <- gets checkingGoals
        setGoals [claim]
        byAssumption -- locally prove things with just local assumptions
        assume [Asm claim]
        setGoals goals
        checkProof continue
    Have loc claim by continue -> do
        setLocation loc
        assertKnownFormulaVars "claim" claim
        locally do
            goals <- gets checkingGoals
            claims <- case by of
                JustificationEmpty ->
                    pure [claim]
                JustificationSetExt ->
                    splitGoalWithSetExt claim
                -- NOTE: we already handled @JustificationRef ms@ and GHC recognizes this
            setGoals claims
            tellTasks
            assume [Asm claim]
            setGoals goals
            checkProof continue
    Define loc x t continue -> locally do
        setLocation loc
        checkFreshLocalVars "definition step" [x]
        assertKnownTermVars "definition right-hand side" t
        addLocalVars (Set.singleton x)
        case t of
            TermSep y yBound phi ->
                assume [Asm $
                    makeForall [y] $
                        Iff (isElementOf (TermVar y) (TermVar x))
                            ((isElementOf (TermVar y) yBound) `And` instantiate1 (TermVar y) phi)
                    ]
            ReplacePred _y _x xBound scope -> do
                goals <- gets checkingGoals
                let x' = FreshVar 0
                let y  = FreshVar 1
                let y' = FreshVar 2
                let fromReplacementVar = \case
                        ReplacementDomVar -> TermVar x'
                        ReplacementRangeVar -> TermVar y
                let fromReplacementVar' = \case
                        ReplacementDomVar -> TermVar x'
                        ReplacementRangeVar -> TermVar y'
                let phi = instantiate fromReplacementVar scope
                let psi = instantiate fromReplacementVar' scope
                let singleValued =
                        makeForall [x'] $
                            (TermVar x' `isElementOf` xBound) `Implies`
                                makeForall [y, y'] (((phi `And` psi) `Implies` (TermVar y `equals` TermVar y')))
                setGoals [singleValued]
                tellTasks

                -- Now we restore the goals, which should already be canonical, so we don't need to canonicalize them again.
                modify (\st -> st{checkingGoals = goals})
                assume [Asm $
                    makeForall [y] $
                        (TermVar y `isElementOf` TermVar x)
                            `Iff`
                        makeExists [x'] ((TermVar x' `isElementOf` xBound) `And` phi)
                    ]
            ReplaceFun bounds lhs cond ->
                assume [Asm (makeReplacementIff (TermVar (F x)) bounds lhs cond)]
            _ ->
                assume [Asm (Equals loc (TermVar x) t)]
        checkProof continue
    DefineFunction loc funVar argVar valueExpr domExpr continue -> do
        setLocation loc
        checkFreshLocalVars "function definition" [funVar]
        checkBinderVarsFresh "function definition" [argVar]
        when (funVar == argVar) do
            throwCheckingError "function definition uses the same variable for the function and its argument"
        known <- gets localVars
        assertKnownTermVarsWith "function definition domain" known domExpr
        assertKnownTermVarsWith "function definition value" (Set.insert argVar known) valueExpr
        addLocalVars (Set.singleton funVar)
        -- we're given f, x, e, d
        assume
            [ Asm (Equals loc (TermOp Nowhere DomSymbol [TermVar funVar]) domExpr) -- dom(f) = d
            , Asm (makeForall [argVar] ((isElementOf (TermVar argVar) domExpr) `Implies` (Equals Nowhere (TermOp Nowhere ApplySymbol [TermVar funVar, TermVar argVar]) valueExpr))) -- f(x) = e for all x\in d
            , Asm (rightUniqueAdj loc (TermVar funVar))
            , Asm (relationNoun loc (TermVar funVar))
            ]
        checkProof continue
    Calc loc quant calc continue -> do
        setLocation loc
        for_ (calculation quant calc) \(goal, _) ->
            assertKnownFormulaVars "calculation step" goal
        assertKnownFormulaVars "calculation result" (calcResult quant calc)
        checkCalc quant calc
        assume [Asm (calcResult quant calc)]
        checkProof continue
    DefineFunctionLocal loc funVar argVar domVar ranExpr definitions continue -> do -- TODO refactor
        setLocation loc
        checkFreshLocalVars "local function definition" [funVar]
        checkBinderVarsFresh "local function definition" [argVar]
        when (funVar == argVar) do
            throwCheckingError "local function definition uses the same variable for the function and its argument"
        known <- gets localVars
        assertKnownFormulaVarsWith "local function definition range" known ranExpr
        let knownWithArg = Set.insert argVar known
        for_ definitions \(expr, frm) -> do
            assertKnownTermVarsWith "local function definition case expression" knownWithArg expr
            assertKnownFormulaVarsWith "local function definition case condition" knownWithArg frm
        unless (domVar `Set.member` known) do
            throwCheckingError ("local function definition domain variable is not in scope: " <> formatVars (Set.singleton domVar))
        addLocalVars (Set.singleton funVar)
        -- We have f: X \to Y and x \mapsto ...
        -- definition is a nonempty list of (expresssion e, formula phi)
        -- such that f(x) =  e if phi(x)
        -- since we do a case deduction in the definition there has to be a check that,
        -- our domains in the case are a disjunct union of dom(f)
        assume
            [Asm (Equals Nowhere (TermOp Nowhere DomSymbol [TermVar funVar]) (TermVar domVar))
            ,Asm (rightUniqueAdj Nowhere (TermVar funVar))
            ,Asm (relationNoun Nowhere (TermVar funVar))]

        goals <- gets checkingGoals
        setGoals [makeForall [argVar] ((isElementOf (TermVar argVar) (TermVar domVar)) `Iff` localFunctionGoal definitions)]
        tellTasks

        locals <- gets localVars
        assume [Asm (makeForall [argVar] (isElementOf (TermVar argVar) (TermVar domVar) `Implies` isElementOf (TermOp Nowhere ApplySymbol [TermVar funVar, TermVar argVar]) ranExpr))] -- function f from \dom(f) \to \ran(f)
        assume (functionSubdomianExpression funVar argVar domVar locals (NonEmpty.toList definitions)) --behavior on the subdomians
        setGoals goals
        checkProof continue

-- |Creats the Goal \forall x. x \in dom{f} \iff (phi_{1}(x) \xor (\phi_{2}(x) \xor (... \xor (\phi_{n}) ..)))
--  where the phi_{i} are the subdomain statments
localFunctionGoal :: NonEmpty (Term,Formula) -> Formula
localFunctionGoal xs = makeXor $ map snd $ NonEmpty.toList xs


-- We have our list of expr and forumlas, in this case normaly someone would write
-- f(x) =  ....cases
--      & (\frac{1}{k} \cdot x) &\text{if} x \in \[k, k+1\)
--
-- since we have to bind all globaly free Varibels we generate following asumptions.
--
-- For    x \mapsto  expr(x,ys,cs) , if formula(x,ys)        ; here cs are just global constants
--          ->   \forall x,ys: ( formula(x,ys) => expr(x,ys,cs))


functionSubdomianExpression :: VarSymbol -> VarSymbol -> VarSymbol -> Set VarSymbol -> [(Term, Formula)] -> [Asm]
functionSubdomianExpression f a d s (x:xs) = singleFunctionSubdomianExpression f a d s x : functionSubdomianExpression f a d s xs
functionSubdomianExpression _ _ _ _ [] = []

singleFunctionSubdomianExpression :: VarSymbol -> VarSymbol -> VarSymbol -> Set VarSymbol -> (Term, Formula) -> Asm
singleFunctionSubdomianExpression funVar argVar domVar fixedV (expr, frm) = let
    -- boundVar = Set.toList (freeVars expr) in
    -- let def = makeForall (argVar:boundVar) (((TermVar argVar `IsElementOf` TermVar domVar) `And` frm) `Implies` TermOp ApplySymbol [TermVar funVar, TermVar argVar] `Equals` expr)
    boundVar = fixedV in
    let def = forallClosure boundVar (((isElementOf (TermVar argVar) (TermVar domVar)) `And` frm) `Implies` Equals Nowhere (TermOp Nowhere ApplySymbol [TermVar funVar, TermVar argVar]) expr)
    in Asm def



checkCalc :: CalcQuantifier -> Calc -> Checking
checkCalc quant calc = locally do
    let tasks = calculation quant calc
    forM_ tasks tell
    where
        tell = \case
            (goal, by) -> setGoals [goal] *> justify by


makeReplacementIff
    :: forall a. (ExprOf (Var VarSymbol a) -- ^ Newly defined local constant.
    -> NonEmpty (VarSymbol, ExprOf a) -- ^ Bounds of the replacement.
    -> Scope VarSymbol ExprOf a -- ^ Left hand side (function application).
    -> Scope VarSymbol ExprOf a -- ^ Optional constraints on bounds (can just be 'Top').
    -> ExprOf a)
makeReplacementIff e bounds lhs cond =
    Forall (toScope (Iff (isElementOf (TermVar (B "frv")) e)  existsPreimage))
      where
        existsPreimage :: ExprOf (Var VarSymbol a)
        existsPreimage = Exists (toScope replaceBound)

        replaceBound :: ExprOf (Var VarSymbol (Var VarSymbol a))
        replaceBound = makeConjunction [isElementOf (TermVar (B x)) (F . F <$> xB) | (x, xB) <- toList bounds] `And` replaceCond

        replaceEq :: ExprOf (Var VarSymbol (Var VarSymbol a))
        replaceEq = Equals Nowhere (nestF <$> fromScope lhs) (TermVar (F (B "frv")))

        replaceCond :: ExprOf (Var VarSymbol (Var VarSymbol a))
        replaceCond = case fromScope cond of
            Top -> replaceEq
            cond' -> replaceEq `And` (F <$> cond')

        nestF :: Var b a1 -> Var b (Var b1 a1)
        nestF (B a) = B a
        nestF (F a) = F (F a)


splitGoalWithSetExt :: Formula -> CheckingM [Formula]
splitGoalWithSetExt = \case
    NotEquals loc x y -> do
        let z = FreshVar 0
            elemNotElem x' y' = makeExists [FreshVar 0] (And (IsElementOf loc (TermVar z) x') (isNotElementOf loc (TermVar z) y'))
        pure [elemNotElem x y `Or` elemNotElem y x]
    Equals loc x y -> do
        let z = FreshVar 0
            subset x' y' = makeForall [FreshVar 0] (Implies (IsElementOf loc (TermVar z) x') (IsElementOf loc (TermVar z) y'))
        pure [subset x y, subset y x]
    goal -> throwWithLocationAndMarker (MismatchedSetExt [goal])

justify :: Justification -> Checking
justify = \case
    JustificationEmpty -> tellTasks
    JustificationLocal -> byAssumption
    JustificationRef ms -> byRef ms
    JustificationSetExt -> do
        goals <- gets checkingGoals
        case goals of
            [goal] -> do
                goals' <- splitGoalWithSetExt goal
                setGoals goals'
                tellTasks
            _ -> throwWithLocationAndMarker (MismatchedSetExt goals)

byRef :: NonEmpty Marker -> Checking
byRef ms = locally do
    facts <- gets checkingFacts
    dumpPremselTraining <- gets checkingDumpPremselTraining
    case dumpPremselTraining of
        WithDumpPremselTraining -> dumpTrainingData facts ms
        WithoutDumpPremselTraining -> skip
    case InsOrdMap.lookupsMap ms facts of
        Left (Marker str) -> throwWithLocationAndMarker (CheckingError ("unknown marker: " <> str))
        Right facts' -> modify (\st -> st{checkingFacts = facts'}) *> tellTasks

byAssumption :: Checking
byAssumption = locally do
    modify (\st -> st{checkingFacts = mempty}) *> tellTasks

dumpTrainingData :: InsOrdMap Marker Hypothesis -> NonEmpty Marker -> Checking
dumpTrainingData facts ms = do
    let (picked, unpicked) = InsOrdMap.pickOutMap ms facts
    goals <- gets checkingGoals
    m@(Marker m_) <- gets blockLabel
    loc <- gets stepLocation
    let dir = "premseldump"
    let makePath k = dir </> (Text.unpack m_ <> show (k :: Int)) <.> "txt"
    let dumpTrainingExample goal =
            let conj = encodeConjecture m loc Direct goal
                usefuls = encodeWithRole Tptp.AxiomUseful (snd <$> InsOrdMap.toList picked)
                redundants = encodeWithRole Tptp.AxiomRedundant (snd <$> InsOrdMap.toList unpicked)
                k = hash goal
                example = Tptp.toTextNewline (Tptp.Task (conj : (usefuls <> redundants)))
            in do
                liftIO (Text.writeFile (makePath k) example)
    liftIO (createDirectoryIfMissing True dir)
    forM_ goals dumpTrainingExample

-- | Since the case tactic replaces /all/ current goals with the disjunction
-- of the different case hypotheses, each case proof must cover all goals.
checkCase :: Case -> Checking
checkCase (Case split proof) = locally do
        assume [Asm split]
        checkProof proof


checkDefn :: Location -> Defn -> Checking
checkDefn loc = \case
    DefnPredicate asms symb vs f -> do
        -- We first need to take the universal closure of the defining formula
        -- while ignoring the variables that occur on the lhs, then take the
        -- universal formula of the equivalence, quantifying the remaining
        -- variables (from the lhs).
        let vs' = TermVar <$> toList vs
        let f' = forallClosure (Set.fromList (toList vs)) f
        addFactWithAsms asms (Atomic loc symb vs' `Iff` f')
    DefnFun asms fun vs rhs -> do
        let lhs = TermSymbol loc (SymbolFun fun) (TermVar <$> vs)
        addFactWithAsms asms (Equals loc lhs rhs)
    -- TODO Check that the function symbol on the lhs does not appear on the rhs.
    DefnOp op vs (TermSep x bound phi) ->
        addFact $ makeForall (x : vs) $
            Iff (TermVar x `isElementOf` TermOp loc op (TermVar <$> vs))
                ((TermVar x `isElementOf` bound) `And` instantiate1 (TermVar x) phi)
    DefnOp op vs (TermSymbol _pos symbol [x, y]) | symbol == SymbolMixfix ConsSymbol -> do
        -- TODO generalize this to support arbitrarily many applications of _Cons
        -- and also handle the case of emptyset or singleton as final argument separately
        -- so that finite set terms get recognized in full.
        let phi = isElementOf (TermVar "any") (TermOp loc op (TermVar <$> vs))
        let psi = (isElementOf (TermVar "any") y) `Or` (Equals loc (TermVar "any") x)
        addFact (makeForall ("any" : vs) (phi `Iff` psi))
    DefnOp op vs (ReplacePred _y _x xBound scope) -> do
        let x  = (FreshVar 0)
        let y  = (FreshVar 1)
        let y' = (FreshVar 2)
        let fromReplacementVar = \case
                ReplacementDomVar -> TermVar x
                ReplacementRangeVar -> TermVar y
        let fromReplacementVar' = \case
                ReplacementDomVar -> TermVar x
                ReplacementRangeVar -> TermVar y'
        let phi = instantiate fromReplacementVar scope
        let psi = instantiate fromReplacementVar' scope
        let singleValued = makeForall [x] ((TermVar x `isElementOf` xBound) `Implies` makeForall [y, y'] ((phi `And` psi) `Implies` (TermVar y `equals` TermVar y')))
        setGoals [singleValued]
        tellTasks
        addFact (makeForall (y : vs) ((TermVar y `isElementOf` TermOp Nowhere op (TermVar <$> vs)) `Iff` makeExists [x] ((TermVar x `isElementOf` xBound) `And` phi)))

    DefnOp op vs (ReplaceFun bounds lhs cond) ->
        addFact (forallClosure mempty (makeReplacementIff (TermOp Nowhere op (TermVar . F <$> vs)) bounds lhs cond))
    DefnOp op vs rhs ->
        if containsHigherOrderConstructs rhs
            then throwWithMarker (CouldNotEliminateHigherOrder op rhs loc)
            else do
                let lhs = TermSymbol Nowhere (SymbolMixfix op) (TermVar <$> vs)
                addFactWithAsms [] (Equals Nowhere lhs rhs)


checkSig :: [Asm] -> Signature -> Checking
checkSig asms sig = case sig of
        SignatureFormula f ->
            addFactWithAsms asms f
        SignaturePredicate _predi _vs -> do
            skip -- TODO

mergeAssumptions :: [Asm] -> CheckingM ([Formula], StructContext)
mergeAssumptions asms = do
    ctx <- assumptionStructContext asms
    phis <- traverse asmFormula asms
    pure (phis, ctx)

asmFormula :: Asm -> CheckingM Formula
asmFormula = \case
    Asm phi ->
        pure phi
    AsmStruct x phrase ->
        pure (structPredicate x phrase)

canonicalize :: Formula -> CheckingM Formula
canonicalize phi = do
    ctx <- gets checkingStructContext
    canonicalizeWith ctx phi

-- | Canonicalize using an explicit structure context instead of the checker state's current one.
canonicalizeWith :: StructContext -> Formula -> CheckingM Formula
canonicalizeWith ctx phi = do
    phi' <- unabbreviate phi
    desugarComprehensionsA (annotateWithStructContext ctx phi')

annotateWithStructContext :: StructContext -> Formula -> Formula
annotateWithStructContext StructContext{..} =
    annotateWith structContextLabels structContextOps

unabbreviate :: ExprOf a -> CheckingM (ExprOf a)
unabbreviate phi = do
    abbrs <- gets checkingAbbreviations
    pure (unabbreviateWith ((absurd <$>) <$> abbrs) phi)



checkInductive :: Inductive -> Checking
checkInductive Inductive{..} = do
    forM inductiveIntros (checkIntroRule inductiveSymbol inductiveParams inductiveDomain)
    addFact (forallClosure mempty (IsSubsetOf Nowhere (TermOp Nowhere inductiveSymbol (TermVar <$> inductiveParams)) inductiveDomain))
    forM_ inductiveIntros addIntroRule

addIntroRule :: IntroRule -> Checking
addIntroRule (IntroRule conditions result) = addFact (forallClosure mempty (makeConjunction conditions `Implies` result))

checkIntroRule :: FunctionSymbol -> [VarSymbol] -> Expr -> IntroRule -> Checking
checkIntroRule f args dom rule = do
    case isValidIntroRule f args rule of
        Left err -> throwWithLocationAndMarker (\loc marker -> CheckingError (Text.pack err) loc marker)
        Right goals -> case List.filter (/= Top) goals of
            [] -> skip
            goals' -> setGoals goals' *> tellTasks
    setGoals [typecheckRule f args dom rule] *> tellTasks

-- isValidCondition e phi returns 'Right' the needed monotonicity proof tasks if the condition is a valid for an inductive definition of e, and returns 'Left' if the condition is invalid.
isValidCondition :: FunctionSymbol -> [VarSymbol] -> Formula -> Either String Formula
isValidCondition f args phi = if isSideCondition phi
    then Right Top -- Side conditions do not require any monotonicty proofs.
    else monotonicty phi
    where
        -- Side conditions are formulas in which f does not appear.
        isSideCondition :: ExprOf a -> Bool
        isSideCondition = \case
            Not _pos a -> isSideCondition a
            Connected _ a b -> isSideCondition a && isSideCondition b
            TermVar _ -> True
            TermSymbol _pos f' args -> f' /= SymbolMixfix f && all isSideCondition args
            TermSymbolStruct _ Nothing -> True
            TermSymbolStruct _ (Just e) -> isSideCondition e
            Apply a b -> isSideCondition a && all isSideCondition b
            TermSep _ xB cond -> isSideCondition xB && isSideCondition (fromScope cond)
            ReplacePred _ _ e scope -> isSideCondition e && isSideCondition (fromScope scope)
            ReplaceFun bounds lhs rhs ->
                all (\(_, e) -> isSideCondition e) bounds && isSideCondition (fromScope lhs) && isSideCondition (fromScope rhs)
            Lambda body -> isSideCondition (fromScope body)
            Quantified _ body -> isSideCondition (fromScope body)
            PropositionalConstant _ -> True
        -- Potential monotonicity task for conditions in which f appears
        monotonicty = \case
            -- Conditions that are not side conditions must be atomic statements about membership
            IsElementOf _pos1 _e (TermOp _pos2 f' args') | f' == f && args' == (TermVar <$> args) ->
                Right Top -- No monotonicity to prove if the symbols occurs plainly.
            IsElementOf _pos _ e ->
                Right (monotone (extractMonotonicFunction e))
            _ -> Left "Intro rule not of the form \"_ \\in h(_) \""
        -- IMPORTANT: we assume that extractMonotonicFunction is applied to a first-order term
        extractMonotonicFunction :: Expr -> (Expr -> Expr)
        extractMonotonicFunction e = \x -> go x e
            where
                go x = \case
                    TermSymbol loc f' args' -> if
                        | f' == SymbolMixfix f && args' == (TermVar <$> args) -> x
                        | f' == SymbolMixfix f -> error ("symbol " <> show f <> " occurred with the wrong arguments " <> show args')
                        | otherwise -> TermSymbol loc f' (go x <$> args')
                    TermVar x -> TermVar x
                    e@(TermSymbolStruct _ Nothing) -> e
                    TermSymbolStruct s (Just e') -> TermSymbolStruct s (Just (go x e'))
                    _ -> error "could not extract monotonic function"

isValidResult :: FunctionSymbol -> [VarSymbol] -> Formula -> Bool
isValidResult f args phi = case phi of
    IsElementOf _pos _ e | equivalent e (TermOp Nowhere f (TermVar <$> args)) -> True
    _ -> False

isValidIntroRule :: FunctionSymbol -> [VarSymbol] -> IntroRule -> Either String [Formula]
isValidIntroRule f args rule = if isValidResult f args (introResult rule)
    then mapM (isValidCondition f args) (introConditions rule)
    else Left "invalid result in rule"

monotone :: (Expr -> Expr) -> Formula
monotone h = makeForall ("xa" : ["xb"])
        ((IsSubsetOf Nowhere (TermVar "xa") (TermVar "xb")) `Implies` (IsSubsetOf Nowhere (h (TermVar "xa")) (h (TermVar "xb"))))

typecheckRule :: FunctionSymbol -> [VarSymbol] -> Expr -> IntroRule -> Formula
typecheckRule f args dom (IntroRule conds result) = makeConjunction (go <$> conds) `Implies` go result
    where
        -- replace symbol by dom for TC rule
        go :: Expr -> Expr
        go = \case
            TermSymbol loc f' args' -> if
                | f' == SymbolMixfix f && args' == (TermVar <$> args) -> dom
                | f' == SymbolMixfix f -> error ("typecheckRule: symbol " <> show f <> " occurred with the wrong arguments " <> show args')
                | otherwise -> TermSymbol loc f' (go <$> args')
            TermVar x -> TermVar x
            e@(TermSymbolStruct _ Nothing) -> e
            TermSymbolStruct s (Just e') -> TermSymbolStruct s (Just (go e'))
            Not loc a -> Not loc (go a)
            Connected conn a b -> Connected conn (go a) (go b)
            Apply a b -> Apply (go a) (go <$> b)
            e@PropositionalConstant{} -> e
            --TermSep x xB cond -> TermSep x (go xB) (hoistScope go cond)
            --ReplacePred x y e scope -> ReplacePred x y (go e) (hoistScope go scope)
            --ReplaceFun bounds lhs rhs ->
            --    ReplaceFun ((\(x, e) -> (x, go e)) <$> bounds) (hoistScope go lhs) (hoistScope go rhs)
            --Lambda body -> Lambda (hoistScope go body)
            --Quantified quant body -> Quantified quant (hoistScope go body)
            _ -> error "typecheckRule does not handle binders"

checkStructDefn :: StructDefn -> Checking
checkStructDefn StructDefn{..} = do
    st <- get
    let structGraph = checkingStructs st
    let m@(Marker m_) = blockLabel st
    let structAncestors = Set.unions (Set.map (`StructGraph.lookupAncestors` structGraph) structParents)
    let structAncestors' = structParents <> structAncestors
    let parentSymbols = Set.unions (Set.map (`StructGraph.lookupSymbols` structGraph) structParents)
    let structContext = structContextFromSymbols structDefnLabel (structDefnFixes <> parentSymbols)
    let isStruct p = TermSymbol Nowhere (SymbolPredicate (PredicateNounStruct p)) [TermVar structDefnLabel]
    let intro = if structParents == Set.singleton _Onesorted
            then makeConjunction (snd <$> structDefnAssumes) `Implies` isStruct structPhrase
            else makeConjunction ([isStruct parent | parent <- toList structParents] <> (snd <$> structDefnAssumes)) `Implies` isStruct structPhrase
    let intro' = (m, intro)
    let inherit' = (Marker (m_ <> "inherit"), isStruct structPhrase `Implies` makeConjunction [isStruct parent | parent <- toList structParents])
    let elims' = [(marker, isStruct structPhrase `Implies` phi) | (marker, phi) <- structDefnAssumes]
    rules' <- forM (InsOrdMap.toList (InsOrdMap.fromList (intro' : inherit' : elims'))) \(marker, phi) -> do
        phi' <- canonicalizeWith structContext phi
        pure (marker, encodeHypothesis marker (forallClosure mempty phi'))
    let rules'' = InsOrdMap.fromList rules'
    put st
            { checkingStructs = StructGraph.insert structPhrase structAncestors' structDefnFixes (checkingStructs st)
            , checkingFacts = rules'' <> checkingFacts st
            }

fixing :: NonEmpty VarSymbol -> Checking
fixing xs = do
    goals <- gets checkingGoals
    (goal, goals') <- case goals of
            goal : goals' -> return (goal, goals')
            _ -> throwWithLocationAndMarker (CheckingError "No open goals, cannot use \"fix\" step")
    goal' <- case goal of
            Forall body | [_bv] <- nubOrd (bindings body) ->
                -- If there's only one quantified variable we can freely choose a new name.
                -- This is useful for nameless quantified phrases such as @every _ is an element of _@.
                case xs of
                    x :| [] -> return (instantiate (\_bv -> TermVar x) body)
                    _ -> throwWithLocationAndMarker (CheckingError "Couldn't use \"fix\" step: only one bound variable but multiple variables to be fixed")
            Forall body | toList xs `List.intersect` nubOrd (bindings body) == toList xs ->
                return (Forall (instantiateSome xs body))
            Forall body ->
                throwWithLocationAndMarker (CheckingError ("You can only use a \"fix\" step if all specified variables occur in the outermost quantifier. Variables to be fixed were: "
                    <> Text.pack (show xs) <> " but only the following are bound: " <> Text.pack (show (nubOrd (bindings body)))))
            _ ->
                throwWithLocationAndMarker (CheckingError "You can only use a \"fix\" step if the goal is universal.")
    setGoals (goal' : goals')

-- | An assumption step in a proof is supposed to match the goal.
matchAssumptionWithGoal :: Location -> Formula -> CheckingM [Formula]
matchAssumptionWithGoal loc asm = do
    asm' <- canonicalize asm
    goals <- gets checkingGoals
    let (goal, goals') = case goals of
            goal : goals' -> (goal, goals')
            _ -> error "no open goals, cannot use assumption step"
    defns <- gets checkingPredicateDefinitions
    case syntacticMatch asm' goal of
        Just phi -> pure (phi : goals')
        --
        -- Unfolding definitions against atomic goals
        Nothing -> case goal of
            phi@(Atomic _pos p args) ->
                let rhos = (HM.lookup p defns ?? [])
                    rhos' = [instantiate (\k -> nth k args ?? error "defns: incorrect index") (absurd <$> rho) | rho <- rhos]
                in case firstJust (syntacticMatch asm') rhos' of
                    Nothing -> throwWithMarker (MismatchedAssume asm' phi loc)
                    Just match -> pure (match : goals')
            phi -> throwWithMarker (MismatchedAssume asm' phi loc)


syntacticMatch :: Formula -> Formula -> Maybe Formula
syntacticMatch asm = \case
    (phi1 `And` phi2) `Implies` psi  | equivalent phi1 asm ->
        Just (phi2 `Implies` psi)
    (phi1 `And` phi2) `Implies` psi  | equivalent phi2 asm ->
        Just (phi1 `Implies` psi)
    phi `Implies` psi  | equivalent phi asm ->
        Just psi
    phi `Or` psi | equivalent phi asm ->
        Just (dual psi)
    _ -> Nothing


checkAbbr :: Abbreviation -> Checking
checkAbbr (Abbreviation symbol scope) = do
    scope' <- transverseScope unabbreviate scope
    modify (\st -> st{checkingAbbreviations = HM.insert symbol scope' (checkingAbbreviations st)})
