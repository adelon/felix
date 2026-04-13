{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Checking (unitTests) where

import Base
import Checking
import Encoding (encodeTaskText)
import Report.Location
import Syntax.Internal
import Syntax.Lexicon (_Onesorted, pattern ApplySymbol)

import Bound.Scope (toScope)
import Control.Exception (try)
import Data.HashSet qualified as HS
import Data.InsOrdMap qualified as InsOrdMap
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Checking"
    [ testCase "fix rejects shadowing theorem-local variables" do
        expectCheckingError "already in scope" (lemmaBlocks "bad_fix" badFixLemma badFixProof)
    , testCase "define rejects shadowing local variables" do
        expectCheckingError "already in scope" (lemmaBlocks "bad_define" badDefineLemma badDefineProof)
    , testCase "take rejects shadowing theorem-local variables" do
        expectCheckingError "already in scope" (lemmaBlocks "bad_take" badTakeLemma badTakeProof)
    , testCase "have rejects new free variables" do
        expectCheckingError "not in scope" (lemmaBlocks "bad_have" badHaveLemma badHaveProof)
    , testCase "define function rejects shadowing function name" do
        expectCheckingError "already in scope" (lemmaBlocks "bad_define_function_name" badDefineFunctionNameLemma badDefineFunctionNameProof)
    , testCase "define function rejects shadowing argument name" do
        expectCheckingError "shadow local variable" (lemmaBlocks "bad_define_function_arg" badDefineFunctionArgLemma badDefineFunctionArgProof)
    , testCase "free theorem goal variables are local in proofs" do
        expectChecks (lemmaBlocks "free_goal_local" freeGoalLocalLemma freeGoalLocalProof)
    , testCase "fix accepts fresh local variables" do
        expectChecks (lemmaBlocks "fresh_fix" freshFixLemma freshFixProof)
    , testCase "take accepts fresh witnesses" do
        expectChecks (lemmaBlocks "fresh_take" freshTakeLemma freshTakeProof)
    , testCase "struct axioms recursively rewrite carrier labels" do
        text <- encodedTasksText structCarrierRewriteBlocks
        assertContains "rewritten struct-rule carrier" "elem(Xx,s__carrier(XA))" text
        assertNotContains "raw struct-rule carrier" "elem(Xx,XA)" text
    , testCase "struct axioms annotate operations from the same structure" do
        text <- encodedTasksText structOperationRewriteBlocks
        assertContains "annotated struct operation in rule" "elem(s__fooop(XA),s__carrier(XA))" text
    , testCase "exact structure claims introduce carrier labels for continuations" do
        text <- encodedTasksText structClaimIntroducesContextBlocks
        assertContains "claim continuation carrier" "conjecture,elem(fx,s__carrier(fA))" text
    , testCase "bound variables do not inherit outer carrier labels" do
        text <- encodedTasksText boundStructLabelBlocks
        assertContains "bound carrier label remains raw" "![XA]:elem(fx,XA)" text
        assertNotContains "bound carrier label not rewritten" "![XA]:elem(fx,s__carrier(XA))" text
    , testCase "abbreviations reject direct self-reference" do
        expectCheckingError "self-referential" directSelfReferentialAbbrBlocks
    , testCase "abbreviations reject indirect self-reference after expansion" do
        expectCheckingError "self-referential" indirectSelfReferentialAbbrBlocks
    , testCase "datatype accepts bootstrap propositional fragment" do
        expectChecks [goodDatatypeBlock]
    , testCase "datatype rejects nested recursive premise" do
        expectCheckingError "recursive premise must be direct" [badNestedRecursiveDatatypeBlock]
    , testCase "datatype rejects missing constructor premise" do
        expectCheckingError "missing premise" [badMissingPremiseDatatypeBlock]
    , testCase "datatype rejects duplicate constructor patterns" do
        expectCheckingError "constructor patterns must be distinct" [badDuplicateConstructorDatatypeBlock]
    , testCase "datatype rejects open premise domains" do
        expectCheckingError "closed terms" [badOpenDomainDatatypeBlock]
    , testCase "datatype rejects application-shaped constructors" do
        expectCheckingError "function application" [badApplyConstructorDatatypeBlock]
    , testCase "datatype generates trusted fact markers" do
        expectFactMarkers datatypeGeneratedMarkers [goodDatatypeBlock]
    , testCase "datatype generated fact markers are reserved globally" do
        expectDefinedMarkers datatypeGeneratedMarkers [goodDatatypeBlock]
    , testCase "datatype generated intro facts are usable by reference" do
        expectChecks datatypeIntroReferenceBlocks
    , testCase "datatype generated distinctness fact is usable by reference" do
        expectChecks datatypeDistinctReferenceBlocks
    , testCase "datatype generated injective fact is usable by reference" do
        expectChecks datatypeInjectiveReferenceBlocks
    , testCase "datatype generated fact markers cannot be reused by later blocks" do
        expectDuplicateMarker "propform_cases" datatypeGeneratedMarkerReuseBlocks
    , testCase "datatype rejects synthetic fact marker collisions within one datatype" do
        expectDuplicateMarker "propform_dup_intro" [badDuplicateGeneratedMarkerDatatypeBlock]
    ]

expectChecks :: [Block] -> Assertion
expectChecks blocks = do
    _tasks <- check WithoutDumpPremselTraining blocks
    pure ()

encodedTasksText :: [Block] -> IO Text
encodedTasksText blocks = do
    tasks <- check WithoutDumpPremselTraining blocks
    if null tasks
        then assertFailure "expected at least one generated task"
        else pure ()
    pure (Text.intercalate "\n------------------\n" (encodeTaskText <$> tasks))

expectCheckingError :: Text -> [Block] -> Assertion
expectCheckingError fragment blocks = do
    result <- try (check WithoutDumpPremselTraining blocks) :: IO (Either CheckingError [Task])
    case result of
        Right _ ->
            assertFailure "expected a CheckingError, but checking succeeded"
        Left err@(CheckingError msg _ _) ->
            assertBool ("expected error containing " <> show fragment <> ", got " <> show err) (fragment `Text.isInfixOf` msg)
        Left err ->
            assertFailure ("expected generic CheckingError, got " <> show err)

expectFactMarkers :: [Marker] -> [Block] -> Assertion
expectFactMarkers markers blocks = do
    checkingState <- runCheckingBlocks blocks (initialCheckingState WithoutDumpPremselTraining (\_task -> pure ()))
    let facts = checkingFacts checkingState
    for_ markers \marker ->
        assertBool ("expected generated fact marker " <> show marker) (isJust (InsOrdMap.lookup marker facts))

expectDefinedMarkers :: [Marker] -> [Block] -> Assertion
expectDefinedMarkers markers blocks = do
    checkingState <- runCheckingBlocks blocks (initialCheckingState WithoutDumpPremselTraining (\_task -> pure ()))
    let reserved = definedMarkers checkingState
    for_ markers \marker ->
        assertBool ("expected reserved marker " <> show marker) (HS.member marker reserved)

expectDuplicateMarker :: Marker -> [Block] -> Assertion
expectDuplicateMarker expectedMarker blocks = do
    result <- try (check WithoutDumpPremselTraining blocks) :: IO (Either CheckingError [Task])
    case result of
        Right _ ->
            assertFailure "expected a DuplicateMarker error, but checking succeeded"
        Left (DuplicateMarker _ actualMarker) ->
            assertEqual "duplicate marker" expectedMarker actualMarker
        Left err ->
            assertFailure ("expected DuplicateMarker, got " <> show err)

assertContains :: String -> Text -> Text -> Assertion
assertContains label needle haystack =
    assertBool (label <> ": expected to find " <> show needle <> " in " <> Text.unpack haystack) (needle `Text.isInfixOf` haystack)

assertNotContains :: String -> Text -> Text -> Assertion
assertNotContains label needle haystack =
    assertBool (label <> ": expected not to find " <> show needle <> " in " <> Text.unpack haystack) (not (needle `Text.isInfixOf` haystack))

lemmaBlocks :: Marker -> Lemma -> Proof -> [Block]
lemmaBlocks marker lemma proof =
    [BlockLemma Nowhere marker lemma, BlockProof Nowhere Nowhere proof]

badFixLemma :: Lemma
badFixLemma =
    Lemma [Asm (var "a" `eq` var "b")] (makeForall ["x"] (var "x" `eq` var "b"))

badFixProof :: Proof
badFixProof =
    Fix Nowhere ("a" :| []) Top (Qed (Just Nowhere) JustificationLocal)

badDefineLemma :: Lemma
badDefineLemma =
    Lemma [] (makeForall ["x"] (var "x" `eq` emptySet))

badDefineProof :: Proof
badDefineProof =
    Fix Nowhere ("x" :| []) Top (Define Nowhere "x" emptySet (Qed (Just Nowhere) JustificationLocal))

badTakeLemma :: Lemma
badTakeLemma =
    Lemma [Asm (makeExists ["x"] (var "x" `neq` var "b"))] (var "a" `neq` var "b")

badTakeProof :: Proof
badTakeProof =
    Take Nowhere ("a" :| []) (var "a" `neq` var "b") JustificationLocal (Qed (Just Nowhere) JustificationLocal)

badHaveLemma :: Lemma
badHaveLemma =
    Lemma [] Top

badHaveProof :: Proof
badHaveProof =
    Have Nowhere (var "c" `eq` var "c") JustificationEmpty (Qed (Just Nowhere) JustificationEmpty)

badDefineFunctionNameLemma :: Lemma
badDefineFunctionNameLemma =
    Lemma [] (var "f" `eq` var "f")

badDefineFunctionNameProof :: Proof
badDefineFunctionNameProof =
    DefineFunction Nowhere "f" "x" (var "x") emptySet (Qed (Just Nowhere) JustificationLocal)

badDefineFunctionArgLemma :: Lemma
badDefineFunctionArgLemma =
    Lemma [] (var "x" `eq` var "x")

badDefineFunctionArgProof :: Proof
badDefineFunctionArgProof =
    DefineFunction Nowhere "f" "x" (var "x") emptySet (Qed (Just Nowhere) JustificationLocal)

freeGoalLocalLemma :: Lemma
freeGoalLocalLemma =
    Lemma [] (var "b" `eq` var "b")

freeGoalLocalProof :: Proof
freeGoalLocalProof =
    Qed (Just Nowhere) JustificationEmpty

freshFixLemma :: Lemma
freshFixLemma =
    Lemma [] (makeForall ["x"] (var "x" `eq` var "x"))

freshFixProof :: Proof
freshFixProof =
    Fix Nowhere ("x" :| []) Top (Qed (Just Nowhere) JustificationEmpty)

freshTakeLemma :: Lemma
freshTakeLemma =
    Lemma [Asm (makeExists ["x"] (var "x" `eq` var "b"))] (var "b" `eq` var "b")

freshTakeProof :: Proof
freshTakeProof =
    Take Nowhere ("y" :| []) (var "y" `eq` var "b") JustificationLocal (Qed (Just Nowhere) JustificationEmpty)

structCarrierRewriteBlocks :: [Block]
structCarrierRewriteBlocks =
    [ fooStructBlock "foo_struct" [("foo_rule", makeForall ["x"] ((var "x" `isElementOf` var "A") `Implies` (var "x" `isElementOf` var "A")))]
    , BlockLemma Nowhere "foo_test" (Lemma [AsmStruct "A" fooStruct, Asm (var "x" `isElementOf` var "A")] (var "x" `isElementOf` var "A"))
    , BlockProof Nowhere Nowhere (Qed (Just Nowhere) (JustificationRef ("foo_rule" :| [])))
    ]

structOperationRewriteBlocks :: [Block]
structOperationRewriteBlocks =
    [ fooStructBlock "foo_op_struct" [("foo_op_rule", TermSymbolStruct fooOp Nothing `isElementOf` var "A")]
    , BlockLemma Nowhere "foo_op_test" (Lemma [AsmStruct "A" fooStruct] (TermSymbolStruct fooOp Nothing `isElementOf` var "A"))
    , BlockProof Nowhere Nowhere (Qed (Just Nowhere) (JustificationRef ("foo_op_rule" :| [])))
    ]

structClaimIntroducesContextBlocks :: [Block]
structClaimIntroducesContextBlocks =
    [ fooStructBlock "foo_claim_struct" []
    , BlockLemma Nowhere "foo_claim_test" (Lemma [] ((var "A" `eq` var "A") `And` (var "x" `eq` var "x")))
    , BlockProof Nowhere Nowhere $
        Have Nowhere (fooStructPredicate "A") JustificationEmpty $
            Have Nowhere (var "x" `isElementOf` var "A") JustificationEmpty Omitted
    ]

boundStructLabelBlocks :: [Block]
boundStructLabelBlocks =
    [ fooStructBlock "foo_bound_struct" []
    , BlockLemma Nowhere "foo_bound_test" (Lemma [AsmStruct "A" fooStruct] (makeForall ["A"] (var "x" `isElementOf` var "A")))
    , BlockProof Nowhere Nowhere (Qed (Just Nowhere) JustificationEmpty)
    ]

fooStructBlock :: Marker -> [(Marker, Formula)] -> Block
fooStructBlock marker assumes =
    BlockStruct Nowhere marker StructDefn
        { structPhrase = fooStruct
        , structParents = Set.singleton _Onesorted
        , structDefnLabel = "A"
        , structDefnFixes = Set.singleton fooOp
        , structDefnAssumes = assumes
        }

fooStruct :: StructPhrase
fooStruct =
    mkLexicalItemSgPl (unsafeReadPhraseSgPl "foo[/s]") "foo"

fooOp :: StructSymbol
fooOp =
    StructSymbol "fooop"

fooStructPredicate :: VarSymbol -> Formula
fooStructPredicate x =
    TermSymbol Nowhere (SymbolPredicate (PredicateNounStruct fooStruct)) [TermVar x]

var :: VarSymbol -> Term
var = TermVar

emptySet :: Term
emptySet = EmptySet Nowhere

eq :: Term -> Term -> Formula
eq = Equals Nowhere

neq :: Term -> Term -> Formula
neq = NotEquals Nowhere

directSelfReferentialAbbrBlocks :: [Block]
directSelfReferentialAbbrBlocks =
    [ BlockAbbr Nowhere "bad_abbr_direct" (Abbreviation abbrSymbolA (toScope (TermSymbol Nowhere abbrSymbolA [])))
    ]

indirectSelfReferentialAbbrBlocks :: [Block]
indirectSelfReferentialAbbrBlocks =
    [ BlockAbbr Nowhere "bad_abbr_indirect_a" (Abbreviation abbrSymbolA (toScope (TermSymbol Nowhere abbrSymbolB [])))
    , BlockAbbr Nowhere "bad_abbr_indirect_b" (Abbreviation abbrSymbolB (toScope (TermSymbol Nowhere abbrSymbolA [])))
    ]

goodDatatypeBlock :: Block
goodDatatypeBlock =
    datatypeBlock "good_datatype" goodDatatypeClauses

datatypeGeneratedMarkers :: [Marker]
datatypeGeneratedMarkers =
    [ "propform_propbot_intro"
    , "propform_propvar_intro"
    , "propform_propto_intro"
    , "propform_propbot_propvar_distinct"
    , "propform_propbot_propto_distinct"
    , "propform_propvar_propto_distinct"
    , "propform_propvar_injective"
    , "propform_propto_injective"
    , "propform_cases"
    , "propform_induct"
    ]

datatypeIntroReferenceBlocks :: [Block]
datatypeIntroReferenceBlocks =
    [ goodDatatypeBlock
    , BlockLemma Nowhere "datatype_propbot_intro_ref"
        (Lemma [] (propbotTerm `isElementOf` propformTerm))
    , BlockProof Nowhere Nowhere
        (Qed (Just Nowhere) (JustificationRef ("propform_propbot_intro" :| [])))
    , BlockLemma Nowhere "datatype_propvar_intro_ref"
        (Lemma [Asm (var "n" `isElementOf` naturalsTerm)] (termSymbol propvarSym [var "n"] `isElementOf` propformTerm))
    , BlockProof Nowhere Nowhere
        (Qed (Just Nowhere) (JustificationRef ("propform_propvar_intro" :| [])))
    , BlockLemma Nowhere "datatype_propto_intro_ref"
        (Lemma [Asm (propbotTerm `isElementOf` propformTerm)] (proptoTerm propbotTerm propbotTerm `isElementOf` propformTerm))
    , BlockProof Nowhere Nowhere
        (Qed (Just Nowhere) (JustificationRef ("propform_propto_intro" :| [])))
    ]

datatypeDistinctReferenceBlocks :: [Block]
datatypeDistinctReferenceBlocks =
    [ goodDatatypeBlock
    , BlockLemma Nowhere "datatype_distinct_ref"
        (Lemma [] (propbotTerm `neq` proptoTerm propbotTerm propbotTerm))
    , BlockProof Nowhere Nowhere
        (Qed (Just Nowhere) (JustificationRef ("propform_propbot_propto_distinct" :| [])))
    ]

datatypeInjectiveReferenceBlocks :: [Block]
datatypeInjectiveReferenceBlocks =
    [ goodDatatypeBlock
    , BlockLemma Nowhere "datatype_injective_ref"
        (Lemma [] (makeForall ["m", "n"] ((termSymbol propvarSym [var "m"] `eq` termSymbol propvarSym [var "n"]) `Implies` (var "m" `eq` var "n"))))
    , BlockProof Nowhere Nowhere
        (Qed (Just Nowhere) (JustificationRef ("propform_propvar_injective" :| [])))
    ]

datatypeGeneratedMarkerReuseBlocks :: [Block]
datatypeGeneratedMarkerReuseBlocks =
    [ goodDatatypeBlock
    , BlockLemma Nowhere "propform_cases" (Lemma [] Top)
    ]

badDuplicateGeneratedMarkerDatatypeBlock :: Block
badDuplicateGeneratedMarkerDatatypeBlock =
    datatypeBlock "bad_duplicate_generated_marker_datatype"
        ( DatatypeClause (SymbolPattern (unarySymbol "dup") ["n"]) [("n", naturalsTerm)] :|
            [ DatatypeClause (SymbolPattern (infixSymbol "dup") ["p", "q"]) [("p", propformTerm), ("q", propformTerm)]
            ]
        )

badNestedRecursiveDatatypeBlock :: Block
badNestedRecursiveDatatypeBlock =
    datatypeBlock "bad_nested_recursive_datatype"
        (DatatypeClause (SymbolPattern proptoSym ["p", "q"]) [("p", wrapPropformTerm), ("q", propformTerm)] :| [])

badMissingPremiseDatatypeBlock :: Block
badMissingPremiseDatatypeBlock =
    datatypeBlock "bad_missing_premise_datatype"
        (DatatypeClause (SymbolPattern propvarSym ["n"]) [] :| [])

badDuplicateConstructorDatatypeBlock :: Block
badDuplicateConstructorDatatypeBlock =
    datatypeBlock "bad_duplicate_constructor_datatype"
        ( DatatypeClause (SymbolPattern propbotSym []) [] :|
            [ DatatypeClause (SymbolPattern propbotSym []) []
            ]
        )

badOpenDomainDatatypeBlock :: Block
badOpenDomainDatatypeBlock =
    datatypeBlock "bad_open_domain_datatype"
        (DatatypeClause (SymbolPattern propvarSym ["n"]) [("n", TermVar "A")] :| [])

badApplyConstructorDatatypeBlock :: Block
badApplyConstructorDatatypeBlock =
    datatypeBlock "bad_apply_constructor_datatype"
        (DatatypeClause (SymbolPattern ApplySymbol ["f", "x"]) [("f", naturalsTerm), ("x", naturalsTerm)] :| [])

goodDatatypeClauses :: NonEmpty DatatypeClause
goodDatatypeClauses =
    DatatypeClause (SymbolPattern propbotSym []) [] :|
        [ DatatypeClause (SymbolPattern propvarSym ["n"]) [("n", naturalsTerm)]
        , DatatypeClause (SymbolPattern proptoSym ["p", "q"]) [("p", propformTerm), ("q", propformTerm)]
        ]

datatypeBlock :: Marker -> NonEmpty DatatypeClause -> Block
datatypeBlock marker clauses =
    BlockData Nowhere marker (Datatype (SymbolPattern propformSym []) clauses)

propformTerm :: Term
propformTerm =
    termSymbol propformSym []

propbotTerm :: Term
propbotTerm =
    termSymbol propbotSym []

naturalsTerm :: Term
naturalsTerm =
    termSymbol naturalsSym []

wrapPropformTerm :: Term
wrapPropformTerm =
    termSymbol wrapSym [propformTerm]

termSymbol :: FunctionSymbol -> [Term] -> Term
termSymbol sym args =
    TermSymbol Nowhere (SymbolMixfix sym) args

proptoTerm :: Term -> Term -> Term
proptoTerm left right =
    termSymbol proptoSym [left, right]

propformSym :: FunctionSymbol
propformSym =
    constSymbol "propform"

propbotSym :: FunctionSymbol
propbotSym =
    constSymbol "propbot"

propvarSym :: FunctionSymbol
propvarSym =
    unarySymbol "propvar"

proptoSym :: FunctionSymbol
proptoSym =
    infixSymbol "propto"

naturalsSym :: FunctionSymbol
naturalsSym =
    constSymbol "naturals"

wrapSym :: FunctionSymbol
wrapSym =
    unarySymbol "wrap"

constSymbol :: Text -> FunctionSymbol
constSymbol name =
    mkMixfixItem [Just (Command name)] (Marker name) NonAssoc

unarySymbol :: Text -> FunctionSymbol
unarySymbol name =
    mkMixfixItem [Just (Command name), Just InvisibleBraceL, Nothing, Just InvisibleBraceR] (Marker name) NonAssoc

infixSymbol :: Text -> FunctionSymbol
infixSymbol name =
    mkMixfixItem [Nothing, Just (Command name), Nothing] (Marker name) NonAssoc

abbrSymbolA :: Symbol
abbrSymbolA = abbrSymbol "unit_test_abbr_a"

abbrSymbolB :: Symbol
abbrSymbolB = abbrSymbol "unit_test_abbr_b"

abbrSymbol :: Text -> Symbol
abbrSymbol name =
    SymbolMixfix (MixfixItem (TokenCons (Command name) End) (Marker name) NonAssoc)
