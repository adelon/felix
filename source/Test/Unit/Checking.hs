{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Checking (unitTests) where

import Base
import Checking
import Report.Location
import Syntax.Internal

import Control.Exception (try)
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
    ]

expectChecks :: [Block] -> Assertion
expectChecks blocks = do
    _tasks <- check WithoutDumpPremselTraining blocks
    pure ()

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

var :: VarSymbol -> Term
var = TermVar

emptySet :: Term
emptySet = EmptySet Nowhere

eq :: Term -> Term -> Formula
eq = Equals Nowhere

neq :: Term -> Term -> Formula
neq = NotEquals Nowhere
