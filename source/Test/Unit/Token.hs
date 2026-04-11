{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Token (unitTests) where

import Base
import Report.Location
import Syntax.Token

import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (errorBundlePretty)

unitTests :: TestTree
unitTests = testGroup "Lexer"
    [ testCase "nested math inside text returns to text" nestedMathInsideText
    , testCase "nested text and math returns to the enclosing frame" deeperAlternation
    , testCase "braces inside text do not close the text frame" textBraceNesting
    , testCase "cases is tokenized as an environment only inside math" casesOnlyInsideMath
    ]

nestedMathInsideText :: Assertion
nestedMathInsideText = do
    tokens <- tokensInProof "$\\text{if $x \\in A$ then}$"
    tokens `shouldBe`
        [ BeginEnv "proof"
        , BeginEnv "math"
        , BeginEnv "text"
        , Word "if"
        , BeginEnv "math"
        , Variable "x"
        , Command "in"
        , Variable "A"
        , EndEnv "math"
        , Word "then"
        , EndEnv "text"
        , EndEnv "math"
        , EndEnv "proof"
        ]

deeperAlternation :: Assertion
deeperAlternation = do
    tokens <- tokensInProof "$\\text{a $ \\text{b $c$ d} e$ f}$"
    tokens `shouldBe`
        [ BeginEnv "proof"
        , BeginEnv "math"
        , BeginEnv "text"
        , Word "a"
        , BeginEnv "math"
        , BeginEnv "text"
        , Word "b"
        , BeginEnv "math"
        , Variable "c"
        , EndEnv "math"
        , Word "d"
        , EndEnv "text"
        , Variable "e"
        , EndEnv "math"
        , Word "f"
        , EndEnv "text"
        , EndEnv "math"
        , EndEnv "proof"
        ]

textBraceNesting :: Assertion
textBraceNesting = do
    tokens <- tokensInProof "$\\text{a {b}}$"
    tokens `shouldBe`
        [ BeginEnv "proof"
        , BeginEnv "math"
        , BeginEnv "text"
        , Word "a"
        , InvisibleBraceL
        , Word "b"
        , InvisibleBraceR
        , EndEnv "text"
        , EndEnv "math"
        , EndEnv "proof"
        ]

casesOnlyInsideMath :: Assertion
casesOnlyInsideMath = do
    tokensInsideMath <- tokensInProof "$\\begin{cases}x\\end{cases}$"
    tokensInsideMath `shouldBe`
        [ BeginEnv "proof"
        , BeginEnv "math"
        , BeginEnv "cases"
        , Variable "x"
        , EndEnv "cases"
        , EndEnv "math"
        , EndEnv "proof"
        ]

    tokensOutsideMath <- tokensInProof "\\begin{cases}x\\end{cases}"
    assertBool
        "cases should not be tokenized as an environment outside math"
        (BeginEnv "cases" `notElem` tokensOutsideMath && EndEnv "cases" `notElem` tokensOutsideMath)

tokensInProof :: Text -> IO [Token]
tokensInProof raw =
    case runLexer (FileId maxBound) "lexer-unit" wrapped of
        Left err ->
            assertFailure (errorBundlePretty err)
        Right (_imports, chunks) ->
            pure (concatMap (map unLocated) chunks)
    where
        wrapped = Text.unlines
            [ "\\begin{proof}"
            , raw
            , "\\end{proof}"
            ]

shouldBe :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
shouldBe = flip (assertEqual "")
