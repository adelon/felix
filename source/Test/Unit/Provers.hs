{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Provers (unitTests) where

import Base
import Provers
import Report.Location (pattern Nowhere)
import Syntax.Internal (Directness(..), Marker(..), Task(..), pattern Top)

import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parseMaybe)

shouldBe :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
shouldBe = flip (assertEqual "")

unitTests :: TestTree
unitTests = testGroup "Provers"
    [ testCase "vampireStatusParser parses canonical SZS status line" do
        parseMaybe vampireStatusParser "% SZS status ContradictoryAxioms for 2260"
            `shouldBe` Just "ContradictoryAxioms"
    , testCase "vampireStatusParser parses worker-prefixed SZS status line" do
        parseMaybe vampireStatusParser "% (2581105)SZS status Timeout for "
            `shouldBe` Just "Timeout"
    , testCase "vampireStatusParser keeps unknown status as data" do
        parseMaybe vampireStatusParser "% SZS status ResourceOut for 2260"
            `shouldBe` Just "ResourceOut"
    , testCase "Vampire recognizer reads status from stderr too" do
        recognizeAnswer vampireProver directTask "" "% (2581105)SZS status Timeout for "
            `shouldBe` Uncertain
    , testCase "ContradictoryAxioms counts as Yes for indirect goals" do
        recognizeAnswer vampireProver indirectTask "" "% (2581105)SZS status ContradictoryAxioms for "
            `shouldBe` Yes
    , testCase "Vampire recognizer uses first parsed status only" do
        let
            out = Text.unlines
                [ "% SZS status Timeout for 1"
                , "% SZS status Theorem for 1"
                ]
        recognizeAnswer vampireProver directTask out ""
            `shouldBe` Uncertain
    ]

directTask :: Task
directTask = Task
    { taskDirectness = Direct
    , taskHypotheses = []
    , taskConjectureLabel = Marker "dummy"
    , taskLocation = Nowhere
    , taskConjecture = Top
    }

indirectTask :: Task
indirectTask = directTask{taskDirectness = Indirect Top}

vampireProver :: ProverInstance
vampireProver = vampire "vampire" Silent defaultTimeLimit defaultMemoryLimit
