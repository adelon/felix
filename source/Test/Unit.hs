module Test.Unit where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Unit.Provers qualified as Provers
import Test.Unit.Symdiff qualified as Symdiff
import Test.Unit.Syntax qualified as Syntax

unitTests :: TestTree
unitTests = testGroup "unit tests"
    [testCase "filter" filtersWell
    , Provers.unitTests
    , Syntax.unitTests  -- include the Syntax.DeBruijn tests
    ]


filtersWell :: Assertion
filtersWell = do
    assertBool "Filter works on symdiff problem" Symdiff.filtersWell
