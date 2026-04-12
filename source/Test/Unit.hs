module Test.Unit where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Unit.Checking qualified as Checking
import Test.Unit.Html qualified as Html
import Test.Unit.Provers qualified as Provers
import Test.Unit.Symdiff qualified as Symdiff
import Test.Unit.Syntax qualified as Syntax
import Test.Unit.Token qualified as Token

unitTests :: TestTree
unitTests = testGroup "unit tests"
    [testCase "filter" filtersWell
    , Checking.unitTests
    , Html.unitTests
    , Provers.unitTests
    , Syntax.unitTests  -- include the Syntax.DeBruijn tests
    , Token.unitTests
    ]


filtersWell :: Assertion
filtersWell = do
    assertBool "Filter works on symdiff problem" Symdiff.filtersWell
    assertBool "Filter handles struct/app syntax" Symdiff.handlesStructAndApply
