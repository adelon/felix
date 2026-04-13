{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Html (unitTests) where

import Base
import Api qualified
import Render.Html qualified as Html
import Report.Location (Location, mkLocation, pattern Nowhere, registerFilePath)
import Syntax.Abstract

import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "HTML renderer"
    [ testCase "reference previews are rendered for local and imported refs" referencePreviews
    , testCase "missing reference preview data falls back to readable text" missingReferenceFallback
    , testCase "datatype derived facts render as collapsed local reference targets" datatypeDerivedFactTargets
    , testCase "imported datatype derived facts get hidden preview templates" importedDatatypeDerivedFactPreviews
    ]

referencePreviews :: Assertion
referencePreviews = do
    html <- Api.exportHtml "test/html-fixtures/root-preview.tex"
    assertContains "local references keep page anchors" "href=\"#local_prop\"" html
    assertContains "local references point previews at the visible target block" "data-reference-label=\"local_prop\" data-preview-target-id=\"local_prop\"" html
    assertNotContains "local references do not use hidden preview ids" "data-reference-label=\"local_prop\" data-preview-id=\"" html
    assertContains "visible target blocks expose preview metadata" "id=\"local_prop\" data-preview-kind=\"Proposition\" data-preview-label=\"local_prop\"" html
    assertContains "imported references get preview metadata" "data-reference-label=\"imported_prop\"" html
    assertContains "imported references link to extensionless external theory routes" "href=\"/test/html-fixtures/imported-preview#imported_prop\"" html
    assertContains "imported references still use hidden preview templates" "data-reference-label=\"imported_prop\" data-preview-id=\"reference-preview-" html
    assertNotContains "imported references do not get broken page anchors" "href=\"#imported_prop\"" html
    assertNotContains "imported references are not non-clickable spans" "<span class=\"ref-badge has-preview\" data-reference-label=\"imported_prop\"" html
    assertCount "hidden preview store only contains the imported fixture preview" 1 "class=\"reference-preview-template\"" html
    assertContains "page grid is scoped to an explicit shell" "class=\"page-layout\"" html
    assertNotContains "page grid does not apply to every body div" "body > div" html
    assertContains "imported preview records its source file" "test/html-fixtures/imported-preview.tex" html
    assertContains "imported source renders on its own line" "class=\"reference-preview-source\"" html
    assertContains "multi-reference rendering preserves the comma separator" ", <a href=\"/test/html-fixtures/imported-preview#imported_prop\" class=\"ref-badge has-preview\" data-reference-label=\"imported_prop\"" html
    assertContains "calculation justifications also use target previews" "Step 2: by <a href=\"#local_prop\" class=\"ref-badge has-preview\" data-reference-label=\"local_prop\" data-preview-target-id=\"local_prop\"" html
    assertContains "large reference lists collapse to an ellipsis trigger" "Follows by <span class=\"ref-badge has-preview ref-badge-group\" data-preview-group=\"true\" data-reference-label=\"5 references\"" html
    assertContains "collapsed references keep current-page item metadata" "data-reference-label=\"group_source\" data-preview-link=\"#group_source\" data-preview-target-id=\"group_source\"" html
    assertContains "collapsed references keep imported item metadata" "data-reference-label=\"imported_prop\" data-preview-link=\"/test/html-fixtures/imported-preview#imported_prop\" data-preview-id=\"reference-preview-" html
    assertNotContains "collapsed references do not inline the long list" "Follows by <a href=\"#local_prop\" class=\"ref-badge has-preview\" data-reference-label=\"local_prop\" data-preview-target-id=\"local_prop\" aria-describedby=\"reference-preview-popup\">local_prop</a>, <a href=\"#uses_refs\"" html
    assertContains "collapsed tooltips compose full preview templates" "const preview = cloneHiddenPreview(item) || buildCurrentPreview(item) || buildMissingPreview(item);" html
    assertContains "collapsed tooltip labels become links" "template.append(linkGroupHeading(item, preview));" html
    assertContains "collapsed tooltip labels use generated reference links" "link.href = href;" html
    assertContains "collapsed tooltip heading links have hover affordance" ".reference-preview-heading a:hover," html
    assertContains "collapsed tooltips use stacked preview sections" "className = 'reference-preview-group-template'" html
    assertContains "visible preview popup accepts pointer interaction" "pointer-events: auto;" html
    assertContains "preview popup uses a wider bounded layout" "width: 44rem;" html
    assertContains "preview popup uses a taller bounded layout" "max-height: min(34rem, calc(100vh - 2rem));" html
    assertContains "preview popup cancels delayed hide on pointer entry" "popup.addEventListener('pointerenter', clearHideTimer);" html
    assertContains "preview popup schedules delayed hide on pointer exit" "popup.addEventListener('pointerleave', scheduleHide);" html
    assertContains "group click pins the preview popup" "showPreview(trigger, event, true);" html
    assertContains "group keyboard activation pins the preview popup" "showPreview(trigger, null, true);" html
    assertContains "preview statements use a full-width paragraph" "class=\"reference-preview-statement\"" html
    assertContains "preview popup is emitted once" "id=\"reference-preview-popup\"" html

missingReferenceFallback :: Assertion
missingReferenceFallback = do
    let proof = Qed (Just Nowhere) (JustificationRef ("missing_ref" :| []))
        html = Html.renderDocument "synthetic.tex" "" [BlockProof Nowhere proof Nowhere] []
    assertContains "missing references remain visible" "missing_ref" html
    assertNotContains "missing references do not claim preview content" "data-preview-id=" html

datatypeDerivedFactTargets :: Assertion
datatypeDerivedFactTargets = do
    let blocks =
            [ propformDatatypeBlock Nowhere
            , referenceClaimBlock "uses_local_datatype_fact"
            , referenceProofBlock "propform_propbot_intro"
            ]
        html = Html.renderDocument "synthetic.tex" "" blocks []
    assertContains "datatype blocks render derived facts inside a details element" "<summary>Derived facts</summary>" html
    assertContains "local datatype fact refs keep page anchors" "href=\"#propform_propbot_intro\"" html
    assertContains "local datatype fact refs point previews at the derived fact target" "data-reference-label=\"propform_propbot_intro\" data-preview-target-id=\"propform_propbot_intro\"" html
    assertContains "derived fact targets expose preview metadata" "id=\"propform_propbot_intro\" data-preview-kind=\"Datatype Fact\" data-preview-label=\"propform_propbot_intro\"" html
    assertContains "local hash navigation opens collapsed ancestors before scrolling" "revealTarget(target);" html

importedDatatypeDerivedFactPreviews :: Assertion
importedDatatypeDerivedFactPreviews = do
    importedLoc <- fileLocation "test/html-fixtures/imported-datatype.tex"
    let rootBlocks =
            [ referenceClaimBlock "uses_imported_datatype_fact"
            , referenceProofBlock "propform_propbot_intro"
            ]
        html = Html.renderDocument "test/html-fixtures/root-datatype.tex" "" rootBlocks [propformDatatypeBlock importedLoc]
    assertContains "imported datatype fact refs link to the imported theory route" "href=\"/test/html-fixtures/imported-datatype#propform_propbot_intro\"" html
    assertContains "imported datatype fact refs use hidden preview ids" "data-reference-label=\"propform_propbot_intro\" data-preview-id=\"reference-preview-" html
    assertContains "imported datatype fact previews identify their kind" "Datatype Fact <code>propform_propbot_intro</code>" html
    assertContains "imported datatype fact previews record their source file" "test/html-fixtures/imported-datatype.tex" html

fileLocation :: FilePath -> IO Location
fileLocation path = do
    fileId <- registerFilePath path
    pure (mkLocation fileId 1 1)

propformDatatypeBlock :: Location -> Block
propformDatatypeBlock blockLoc =
    BlockData blockLoc Nothing "propform" propformDatatype

propformDatatype :: Datatype
propformDatatype =
    Datatype
        { datatypeHeadExpr = ExprOp Nowhere (constSymbol "propform") []
        , datatypeClauses =
            DatatypeClause (ExprOp Nowhere (constSymbol "propbot") []) (ExprOp Nowhere (constSymbol "propform") []) [] :|
                [ DatatypeClause (ExprOp Nowhere (unarySymbol "propvar") [ExprVar "n"]) (ExprOp Nowhere (constSymbol "propform") []) [("n", ExprOp Nowhere (constSymbol "naturals") [])]
                , DatatypeClause
                    (ExprOp Nowhere (infixSymbol "propto") [ExprVar "p", ExprVar "q"])
                    (ExprOp Nowhere (constSymbol "propform") [])
                    [ ("p", ExprOp Nowhere (constSymbol "propform") [])
                    , ("q", ExprOp Nowhere (constSymbol "propform") [])
                    ]
                ]
        }

referenceClaimBlock :: Marker -> Block
referenceClaimBlock marker =
    BlockClaim Proposition Nowhere Nothing marker (Claim [] (StmtFormula (PropositionalConstant Nowhere IsTop)))

referenceProofBlock :: Marker -> Block
referenceProofBlock marker =
    BlockProof Nowhere (Qed (Just Nowhere) (JustificationRef (marker :| []))) Nowhere

constSymbol :: Text -> FunctionSymbol
constSymbol name =
    mkMixfixItem [Just (Command name)] (Marker name) NonAssoc

unarySymbol :: Text -> FunctionSymbol
unarySymbol name =
    mkMixfixItem [Just (Command name), Just InvisibleBraceL, Nothing, Just InvisibleBraceR] (Marker name) NonAssoc

infixSymbol :: Text -> FunctionSymbol
infixSymbol name =
    mkMixfixItem [Nothing, Just (Command name), Nothing] (Marker name) NonAssoc

assertContains :: HasCallStack => String -> Text -> Text -> Assertion
assertContains label needle haystack =
    assertBool
        (label <> "\nExpected to find: " <> Text.unpack needle)
        (needle `Text.isInfixOf` haystack)

assertNotContains :: HasCallStack => String -> Text -> Text -> Assertion
assertNotContains label needle haystack =
    assertBool
        (label <> "\nDid not expect to find: " <> Text.unpack needle)
        (not (needle `Text.isInfixOf` haystack))

assertCount :: HasCallStack => String -> Int -> Text -> Text -> Assertion
assertCount label expected needle haystack =
    assertEqual
        (label <> "\nExpected count for: " <> Text.unpack needle)
        expected
        (Text.count needle haystack)
