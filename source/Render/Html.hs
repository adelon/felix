{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Render.Html
    ( renderDocument
    ) where

import Syntax.Abstract

import Base
import Lucid hiding (Term, for_)
import Lucid.Math

import Control.Monad (guard, unless, when)
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace, toUpper)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Report.Location (pattern Nowhere)
import Syntax.Token (tokToText)


data HintCategory
    = OperatorHint
    | RelationHint
    | PredicateHint
    | StructOpHint
    deriving (Show, Eq, Ord)

data TemplatePiece
    = Literal Text
    | Slot Int
    deriving (Show, Eq, Ord)

data RenderHint = RenderHint
    { renderHintArity :: Int
    , renderHintTemplate :: [TemplatePiece]
    } deriving (Show, Eq, Ord)

type HintMap = Map (HintCategory, Marker) RenderHint

proofCollapseThreshold :: Int
proofCollapseThreshold = 10


renderDocument :: FilePath -> Text -> [Block] -> Text
renderDocument inputPath hintsSource blocks =
    LazyText.toStrict (renderText (renderPage hints))
    where
        hints = parseHints hintsSource
        indexedBlocks = zip [1 :: Int ..] blocks
        tocBlocks = [(index, block) | (index, block) <- indexedBlocks, includeInToc block]

        renderPage :: HintMap -> Html ()
        renderPage hintMap = doctypehtml_ do
            head_ do
                meta_ [charset_ "utf-8"]
                title_ (toHtml (Text.pack inputPath))
                style_ pageStyles
            body_ do
                div_ [class_ "layout"] do
                    aside_ [class_ "toc-column"] do
                        nav_ [class_ "toc"] do
                            h2_ [class_ "toc-heading"] "Contents"
                            ol_ [class_ "toc-list"] do
                                traverse_ (uncurry renderTocEntry) tocBlocks
                    main_ [class_ "content"] do
                        h1_ (toHtml (Text.pack inputPath))
                        traverse_ (uncurry (renderBlock hintMap)) indexedBlocks


pageStyles :: Text
pageStyles = Text.unlines
    [ ":root {"
    , "  color-scheme: light dark;"
    , "  font-family: Georgia, \"Times New Roman\", serif;"
    , "  --page-bg: #ffffff;"
    , "  --page-fg: #111111;"
    , "  --muted-fg: #666666;"
    , "  --subtle-fg: #444444;"
    , "  --badge-bg: #f1f1f1;"
    , "  --badge-border: #dddddd;"
    , "  --badge-fg: #555555;"
    , "  --rule-color: #d9d2c2;"
    , "  --error-fg: #9f1d1d;"
    , "  --error-bg: #fff1f1;"
    , "}"
    , "html {"
    , "  height: 100%;"
    , "}"
    , "body {"
    , "  margin: 0;"
    , "  height: 100vh;"
    , "  overflow: hidden;"
    , "  line-height: 1.5;"
    , "  background: var(--page-bg);"
    , "  color: var(--page-fg);"
    , "}"
    , ".layout {"
    , "  display: grid;"
    , "  grid-template-columns: minmax(14rem, 18rem) minmax(0, 1fr);"
    , "  grid-template-rows: minmax(0, 1fr);"
    , "  gap: 2rem;"
    , "  align-items: stretch;"
    , "  box-sizing: border-box;"
    , "  margin: 0 auto;"
    , "  max-width: 84rem;"
    , "  height: 100vh;"
    , "  padding: 2rem 1.25rem 3rem;"
    , "}"
    , ".toc-column {"
    , "  display: block;"
    , "  min-height: 0;"
    , "}"
    , ".toc {"
    , "  height: 100%;"
    , "  overflow-y: auto;"
    , "  padding-right: 0.5rem;"
    , "}"
    , ".toc-heading {"
    , "  margin: 0 0 0.75rem;"
    , "  color: var(--muted-fg);"
    , "  font-size: 0.9rem;"
    , "  letter-spacing: 0.04em;"
    , "  text-transform: uppercase;"
    , "}"
    , ".toc-list {"
    , "  list-style: none;"
    , "  margin: 0;"
    , "  padding: 0;"
    , "}"
    , ".toc-item {"
    , "  margin: 0 0 0.45rem;"
    , "}"
    , ".toc-link {"
    , "  color: inherit;"
    , "  text-decoration: none;"
    , "}"
    , ".toc-link:hover,"
    , ".toc-link:focus-visible {"
    , "  text-decoration: underline;"
    , "}"
    , ".toc-prefix {"
    , "  font-weight: 700;"
    , "}"
    , ".toc-marker {"
    , "  margin-left: 0.35rem;"
    , "  color: var(--muted-fg);"
    , "  font-family: \"SFMono-Regular\", Menlo, Consolas, \"Liberation Mono\", monospace;"
    , "  font-size: 0.9em;"
    , "}"
    , ".toc-title {"
    , "  color: var(--subtle-fg);"
    , "}"
    , ".content {"
    , "  display: block;"
    , "  min-width: 0;"
    , "  min-height: 0;"
    , "  overflow-y: auto;"
    , "}"
    , ".block {"
    , "  display: block;"
    , "  margin: 0 0 1rem;"
    , "  scroll-margin-top: 1rem;"
    , "}"
    , ".block-header {"
    , "  font-weight: 700;"
    , "}"
    , ".block-title {"
    , "  font-weight: 400;"
    , "}"
    , ".block-marker {"
    , "  display: inline-block;"
    , "  margin-left: 0.45rem;"
    , "  padding: 0.02rem 0.35rem;"
    , "  border: 1px solid var(--badge-border);"
    , "  border-radius: 0.2rem;"
    , "  background: var(--badge-bg);"
    , "  color: var(--badge-fg);"
    , "  font-family: \"SFMono-Regular\", Menlo, Consolas, \"Liberation Mono\", monospace;"
    , "  font-size: 0.82em;"
    , "}"
    , ".block-body,"
    , ".proof-body {"
    , "  display: contents;"
    , "}"
    , ".proof-body > p:first-child {"
    , "  display: inline;"
    , "  margin: 0;"
    , "}"
    , ".proof-nested {"
    , "  margin: 0.5rem 0 0.5rem 1rem;"
    , "  padding-left: 0.75rem;"
    , "  border-left: 1px solid var(--rule-color);"
    , "}"
    , ".proof-details {"
    , "  margin: 0;"
    , "}"
    , ".proof-details > summary {"
    , "  cursor: pointer;"
    , "  font-weight: 700;"
    , "}"
    , ".proof-details > summary .block-title {"
    , "  font-weight: 400;"
    , "}"
    , ".proof-details > :not(summary) {"
    , "  margin-top: 0.5rem;"
    , "}"
    , ".math-block {"
    , "  margin: 0.5rem 0;"
    , "}"
    , "merror {"
    , "  color: var(--error-fg);"
    , "  background: var(--error-bg);"
    , "}"
    , "@media (prefers-color-scheme: dark) {"
    , "  :root {"
    , "    --page-bg: #161616;"
    , "    --page-fg: #e9e6df;"
    , "    --muted-fg: #b7b0a4;"
    , "    --subtle-fg: #cfc8bc;"
    , "    --badge-bg: #2a2a2a;"
    , "    --badge-border: #444444;"
    , "    --badge-fg: #d8d3ca;"
    , "    --rule-color: #5b5348;"
    , "    --error-fg: #ffb0b0;"
    , "    --error-bg: #3b1f1f;"
    , "  }"
    , "}"
    , "@media (max-width: 900px) {"
    , "  body {"
    , "    height: auto;"
    , "    overflow: auto;"
    , "  }"
    , "  .layout {"
    , "    grid-template-columns: 1fr;"
    , "    grid-template-rows: auto;"
    , "    gap: 1.5rem;"
    , "    height: auto;"
    , "  }"
    , "  .toc-column {"
    , "    display: none;"
    , "  }"
    , "  .content {"
    , "    min-height: auto;"
    , "    overflow: visible;"
    , "  }"
    , "}"
    ]


parseHints :: Text -> HintMap
parseHints source = Map.fromList (parseLine <$> zip [1 :: Int ..] relevantLines)
    where
        relevantLines = [line | line <- Text.lines source, not (Text.all isSpace line)]

        parseLine :: (Int, Text) -> ((HintCategory, Marker), RenderHint)
        parseLine (lineNo, line) = case Text.splitOn "\t" line of
            [categoryText, markerName, arityText, templateText] ->
                let category = parseCategory lineNo categoryText
                    marker = Marker markerName
                    arity = parseArity lineNo arityText
                    template = parseTemplate lineNo templateText
                in ((category, marker), RenderHint arity template)
            _ ->
                error ("Malformed render hint at line " <> show lineNo <> ": expected exactly 4 tab-separated columns")

parseCategory :: Int -> Text -> HintCategory
parseCategory lineNo = \case
    "operator" -> OperatorHint
    "relation" -> RelationHint
    "predicate" -> PredicateHint
    "structop" -> StructOpHint
    other -> error ("Unknown render hint category at line " <> show lineNo <> ": " <> Text.unpack other)

parseArity :: Int -> Text -> Int
parseArity lineNo text = case reads (Text.unpack text) of
    [(n, "")] -> n
    _ -> error ("Malformed render-hint arity at line " <> show lineNo <> ": " <> Text.unpack text)

parseTemplate :: Int -> Text -> [TemplatePiece]
parseTemplate lineNo template = reverse (flush mempty (go mempty [] template))
    where
        go :: Text -> [TemplatePiece] -> Text -> [TemplatePiece]
        go literal acc rest = case parseSlot rest of
            Just (slot, rest') ->
                go mempty (Slot slot : flush literal acc) rest'
            Nothing -> case Text.uncons rest of
                Nothing -> flush literal acc
                Just (c, rest') -> go (Text.snoc literal c) acc rest'

        flush :: Text -> [TemplatePiece] -> [TemplatePiece]
        flush literal acc
            | Text.null literal = acc
            | otherwise = Literal literal : acc

        parseSlot :: Text -> Maybe (Int, Text)
        parseSlot text = do
            text' <- Text.stripPrefix "<x" text
            (digit, rest) <- Text.uncons text'
            guard (isDigit digit)
            rest' <- Text.stripPrefix "/>" rest
            let slot = digitToInt digit
            guard (slot > 0 && slot <= 9)
            pure (slot, rest')

        _unusedLineNo = lineNo


renderBlock :: HintMap -> Int -> Block -> Html ()
renderBlock hints index block = case block of
    BlockAxiom _loc title marker axiom ->
        renderCustomBlock (blockAnchorId index block) "axiom-block" "Axiom" (Just marker) title (renderAxiom hints axiom)
    BlockClaim kind _loc title marker claim ->
        renderCustomBlock (blockAnchorId index block) (claimKindElement kind) (claimKindPrefix kind) (Just marker) title (renderClaim hints claim)
    BlockProof _start proof _end ->
        renderProofBlock (blockAnchorId index block) hints proof
    BlockDefn _loc title marker defn ->
        renderCustomBlock (blockAnchorId index block) "definition-block" "Definition" (Just marker) title (renderDefn hints defn)
    BlockAbbr _loc title marker abbr ->
        renderCustomBlock (blockAnchorId index block) "abbreviation-block" "Abbreviation" (Just marker) title (renderAbbreviation hints abbr)
    BlockData _loc datatype ->
        renderCustomBlock (blockAnchorId index block) "datatype-block" "Datatype" Nothing Nothing (renderDatatype hints datatype)
    BlockInductive _loc title marker ind ->
        renderCustomBlock (blockAnchorId index block) "inductive-block" "Inductive" (Just marker) title (renderInductive hints ind)
    BlockSig _loc title marker asms sig ->
        renderCustomBlock (blockAnchorId index block) "signature-block" "Signature" (Just marker) title (renderSignatureBlock hints asms sig)
    BlockStruct _loc title marker structDefn ->
        renderCustomBlock (blockAnchorId index block) "struct-block" "Structure" (Just marker) title (renderStructDefn hints structDefn)

renderTocEntry :: Int -> Block -> Html ()
renderTocEntry index block =
    li_ [class_ "toc-item"] do
        a_ [class_ "toc-link", href_ ("#" <> blockAnchorId index block)] do
            span_ [class_ "toc-prefix"] (toHtml (blockPrefixText block))
            case formatMarker (blockMarkerOf block) of
                Nothing -> skip
                Just marker ->
                    span_ [class_ "toc-marker"] (toHtml marker)
            case formatBlockTitle (blockTitleOf block) of
                Nothing ->
                    when (blockNeedsIndexLabel block) do
                        toHtml (" " <> Text.pack (show index) :: Text)
                Just title ->
                    span_ [class_ "toc-title"] (toHtml (" (" <> title <> ")" :: Text))

includeInToc :: Block -> Bool
includeInToc = \case
    BlockProof{} -> False
    _ -> True

renderCustomBlock :: Text -> Text -> Text -> Maybe Marker -> Maybe BlockTitle -> Html () -> Html ()
renderCustomBlock blockId name prefix mmarker mtitle body =
    term name [id_ blockId, class_ "block"] do
        span_ [class_ "block-header"] (renderBlockLead prefix mmarker mtitle True)
        div_ [class_ "block-body"] body

renderProofBlock :: Text -> HintMap -> Proof -> Html ()
renderProofBlock blockId hints proof
    | proofStepCount proof >= proofCollapseThreshold =
        term "proof-block" [id_ blockId, class_ "block"] do
            details_ [class_ "proof-details"] do
                summary_ (renderBlockLead "Proof" Nothing Nothing False)
                div_ [class_ "proof-body"] (renderProof hints proof)
    | otherwise =
        renderCustomBlock blockId "proof-block" "Proof" Nothing Nothing (div_ [class_ "proof-body"] (renderProof hints proof))

blockAnchorId :: Int -> Block -> Text
blockAnchorId index block =
    "block-" <> Text.pack (show index) <> "-" <> sanitizeIdFragment raw
    where
        raw = case formatMarker (blockMarkerOf block) of
            Nothing -> blockPrefixText block
            Just marker -> marker

sanitizeIdFragment :: Text -> Text
sanitizeIdFragment =
    Text.dropWhile (== '-') . Text.map sanitize . Text.toLower
    where
        sanitize c
            | isAlphaNum c = c
            | c == '-' || c == '_' = c
            | otherwise = '-'

blockPrefixText :: Block -> Text
blockPrefixText = \case
    BlockAxiom{} -> "Axiom"
    BlockClaim kind _ _ _ _ -> claimKindPrefix kind
    BlockProof{} -> "Proof"
    BlockDefn{} -> "Definition"
    BlockAbbr{} -> "Abbreviation"
    BlockData{} -> "Datatype"
    BlockInductive{} -> "Inductive"
    BlockSig{} -> "Signature"
    BlockStruct{} -> "Structure"

blockMarkerOf :: Block -> Maybe Marker
blockMarkerOf = \case
    BlockAxiom _ _ marker _ -> Just marker
    BlockClaim _ _ _ marker _ -> Just marker
    BlockProof{} -> Nothing
    BlockDefn _ _ marker _ -> Just marker
    BlockAbbr _ _ marker _ -> Just marker
    BlockData{} -> Nothing
    BlockInductive _ _ marker _ -> Just marker
    BlockSig _ _ marker _ _ -> Just marker
    BlockStruct _ _ marker _ -> Just marker

blockTitleOf :: Block -> Maybe BlockTitle
blockTitleOf = \case
    BlockAxiom _ title _ _ -> title
    BlockClaim _ _ title _ _ -> title
    BlockProof{} -> Nothing
    BlockDefn _ title _ _ -> title
    BlockAbbr _ title _ _ -> title
    BlockData{} -> Nothing
    BlockInductive _ title _ _ -> title
    BlockSig _ title _ _ _ -> title
    BlockStruct _ title _ _ -> title

blockNeedsIndexLabel :: Block -> Bool
blockNeedsIndexLabel block = case (formatMarker (blockMarkerOf block), formatBlockTitle (blockTitleOf block)) of
    (Nothing, Nothing) -> True
    _ -> False

renderBlockLead :: Text -> Maybe Marker -> Maybe BlockTitle -> Bool -> Html ()
renderBlockLead prefix mmarker mtitle withTrailingSpace = do
    span_ [class_ "block-prefix"] (toHtml prefix)
    case formatMarker mmarker of
        Nothing -> skip
        Just marker ->
            span_ [class_ "block-marker"] (toHtml marker)
    case formatBlockTitle mtitle of
        Nothing ->
            toHtml ("." <> suffix)
        Just title -> do
            toHtml (" (" :: Text)
            span_ [class_ "block-title"] (toHtml title)
            toHtml (")." <> suffix)
    where
        suffix :: Text
        suffix
            | withTrailingSpace = " "
            | otherwise = ""

formatMarker :: Maybe Marker -> Maybe Text
formatMarker = \case
    Nothing -> Nothing
    Just marker ->
        let text = Text.strip (markerText marker)
        in if Text.null text then Nothing else Just text

formatBlockTitle :: Maybe BlockTitle -> Maybe Text
formatBlockTitle =
    fmap capitalizeTitle . nonEmptyTitle
    where
        nonEmptyTitle = \case
            Nothing -> Nothing
            Just toks ->
                let text = Text.strip (Text.unwords (tokToText <$> toks))
                in if Text.null text then Nothing else Just text

capitalizeTitle :: Text -> Text
capitalizeTitle text = case Text.uncons text of
    Nothing -> text
    Just (c, rest) -> Text.cons (toUpper c) rest

claimKindElement :: ClaimKind -> Text
claimKindElement = \case
    Proposition -> "proposition-block"
    Theorem -> "theorem-block"
    Lemma -> "lemma-block"
    Corollary -> "corollary-block"
    PlainClaim -> "claim-block"

claimKindPrefix :: ClaimKind -> Text
claimKindPrefix = \case
    Proposition -> "Proposition"
    Theorem -> "Theorem"
    Lemma -> "Lemma"
    Corollary -> "Corollary"
    PlainClaim -> "Claim"


renderAxiom :: HintMap -> Axiom -> Html ()
renderAxiom hints (Axiom asms stmt) =
    renderWithAssumptions hints asms stmt

renderClaim :: HintMap -> Claim -> Html ()
renderClaim hints (Claim asms stmt) =
    renderWithAssumptions hints asms stmt

renderWithAssumptions :: HintMap -> [Asm] -> Stmt -> Html ()
renderWithAssumptions hints asms stmt = do
    case asms of
        [] -> renderStmtInline hints stmt
        _ -> do
            toHtml ("If " :: Text)
            renderAsmList hints asms
            toHtml (", then " :: Text)
            renderStmtInline hints stmt
    toHtml ("." :: Text)

renderDefn :: HintMap -> Defn -> Html ()
renderDefn hints = \case
    Defn asms headStmt stmt ->
        do
            when (not (null asms)) do
                toHtml ("If " :: Text)
                renderAsmList hints asms
                toHtml (", then " :: Text)
            renderDefnHead hints headStmt
            toHtml (" iff " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
    DefnFun asms fun maybeSymbol resultTerm ->
        do
            when (not (null asms)) do
                toHtml ("If " :: Text)
                renderAsmList hints asms
                toHtml (", then " :: Text)
            renderFunInline renderVarInline fun
            case maybeSymbol of
                Nothing -> skip
                Just symbolicTerm -> do
                    toHtml (", " :: Text)
                    renderTermInline hints symbolicTerm
            toHtml (" is " :: Text)
            renderTermInline hints resultTerm
            toHtml ("." :: Text)
    DefnOp symb expr ->
        do
            renderSymbolPatternInline hints symb
            toHtml (" is defined as " :: Text)
            inlineMath (renderExprMath hints expr)
            toHtml ("." :: Text)

renderDefnHead :: HintMap -> DefnHead -> Html ()
renderDefnHead hints = \case
    DefnAdj maybeNp var adj -> do
        renderTypedVar hints maybeNp var
        toHtml (" is " :: Text)
        renderAdjInline renderVarInline adj
    DefnVerb maybeNp var verb -> do
        renderTypedVar hints maybeNp var
        toHtml (" " :: Text)
        renderVerbInline False renderVarInline verb
    DefnNoun var noun -> do
        renderVarInline var
        toHtml (" is a " :: Text)
        renderNounInline False renderVarInline noun
    DefnSymbolicPredicate predi vars ->
        inlineMath (renderPrefixPredicateFallback predi (renderVarMath <$> toList vars))
    DefnRel x rel params y ->
        inlineMath (renderRelationApplication hints Positive [ExprVar x] (Relation Nowhere rel [ExprVar p | p <- params]) [ExprVar y])

renderTypedVar :: HintMap -> Maybe (NounPhrase Maybe) -> VarSymbol -> Html ()
renderTypedVar hints = \case
    Nothing -> renderVarInline
    Just np -> \var -> do
        renderNounPhraseMaybe hints np
        toHtml (" " :: Text)
        renderVarInline var


renderAbbreviation :: HintMap -> Abbreviation -> Html ()
renderAbbreviation hints = \case
    AbbreviationAdj var adj stmt ->
        do
            renderVarInline var
            toHtml (" is " :: Text)
            renderAdjInline renderVarInline adj
            toHtml (" stands for " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
    AbbreviationVerb var verb stmt ->
        do
            renderVarInline var
            toHtml (" " :: Text)
            renderVerbInline False renderVarInline verb
            toHtml (" stands for " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
    AbbreviationNoun var noun stmt ->
        do
            renderVarInline var
            toHtml (" is a " :: Text)
            renderNounInline False renderVarInline noun
            toHtml (" stands for " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
    AbbreviationRel x rel params y stmt ->
        do
            inlineMath (renderRelationApplication hints Positive [ExprVar x] (Relation Nowhere rel [ExprVar p | p <- params]) [ExprVar y])
            toHtml (" stands for " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
    AbbreviationFun fun bodyTerm ->
        do
            renderFunInline renderVarInline fun
            toHtml (" stands for " :: Text)
            renderTermInline hints bodyTerm
            toHtml ("." :: Text)
    AbbreviationEq symb expr ->
        do
            renderSymbolPatternInline hints symb
            toHtml (" stands for " :: Text)
            inlineMath (renderExprMath hints expr)
            toHtml ("." :: Text)

renderDatatype :: HintMap -> Datatype -> Html ()
renderDatatype hints = \case
    DatatypeFin noun labels -> do
        toHtml ("Finite datatype of " :: Text)
        renderNounInline False (renderTermInline hints) noun
        toHtml ("." :: Text)
        p_ do
            toHtml ("Constructors: " :: Text)
            toHtml (Text.intercalate ", " (toList labels))
            toHtml ("." :: Text)

renderInductive :: HintMap -> Inductive -> Html ()
renderInductive hints Inductive{..} = do
    toHtml ("Inductive definition of " :: Text)
    renderSymbolPatternInline hints inductiveSymbolPattern
    toHtml (" over " :: Text)
    inlineMath (renderExprMath hints inductiveDomain)
    toHtml ("." :: Text)
    ul_ do
        traverse_ renderIntro (toList inductiveIntros)
    where
        renderIntro :: IntroRule -> Html ()
        renderIntro IntroRule{..} = li_ do
            case introConditions of
                [] -> skip
                _ -> do
                    toHtml ("If " :: Text)
                    joinHtml (toHtml (" and " :: Text)) (inlineMath . renderFormulaMath hints <$> introConditions)
                    toHtml (", then " :: Text)
            inlineMath (renderFormulaMath hints introResult)
            toHtml ("." :: Text)

renderSignatureBlock :: HintMap -> [Asm] -> Signature -> Html ()
renderSignatureBlock hints asms sig = do
    case asms of
        [] -> skip
        _ -> do
            toHtml ("Assumptions: " :: Text)
            renderAsmList hints asms
            toHtml ("." :: Text)
    when (not (null asms)) do
        p_ do
            renderSignature hints sig
            toHtml ("." :: Text)
    when (null asms) do
        renderSignature hints sig
        toHtml ("." :: Text)

renderSignature :: HintMap -> Signature -> Html ()
renderSignature hints = \case
    SignatureAdj var adj -> do
        renderVarInline var
        toHtml (" can be " :: Text)
        renderAdjInline renderVarInline adj
    SignatureVerb var verb -> do
        renderVarInline var
        toHtml (" can " :: Text)
        renderVerbInline False renderVarInline verb
    SignatureNoun var noun -> do
        renderVarInline var
        toHtml (" is a " :: Text)
        renderNounInline False renderVarInline noun
    SignatureSymbolic symb np -> do
        renderSymbolPatternInline hints symb
        toHtml (" is a " :: Text)
        renderNounPhraseMaybe hints np

renderStructDefn :: HintMap -> StructDefn -> Html ()
renderStructDefn hints StructDefn{..} = do
    toHtml ("Structure phrase: " :: Text)
    renderStructPhraseInline structPhrase
    toHtml ("." :: Text)
    p_ do
        toHtml ("Label: " :: Text)
        renderVarInline structLabel
        toHtml ("." :: Text)
    when (not (null structParents)) do
        p_ do
            toHtml ("Parents: " :: Text)
            joinHtml (toHtml (", " :: Text)) (renderStructPhraseInline <$> structParents)
            toHtml ("." :: Text)
    when (not (null structFixes)) do
        p_ do
            toHtml ("Fixes: " :: Text)
            joinHtml (toHtml (", " :: Text)) (inlineMath . renderStructSymbolName <$> structFixes)
            toHtml ("." :: Text)
    when (not (null structAssumes)) do
        ul_ do
            for_ structAssumes \(marker, stmt) -> li_ do
                toHtml (markerText marker)
                toHtml (": " :: Text)
                renderStmtInline hints stmt
                toHtml ("." :: Text)


renderProof :: HintMap -> Proof -> Html ()
renderProof hints = \case
    Omitted ->
        p_ "Proof omitted."
    Qed _loc justification ->
        p_ do
            toHtml ("Qed" :: Text)
            renderJustificationSuffix justification
            toHtml ("." :: Text)
    ByCase _loc cases -> do
        p_ "Proof by cases."
        div_ [class_ "proof-nested"] (traverse_ (renderCase hints) cases)
    ByContradiction _loc proof -> do
        p_ "Proof by contradiction."
        div_ [class_ "proof-nested"] (renderProof hints proof)
    BySetInduction _loc maybeTerm proof -> do
        p_ do
            toHtml ("Proof by set induction" :: Text)
            case maybeTerm of
                Nothing -> skip
                Just targetTerm -> do
                    toHtml (" on " :: Text)
                    renderTermInline hints targetTerm
            toHtml ("." :: Text)
        div_ [class_ "proof-nested"] (renderProof hints proof)
    ByOrdInduction _loc proof -> do
        p_ "Proof by ordinal induction."
        div_ [class_ "proof-nested"] (renderProof hints proof)
    Assume _loc stmt proof -> do
        p_ do
            toHtml ("Assume " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
        renderProof hints proof
    FixSymbolic _loc vars bound proof -> do
        p_ do
            toHtml ("Fix " :: Text)
            renderVarListInline vars
            renderBoundInline hints vars bound
            toHtml ("." :: Text)
        renderProof hints proof
    FixSuchThat _loc vars stmt proof -> do
        p_ do
            toHtml ("Fix " :: Text)
            renderVarListInline vars
            toHtml (" such that " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
        renderProof hints proof
    Calc _loc maybeQuant calc proof -> do
        renderCalc hints maybeQuant calc
        renderProof hints proof
    TakeVar _loc vars bound stmt justification proof -> do
        p_ do
            toHtml ("Take " :: Text)
            renderVarListInline vars
            renderBoundInline hints vars bound
            toHtml (" such that " :: Text)
            renderStmtInline hints stmt
            renderJustificationSuffix justification
            toHtml ("." :: Text)
        renderProof hints proof
    TakeNoun _loc np justification proof -> do
        p_ do
            toHtml ("Take " :: Text)
            renderNounPhraseList hints np
            renderJustificationSuffix justification
            toHtml ("." :: Text)
        renderProof hints proof
    Have _loc maybeStmt stmt justification proof -> do
        p_ do
            case maybeStmt of
                Nothing -> toHtml ("We have " :: Text)
                Just premise -> do
                    toHtml ("Since " :: Text)
                    renderStmtInline hints premise
                    toHtml (", we have " :: Text)
            renderStmtInline hints stmt
            renderJustificationSuffix justification
            toHtml ("." :: Text)
        renderProof hints proof
    Suffices _loc stmt justification proof -> do
        p_ do
            toHtml ("It suffices to show that " :: Text)
            renderStmtInline hints stmt
            renderJustificationSuffix justification
            toHtml ("." :: Text)
        renderProof hints proof
    Subclaim _loc stmt subproof proof -> do
        p_ do
            toHtml ("Show " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
        div_ [class_ "proof-nested"] (renderProof hints subproof)
        renderProof hints proof
    Define _loc var expr proof -> do
        p_ do
            toHtml ("Let " :: Text)
            renderVarInline var
            toHtml (" = " :: Text)
            inlineMath (renderExprMath hints expr)
            toHtml ("." :: Text)
        renderProof hints proof
    DefineFunction _loc fun arg value boundVar boundExpr proof -> do
        p_ do
            toHtml ("Let " :: Text)
            renderVarInline fun
            toHtml ("(" :: Text)
            renderVarInline arg
            toHtml (") = " :: Text)
            inlineMath (renderExprMath hints value)
            toHtml (" for " :: Text)
            renderVarInline boundVar
            toHtml (" in " :: Text)
            inlineMath (renderExprMath hints boundExpr)
            toHtml ("." :: Text)
        renderProof hints proof
    DefineFunctionLocal _loc fun arg _target domVar codVar rules proof -> do
        p_ do
            toHtml ("Let " :: Text)
            renderVarInline fun
            toHtml ("(" :: Text)
            renderVarInline arg
            toHtml (") be locally defined from " :: Text)
            renderVarInline domVar
            toHtml (" to " :: Text)
            renderVarInline codVar
            toHtml ("." :: Text)
        ul_ do
            for_ (toList rules) \(ruleTerm, formula) -> li_ do
                inlineMath (renderExprMath hints ruleTerm)
                toHtml (" if " :: Text)
                inlineMath (renderFormulaMath hints formula)
                toHtml ("." :: Text)
        renderProof hints proof

proofStepCount :: Proof -> Int
proofStepCount = \case
    Omitted -> 1
    Qed{} -> 1
    ByCase _loc cases -> 1 + sum (caseStepCount <$> cases)
    ByContradiction _loc proof -> 1 + proofStepCount proof
    BySetInduction _loc _maybeTerm proof -> 1 + proofStepCount proof
    ByOrdInduction _loc proof -> 1 + proofStepCount proof
    Assume _loc _stmt proof -> 1 + proofStepCount proof
    FixSymbolic _loc _vars _bound proof -> 1 + proofStepCount proof
    FixSuchThat _loc _vars _stmt proof -> 1 + proofStepCount proof
    Calc _loc _maybeQuant calc proof -> 1 + calcStepCount calc + proofStepCount proof
    TakeVar _loc _vars _bound _stmt _justification proof -> 1 + proofStepCount proof
    TakeNoun _loc _np _justification proof -> 1 + proofStepCount proof
    Have _loc _maybeStmt _stmt _justification proof -> 1 + proofStepCount proof
    Suffices _loc _stmt _justification proof -> 1 + proofStepCount proof
    Subclaim _loc _stmt subproof proof -> 1 + proofStepCount subproof + proofStepCount proof
    Define _loc _var _expr proof -> 1 + proofStepCount proof
    DefineFunction _loc _fun _arg _value _boundVar _boundExpr proof -> 1 + proofStepCount proof
    DefineFunctionLocal _loc _fun _arg _target _domVar _codVar rules proof ->
        1 + length rules + proofStepCount proof

caseStepCount :: Case -> Int
caseStepCount Case{caseProof} = 1 + proofStepCount caseProof

calcStepCount :: Calc -> Int
calcStepCount = \case
    Equation _ steps -> length steps
    Biconditionals _ steps -> length steps

renderCase :: HintMap -> Case -> Html ()
renderCase hints Case{..} =
    div_ [class_ "proof-nested"] do
        p_ do
            toHtml ("Case " :: Text)
            renderStmtInline hints caseOf
            toHtml ("." :: Text)
        renderProof hints caseProof

renderCalc :: HintMap -> Maybe CalcQuantifier -> Calc -> Html ()
renderCalc hints maybeQuant calc = do
    p_ do
        toHtml ("Calculation" :: Text)
        case maybeQuant of
            Nothing -> skip
            Just quant -> do
                toHtml (" for " :: Text)
                renderCalcQuantifierInline hints quant
        toHtml ("." :: Text)
    div_ [class_ "math-block"] (blockMath (renderCalcMath hints calc))
    let justifications = calcJustifications calc
    when (not (null justifications)) do
        ul_ do
            traverse_ renderStepJustification justifications
    where
        renderStepJustification :: (Int, Justification) -> Html ()
        renderStepJustification (_idx, JustificationEmpty) = skip
        renderStepJustification (idx, jst) = li_ do
            toHtml ("Step " <> Text.pack (show idx) <> ": " :: Text)
            renderJustification jst
            toHtml ("." :: Text)

renderCalcQuantifierInline :: HintMap -> CalcQuantifier -> Html ()
renderCalcQuantifierInline hints (CalcQuantifier vars bound maybeStmt) = do
    renderVarListInline vars
    renderBoundInline hints vars bound
    case maybeStmt of
        Nothing -> skip
        Just stmt -> do
            toHtml (" such that " :: Text)
            renderStmtInline hints stmt

renderCalcMath :: HintMap -> Calc -> Html ()
renderCalcMath hints = \case
    Equation expr steps ->
        mrow_ do
            renderExprMath hints expr
            for_ (toList steps) \(nextExpr, _jst) -> do
                moText "="
                renderExprMath hints nextExpr
    Biconditionals phi steps ->
        mrow_ do
            renderFormulaMath hints phi
            for_ (toList steps) \(nextPhi, _jst) -> do
                moText "⇔"
                renderFormulaMath hints nextPhi

calcJustifications :: Calc -> [(Int, Justification)]
calcJustifications = \case
    Equation _ steps ->
        zip [1..] (snd <$> toList steps)
    Biconditionals _ steps ->
        zip [1..] (snd <$> toList steps)


renderStmtInline :: HintMap -> Stmt -> Html ()
renderStmtInline hints = \case
    StmtFormula phi ->
        inlineMath (renderFormulaMath hints phi)
    StmtVerbPhrase ts vp -> do
        renderTermList hints ts
        toHtml (" " :: Text)
        renderVerbPhraseInline hints (length ts > 1) vp
    StmtNoun ts np -> do
        renderTermList hints ts
        toHtml (if length ts > 1 then " are a " else " is a " :: Text)
        renderNounPhraseMaybe hints np
    StmtStruct t structPhrase -> do
        renderTermInline hints t
        toHtml (" is a " :: Text)
        renderStructPhraseInline structPhrase
    StmtNeg _loc stmt -> do
        toHtml ("it is not the case that " :: Text)
        renderStmtInline hints stmt
    StmtExists _loc np -> do
        toHtml ("there exists " :: Text)
        renderNounPhraseList hints np
    StmtConnected conn _loc stmt1 stmt2 -> do
        renderStmtInline hints stmt1
        toHtml (" " :: Text)
        toHtml (connectiveWord conn)
        toHtml (" " :: Text)
        renderStmtInline hints stmt2
    StmtQuantPhrase _loc qp stmt -> do
        renderQuantPhraseInline hints qp
        toHtml (" " :: Text)
        renderStmtInline hints stmt
    SymbolicQuantified _loc quant vars bound suchThat stmt -> do
        toHtml (quantifierWord quant)
        toHtml (" " :: Text)
        renderVarListInline vars
        renderBoundInline hints vars bound
        case suchThat of
            Nothing -> skip
            Just suchStmt -> do
                toHtml (" such that " :: Text)
                renderStmtInline hints suchStmt
        toHtml (" we have " :: Text)
        renderStmtInline hints stmt

renderQuantPhraseInline :: HintMap -> QuantPhrase -> Html ()
renderQuantPhraseInline hints (QuantPhrase quant np) = do
    toHtml (quantifierWord quant)
    toHtml (" " :: Text)
    renderNounPhraseList hints np

quantifierWord :: Quantifier -> Text
quantifierWord = \case
    Universally -> "for every"
    Existentially -> "there exists"
    Nonexistentially -> "there exists no"

connectiveWord :: Connective -> Text
connectiveWord = \case
    Conjunction -> "and"
    Disjunction -> "or"
    Implication -> "implies"
    Equivalence -> "iff"
    ExclusiveOr -> "xor"
    NegatedDisjunction -> "nor"


renderAsmList :: HintMap -> [Asm] -> Html ()
renderAsmList hints asms =
    joinHtml (toHtml ("; " :: Text)) (renderAsm hints <$> asms)

renderAsm :: HintMap -> Asm -> Html ()
renderAsm hints = \case
    AsmSuppose stmt ->
        renderStmtInline hints stmt
    AsmLetNoun vars np -> do
        renderVarListInline vars
        toHtml (" be " :: Text)
        renderNounPhraseMaybe hints np
    AsmLetIn vars expr -> do
        renderVarListInline vars
        toHtml (" be in " :: Text)
        inlineMath (renderExprMath hints expr)
    AsmLetThe var fun -> do
        renderVarInline var
        toHtml (" be " :: Text)
        renderFunInline renderTermInline' fun
            where renderTermInline' = renderTermInline hints
    AsmLetEq var expr -> do
        renderVarInline var
        toHtml (" = " :: Text)
        inlineMath (renderExprMath hints expr)
    AsmLetStruct var structPhrase -> do
        renderVarInline var
        toHtml (" be a " :: Text)
        renderStructPhraseInline structPhrase


renderTermInline :: HintMap -> Term -> Html ()
renderTermInline hints = \case
    TermExpr expr ->
        inlineMath (renderExprMath hints expr)
    TermFun fun -> do
        toHtml ("the " :: Text)
        renderFunInline (renderTermInline hints) fun
    TermIota _loc var stmt -> do
        toHtml ("the " :: Text)
        renderVarInline var
        toHtml (" such that " :: Text)
        renderStmtInline hints stmt
    TermQuantified quant _loc np -> do
        toHtml (termQuantifierWord quant)
        toHtml (" " :: Text)
        renderNounPhraseMaybe hints np

termQuantifierWord :: Quantifier -> Text
termQuantifierWord = \case
    Universally -> "every"
    Existentially -> "some"
    Nonexistentially -> "no"

renderTermList :: HintMap -> NonEmpty Term -> Html ()
renderTermList hints =
    joinHtml (toHtml (" and " :: Text)) . fmap (renderTermInline hints) . toList

renderNounPhraseMaybe :: HintMap -> NounPhrase Maybe -> Html ()
renderNounPhraseMaybe hints (NounPhrase ls noun maybeName rs maybeSuchThat) = do
    renderAdjListInline renderTermInline' ls
    renderNounInline False renderTermInline' noun
    case maybeName of
        Nothing -> skip
        Just name -> do
            toHtml (" " :: Text)
            renderVarInline name
    renderAdjRListInline hints rs
    case maybeSuchThat of
        Nothing -> skip
        Just stmt -> do
            toHtml (" such that " :: Text)
            renderStmtInline hints stmt
    where
        renderTermInline' = renderTermInline hints

renderNounPhraseList :: HintMap -> NounPhrase [] -> Html ()
renderNounPhraseList hints (NounPhrase ls noun names rs maybeSuchThat) = do
    renderAdjListInline renderTermInline' ls
    renderNounInline (length names > 1) renderTermInline' noun
    when (not (null names)) do
        toHtml (" " :: Text)
        renderVarListInline (NonEmpty.fromList names)
    renderAdjRListInline hints rs
    case maybeSuchThat of
        Nothing -> skip
        Just stmt -> do
            toHtml (" such that " :: Text)
            renderStmtInline hints stmt
    where
        renderTermInline' = renderTermInline hints

renderAdjListInline :: (a -> Html ()) -> [AdjLOf a] -> Html ()
renderAdjListInline renderArg adjs =
    unless (null adjs) do
        joinHtml (toHtml (" " :: Text)) (renderAdjLInline renderArg <$> adjs)
        toHtml (" " :: Text)

renderAdjRListInline :: HintMap -> [AdjROf Term] -> Html ()
renderAdjRListInline hints adjs =
    unless (null adjs) do
        toHtml (" " :: Text)
        joinHtml (toHtml (" and " :: Text)) (renderAdjRInline hints <$> adjs)

renderAdjLInline :: (a -> Html ()) -> AdjLOf a -> Html ()
renderAdjLInline renderArg (AdjL _loc item args) =
    renderLexicalItemInline renderArg item args

renderAdjRInline :: HintMap -> AdjROf Term -> Html ()
renderAdjRInline hints = \case
    AdjR _loc item args ->
        renderLexicalItemInline (renderTermInline hints) item args
    AttrRThat verbPhrase -> do
        toHtml ("that " :: Text)
        renderVerbPhraseInline hints False verbPhrase

renderAdjInline :: (a -> Html ()) -> AdjOf a -> Html ()
renderAdjInline renderArg (Adj _loc item args) =
    renderLexicalItemInline renderArg item args

renderVerbInline :: Bool -> (a -> Html ()) -> VerbOf a -> Html ()
renderVerbInline isPlural renderArg (Verb _loc item args) =
    renderLexicalItemSgPlInline isPlural renderArg item args

renderVerbPhraseInline :: HintMap -> Bool -> VerbPhrase -> Html ()
renderVerbPhraseInline hints isPlural = \case
    VPVerb verb ->
        renderVerbInline isPlural (renderTermInline hints) verb
    VPAdj adjs -> do
        toHtml (if isPlural then "are " else "is " :: Text)
        joinHtml (toHtml (" and " :: Text)) (renderAdjInline (renderTermInline hints) <$> toList adjs)
    VPVerbNot verb -> do
        toHtml (if isPlural then "do not " else "does not " :: Text)
        renderVerbInline True (renderTermInline hints) verb
    VPAdjNot adjs -> do
        toHtml (if isPlural then "are not " else "is not " :: Text)
        joinHtml (toHtml (" and " :: Text)) (renderAdjInline (renderTermInline hints) <$> toList adjs)

renderNounInline :: Bool -> (a -> Html ()) -> NounOf a -> Html ()
renderNounInline isPlural renderArg (Noun _loc item args) =
    renderLexicalItemSgPlInline isPlural renderArg item args

renderFunInline :: (a -> Html ()) -> FunOf a -> Html ()
renderFunInline renderArg Fun{phrase, funArgs} =
    renderLexicalItemSgPlInline False renderArg phrase funArgs

renderStructPhraseInline :: StructPhrase -> Html ()
renderStructPhraseInline item =
    renderLexicalItemSgPlInline False renderTermInlinePlaceholder item []

renderTermInlinePlaceholder :: a -> Html ()
renderTermInlinePlaceholder _ = toHtml ("?" :: Text)

renderLexicalItemInline :: (a -> Html ()) -> LexicalItem -> [a] -> Html ()
renderLexicalItemInline renderArg item args =
    renderPatternInline renderArg (lexicalItemPhrase item) args

renderLexicalItemSgPlInline :: Bool -> (a -> Html ()) -> LexicalItemSgPl -> [a] -> Html ()
renderLexicalItemSgPlInline isPlural renderArg item args =
    renderPatternInline renderArg phrase args
    where
        phrase = if isPlural then pl (lexicalItemSgPlPhrase item) else sg (lexicalItemSgPlPhrase item)

renderPatternInline :: (a -> Html ()) -> [Maybe Token] -> [a] -> Html ()
renderPatternInline renderArg patternParts args =
    joinHtml (toHtml (" " :: Text)) (go patternParts args)
    where
        go [] [] = []
        go [] (_ : _) = error "renderPatternInline: too many arguments"
        go (Nothing : rest) (arg : restArgs) = renderArg arg : go rest restArgs
        go (Nothing : _) [] = error "renderPatternInline: not enough arguments"
        go (Just tok : rest) restArgs = tokenTextHtml tok : go rest restArgs


renderFormulaMath :: HintMap -> Formula -> Html ()
renderFormulaMath hints = \case
    FormulaChain chain ->
        renderChainMath hints chain
    FormulaPredicate _loc predi marker exprs ->
        renderHintedMath hints PredicateHint marker (toList exprs) (renderPrefixPredicateFallback predi (renderExprMath hints <$> toList exprs))
    Connected _loc conn phi psi ->
        mrow_ do
            renderFormulaMath hints phi
            moText (connectiveSymbol conn)
            renderFormulaMath hints psi
    FormulaNeg _loc phi ->
        mrow_ do
            moText "¬"
            renderFormulaMath hints phi
    FormulaQuantified _loc quant vars bound phi ->
        mrow_ do
            moText (quantifierSymbol quant)
            renderVarListMath vars
            renderBoundMath hints vars bound
            moText "."
            renderFormulaMath hints phi
    PropositionalConstant _loc pc ->
        moText (propositionalConstantSymbol pc)

connectiveSymbol :: Connective -> Text
connectiveSymbol = \case
    Conjunction -> "∧"
    Disjunction -> "∨"
    Implication -> "⇒"
    Equivalence -> "⇔"
    ExclusiveOr -> "⊕"
    NegatedDisjunction -> "↓"

quantifierSymbol :: Quantifier -> Text
quantifierSymbol = \case
    Universally -> "∀"
    Existentially -> "∃"
    Nonexistentially -> "∄"

propositionalConstantSymbol :: PropositionalConstant -> Text
propositionalConstantSymbol = \case
    IsBottom -> "⊥"
    IsTop -> "⊤"

renderChainMath :: HintMap -> Chain -> Html ()
renderChainMath hints chain =
    joinHtml (moText "∧") (renderLink <$> splatChain chain)
    where
        renderLink (lhs, sign, rel, rhs) =
            renderRelationApplication hints sign (toList lhs) rel (toList rhs)

        splatChain :: Chain -> [(NonEmpty Expr, Sign, Relation, NonEmpty Expr)]
        splatChain = \case
            ChainBase es sign rel es' ->
                [(es, sign, rel, es')]
            ChainCons es sign rel ch'@(ChainBase es' _ _ _) ->
                (es, sign, rel, es') : splatChain ch'
            ChainCons es sign rel ch'@(ChainCons es' _ _ _) ->
                (es, sign, rel, es') : splatChain ch'

renderBoundMath :: HintMap -> NonEmpty VarSymbol -> Bound -> Html ()
renderBoundMath hints vars = \case
    Unbounded -> skip
    Bounded _loc sign rel expr -> do
        moText ","
        renderRelationApplication hints sign (ExprVar <$> toList vars) rel [expr]

renderRelationApplication :: HintMap -> Sign -> [Expr] -> Relation -> [Expr] -> Html ()
renderRelationApplication hints sign lhs rel rhs =
    applySign sign (renderRelationCore hints lhs rel rhs)

applySign :: Sign -> Html () -> Html ()
applySign sign html = case sign of
    Positive -> html
    Negative -> mrow_ do
        mo_ "¬"
        html

renderRelationCore :: HintMap -> [Expr] -> Relation -> [Expr] -> Html ()
renderRelationCore hints lhs rel rhs = case rel of
    Relation _loc symbol relParams ->
        mrow_ do
            renderExprListMath hints lhs
            renderRelationSymbolCore hints symbol relParams
            renderExprListMath hints rhs
    RelationExpr _loc expr ->
        mrow_ do
            renderExprListMath hints lhs
            renderExprMath hints expr
            renderExprListMath hints rhs

renderRelationSymbolCore :: HintMap -> RelationSymbol -> [Expr] -> Html ()
renderRelationSymbolCore hints symbol relParams =
    renderHintedMath
        hints
        RelationHint
        (relationSymbolMarker symbol)
        relParams
        (renderRelationFallback hints symbol relParams)

renderRelationFallback :: HintMap -> RelationSymbol -> [Expr] -> Html ()
renderRelationFallback hints symbol relParams =
    merror_ (renderRelationFallbackCore hints symbol relParams)

renderRelationFallbackCore :: HintMap -> RelationSymbol -> [Expr] -> Html ()
renderRelationFallbackCore hints symbol relParams
    | null relParams = renderRelationToken (relationSymbolToken symbol)
    | otherwise = msub_ do
        renderRelationToken (relationSymbolToken symbol)
        mrow_ (renderExprListMath hints relParams)

renderRelationToken :: Token -> Html ()
renderRelationToken = \case
    Command "in" -> moText "∈"
    Command "ni" -> moText "∋"
    Command "notin" -> moText "∉"
    Command "meets" -> moText "⋈"
    Command "notmeets" -> moText "⋈̸"
    Command "subset" -> moText "⊂"
    Command "subseteq" -> moText "⊆"
    Command "supset" -> moText "⊃"
    Command "supseteq" -> moText "⊇"
    Command "neq" -> moText "≠"
    tok -> renderMathToken tok


renderExprMath :: HintMap -> Expr -> Html ()
renderExprMath hints = \case
    ExprVar var ->
        renderVarMath var
    ExprInteger _loc n ->
        mnText (Text.pack (show n))
    ExprOp _loc item args ->
        renderHintedMath hints OperatorHint (mixfixMarker item) args (renderPatternFallback (mixfixPattern item) (renderExprMath hints <$> args))
    ExprStructOp _loc symb maybeExpr ->
        let marker = structMarker symb
            args = maybeToList maybeExpr
        in renderHintedMath hints StructOpHint marker args (renderStructFallback symb (renderExprMath hints <$> args))
    ExprFiniteSet _loc exprs ->
        mrow_ do
            moText "{"
            renderExprListMath hints (toList exprs)
            moText "}"
    ExprSep _loc var bound stmt ->
        mrow_ do
            moText "{"
            renderVarMath var
            moText "∈"
            renderExprMath hints bound
            moText "|"
            case stmt of
                StmtFormula phi -> renderFormulaMath hints phi
                _ -> mtextText "condition"
            moText "}"
    ExprReplace _loc expr bounds maybeStmt ->
        mrow_ do
            moText "{"
            renderExprMath hints expr
            moText "|"
            renderReplaceBoundsMath hints (toList bounds)
            for_ maybeStmt \stmt -> do
                moText "|"
                case stmt of
                    StmtFormula phi -> renderFormulaMath hints phi
                    _ -> mtextText "condition"
            moText "}"
    ExprReplacePred _loc rangeVar domVar domExpr stmt ->
        mrow_ do
            moText "{"
            renderVarMath rangeVar
            moText "|"
            moText "∃"
            renderVarMath domVar
            moText "∈"
            renderExprMath hints domExpr
            moText "."
            case stmt of
                StmtFormula phi -> renderFormulaMath hints phi
                _ -> mtextText "condition"
            moText "}"

renderReplaceBoundsMath :: HintMap -> [(VarSymbol, Expr)] -> Html ()
renderReplaceBoundsMath hints =
    joinHtml (moText ",") . fmap renderBound
    where
        renderBound (var, expr) = mrow_ do
            renderVarMath var
            moText "∈"
            renderExprMath hints expr

renderExprListMath :: HintMap -> [Expr] -> Html ()
renderExprListMath hints =
    joinHtml (moText ",") . fmap (renderExprMath hints)

renderHintedMath :: HintMap -> HintCategory -> Marker -> [Expr] -> Html () -> Html ()
renderHintedMath hints category marker args fallback =
    case Map.lookup (category, marker) hints of
        Nothing ->
            trace warningText fallback
        Just RenderHint{..}
            | renderHintArity /= length args ->
                error ("Render hint arity mismatch for " <> show category <> " " <> show marker <> ": expected " <> show renderHintArity <> ", got " <> show (length args))
            | otherwise ->
                mrow_ (traverse_ renderPiece renderHintTemplate)
    where
        warningText =
            "WARNING: missing render hint for "
                <> show category
                <> " "
                <> show marker

        renderedArgs = renderExprMath hints <$> args

        renderPiece :: TemplatePiece -> Html ()
        renderPiece = \case
            Literal text -> toHtmlRaw text
            Slot ix -> case nth (ix - 1) renderedArgs of
                Just html -> html
                Nothing -> error ("Render hint slot out of bounds for " <> show marker <> ": <x" <> show ix <> "/>")

renderPatternFallback :: Pattern -> [Html ()] -> Html ()
renderPatternFallback patternParts renderedArgs =
    merror_ (renderPatternMath patternParts renderedArgs)

renderPatternMath :: Pattern -> [Html ()] -> Html ()
renderPatternMath patternParts renderedArgs =
    mrow_ (traverse_ id (go patternParts renderedArgs))
    where
        go End [] = []
        go End (_ : _) = error "renderPatternMath: too many arguments"
        go (HoleCons rest) (arg : args) = arg : go rest args
        go (HoleCons _) [] = error "renderPatternMath: not enough arguments"
        go (TokenCons tok rest) args = renderMathToken tok : go rest args

renderPrefixPredicateFallback :: PrefixPredicate -> [Html ()] -> Html ()
renderPrefixPredicateFallback (PrefixPredicate command _arity) renderedArgs =
        merror_ do
        miText command
        when (not (null renderedArgs)) do
            moText "("
            joinHtml (moText ",") renderedArgs
            moText ")"

renderStructFallback :: StructSymbol -> [Html ()] -> Html ()
renderStructFallback symb renderedArgs =
    merror_ do
        renderStructSymbolName symb
        when (not (null renderedArgs)) do
            moText "("
            joinHtml (moText ",") renderedArgs
            moText ")"

renderStructSymbolName :: StructSymbol -> Html ()
renderStructSymbolName (StructSymbol name) = miText name

structMarker :: StructSymbol -> Marker
structMarker (StructSymbol name) = Marker name

renderMathToken :: Token -> Html ()
renderMathToken = \case
    Word w -> miText w
    Variable v -> miText v
    Symbol s -> moText s
    Integer n -> mnText (Text.pack (show n))
    Command cmd -> miText cmd
    Label m -> mtextText ("label:" <> m)
    Ref ms -> mtextText ("ref:" <> Text.intercalate "," (toList ms))
    BeginEnv env -> mtextText ("begin:" <> env)
    EndEnv env -> mtextText ("end:" <> env)
    ParenL -> moText "("
    ParenR -> moText ")"
    BracketL -> moText "["
    BracketR -> moText "]"
    VisibleBraceL -> moText "{"
    VisibleBraceR -> moText "}"
    InvisibleBraceL -> moText "("
    InvisibleBraceR -> moText ")"


inlineMath :: Html () -> Html ()
inlineMath inner = math_ [displayinline_] inner

blockMath :: Html () -> Html ()
blockMath inner = math_ [displayblock_] inner

renderVarInline :: VarSymbol -> Html ()
renderVarInline = inlineMath . renderVarMath

renderVarMath :: VarSymbol -> Html ()
renderVarMath var = miText (varText var)

varText :: VarSymbol -> Text
varText = \case
    NamedVarAt _loc name -> name
    FreshVarAt _loc n -> "_" <> Text.pack (show n)

renderVarListInline :: NonEmpty VarSymbol -> Html ()
renderVarListInline vars =
    joinHtml (toHtml (", " :: Text)) (renderVarInline <$> toList vars)

renderVarListMath :: NonEmpty VarSymbol -> Html ()
renderVarListMath vars =
    joinHtml (moText ",") (renderVarMath <$> toList vars)

renderBoundInline :: HintMap -> NonEmpty VarSymbol -> Bound -> Html ()
renderBoundInline hints vars = \case
    Unbounded -> skip
    bound -> do
        toHtml (" with " :: Text)
        inlineMath (renderBoundPhraseMath hints vars bound)

renderBoundPhraseMath :: HintMap -> NonEmpty VarSymbol -> Bound -> Html ()
renderBoundPhraseMath hints vars = \case
    Unbounded -> mrow_ skip
    Bounded _loc sign rel expr ->
        renderRelationApplication hints sign (ExprVar <$> toList vars) rel [expr]

renderSymbolPatternInline :: HintMap -> SymbolPattern -> Html ()
renderSymbolPatternInline hints (SymbolPattern symbol vars) =
    inlineMath (renderHintedMath hints OperatorHint (mixfixMarker symbol) (ExprVar <$> vars) (renderPatternFallback (mixfixPattern symbol) (renderVarMath <$> vars)))

renderJustification :: Justification -> Html ()
renderJustification = \case
    JustificationRef markers -> do
        toHtml ("by " :: Text)
        toHtml (Text.intercalate ", " (markerText <$> toList markers))
    JustificationSetExt ->
        toHtml ("by set extensionality" :: Text)
    JustificationEmpty ->
        skip
    JustificationLocal ->
        toHtml ("by local assumptions" :: Text)

renderJustificationSuffix :: Justification -> Html ()
renderJustificationSuffix JustificationEmpty = skip
renderJustificationSuffix justification = do
    toHtml (" " :: Text)
    renderJustification justification


markerText :: Marker -> Text
markerText (Marker text) = text

tokenTextHtml :: Token -> Html ()
tokenTextHtml = toHtml . tokToText

joinHtml :: Html () -> [Html ()] -> Html ()
joinHtml _ [] = mempty
joinHtml separator (x : xs) = x <> foldMap (separator <>) xs

miText :: Text -> Html ()
miText = mi_ . toHtml

moText :: Text -> Html ()
moText = mo_ . toHtml

mnText :: Text -> Html ()
mnText = mn_ . toHtml

mtextText :: Text -> Html ()
mtextText = mtext_ . toHtml
