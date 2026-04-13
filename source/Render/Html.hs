{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Render.Html
    ( renderDocument
    , supportScriptAssetOutputPath
    , supportScriptAssetContents
    ) where

import Syntax.Abstract

import Base
import Lucid hiding (Term, for_)
import Lucid.Base (makeAttributes)
import Lucid.Math

import Control.Monad (guard, unless, when)
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace, toUpper)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Report.Location (Location, locFile, pattern Nowhere)
import Syntax.Token (VariableDisplay(..), VariableSuffix(..), displayVariable, tokToText)
import System.FilePath.Posix (dropExtension, replaceExtension, splitDirectories, takeDirectory, (</>))


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

type HintMap = Map (HintCategory, Marker, Int) RenderHint
type AnchorMap = Map Marker Text
type BlockRenderInfo = (Int, Block, Text)
type PreviewMap = Map Marker PreviewEntry

data ReferenceContext = ReferenceContext
    { referenceInputPath :: FilePath
    , referenceAnchors :: AnchorMap
    , referencePreviews :: PreviewMap
    }

data PreviewEntry = PreviewEntry
    { previewMarker :: Marker
    , previewKind :: Text
    , previewTitle :: Maybe Text
    , previewSourceFile :: FilePath
    , previewIsImported :: Bool
    , previewId :: Text
    , previewBody :: HintMap -> Html ()
    }

data ReferenceTarget = ReferenceTarget
    { targetMarker :: Marker
    , targetAnchorId :: Text
    , targetKind :: Text
    , targetTitle :: Maybe Text
    , targetSourceFile :: FilePath
    , targetBody :: HintMap -> Html ()
    }

data StmtMathFragment
    = StmtMathProse Text
    | StmtMathNode (Html ())

type StmtMathFragments = [StmtMathFragment]

newtype MissingHintMap = MissingHintMap
    { unMissingHintMap :: Map HintCategory (Set Marker)
    } deriving (Show, Eq)

instance Semigroup MissingHintMap where
    MissingHintMap left <> MissingHintMap right =
        MissingHintMap (Map.unionWith (<>) left right)

instance Monoid MissingHintMap where
    mempty = MissingHintMap mempty

proofCollapseThreshold :: Int
proofCollapseThreshold = 10

referenceGroupThreshold :: Int
referenceGroupThreshold = 5


renderDocument :: FilePath -> Text -> [Block] -> [Block] -> Text
renderDocument inputPath hintsSource blocks theoryBlocks =
    case formatMissingHintWarning missingHints of
        Nothing -> rendered
        Just warningText -> trace (Text.unpack warningText) rendered
    where
        hints = parseHints hintsSource
        missingHints = collectMissingHints hints blocks
        rendered = LazyText.toStrict (renderText (renderPage hints))
        indexedBlocks = zip [1 :: Int ..] blocks
        blockInfos = [(index, block, blockAnchorId index block) | (index, block) <- indexedBlocks]
        rootTargets = concatMap referenceTargetsOfBlockRenderInfo blockInfos
        tocBlocks = [(index, blockId, block) | (index, block, blockId) <- blockInfos, includeInToc block]
        anchors = Map.fromList
            [ (targetMarker, targetAnchorId)
            | ReferenceTarget{targetMarker, targetAnchorId} <- rootTargets
            ]
        referencedMarkers = collectReferencedMarkers blocks
        previews = buildPreviewMap inputPath referencedMarkers anchors theoryBlocks
        referenceContext = ReferenceContext inputPath anchors previews

        renderPage :: HintMap -> Html ()
        renderPage hintMap = doctypehtml_ do
            head_ do
                meta_ [charset_ "utf-8"]
                title_ (toHtml (Text.pack inputPath))
                style_ pageStyles
            body_ do
                div_ [class_ "page-layout"] do
                    aside_ [class_ "toc-column"] do
                        nav_ [class_ "toc"] do
                            h2_ [class_ "toc-heading"] "Contents"
                            input_
                                [ class_ "toc-filter"
                                , type_ "search"
                                , placeholder_ "Filter labels"
                                , makeAttributes "aria-label" "Filter TOC by label"
                                ]
                            ol_ [class_ "toc-list"] do
                                traverse_ renderTocEntry tocBlocks
                    main_ do
                        h1_ (toHtml (Text.pack inputPath))
                        traverse_ (renderBlock hintMap referenceContext) blockInfos
                renderPreviewStore hintMap previews
                div_
                    [ id_ "reference-preview-popup"
                    , class_ "reference-preview-popup"
                    , makeAttributes "role" "tooltip"
                    , makeAttributes "aria-hidden" "true"
                    ]
                    skip
                script_ [src_ (Text.pack (supportScriptAssetRelativePath inputPath))] ("" :: Text)


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
    , "  --toc-active-bg: #f3efe6;"
    , "  --toc-active-fg: #1b1b1b;"
    , "  --toc-active-accent: #b9aa7a;"
    , "  --preview-bg: #fffdf8;"
    , "  --preview-border: #cfc3a3;"
    , "  --preview-shadow: rgba(0, 0, 0, 0.18);"
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
    , ".page-layout {"
    , "  display: grid;"
    , "  grid-template-columns: minmax(16rem, 24rem) minmax(0, 1fr);"
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
    , "  display: flex;"
    , "  flex-direction: column;"
    , "  height: 100%;"
    , "  min-height: 0;"
    , "}"
    , ".toc-heading {"
    , "  margin: 0 0 0.75rem;"
    , "  color: var(--muted-fg);"
    , "  font-size: 0.9rem;"
    , "  letter-spacing: 0.04em;"
    , "  text-transform: uppercase;"
    , "}"
    , ".toc-filter {"
    , "  box-sizing: border-box;"
    , "  width: 100%;"
    , "  margin: 0 0 0.9rem;"
    , "  padding: 0.45rem 0.55rem;"
    , "  border: 1px solid var(--badge-border);"
    , "  border-radius: 0.35rem;"
    , "  background: var(--page-bg);"
    , "  color: var(--page-fg);"
    , "  font: inherit;"
    , "}"
    , ".toc-filter::placeholder {"
    , "  color: var(--muted-fg);"
    , "}"
    , ".toc-list {"
    , "  flex: 1 1 auto;"
    , "  min-height: 0;"
    , "  overflow-y: auto;"
    , "  list-style: none;"
    , "  margin: 0;"
    , "  padding: 0 0.5rem 0 0;"
    , "}"
    , ".toc-list > li {"
    , "  margin: 0 0 0.8rem;"
    , "}"
    , ".toc-list > li > a {"
    , "  display: block;"
    , "  margin: -0.15rem -0.35rem;"
    , "  padding: 0.15rem 0.35rem;"
    , "  border-radius: 0.35rem;"
    , "  color: inherit;"
    , "  text-decoration: none;"
    , "  transition: background-color 120ms ease, box-shadow 120ms ease, color 120ms ease;"
    , "}"
    , ".toc-list > li > a:hover,"
    , ".toc-list > li > a:focus-visible {"
    , "  text-decoration: underline;"
    , "}"
    , ".toc-list > li > a.is-active {"
    , "  background: var(--toc-active-bg);"
    , "  box-shadow: inset 0.2rem 0 0 var(--toc-active-accent);"
    , "  color: var(--toc-active-fg);"
    , "}"
    , ".toc-list > li > a.is-active > code {"
    , "  color: var(--toc-active-fg);"
    , "}"
    , ".toc-list > li > a > span:first-child {"
    , "  display: block;"
    , "  font-weight: 700;"
    , "}"
    , ".toc-list > li > a > code {"
    , "  display: block;"
    , "  margin-top: 0.15rem;"
    , "  color: var(--muted-fg);"
    , "  font-size: 0.9em;"
    , "  overflow-wrap: anywhere;"
    , "}"
    , "main {"
    , "  min-width: 0;"
    , "  min-height: 0;"
    , "  overflow-y: auto;"
    , "}"
    , "main > *[id] {"
    , "  display: block;"
    , "  margin: 0 0 1rem;"
    , "  scroll-margin-top: 1rem;"
    , "}"
    , "head- {"
    , "  font-weight: 700;"
    , "}"
    , "title- {"
    , "  font-weight: 400;"
    , "}"
    , "id-,"
    , "main a[href^=\"#\"],"
    , ".ref-badge {"
    , "  display: inline-block;"
    , "  padding: 0.02rem 0.35rem;"
    , "  border: 1px solid var(--badge-border);"
    , "  border-radius: 0.2rem;"
    , "  background: var(--badge-bg);"
    , "  color: var(--badge-fg);"
    , "  font-family: \"SFMono-Regular\", Menlo, Consolas, \"Liberation Mono\", monospace;"
    , "  font-size: 0.82em;"
    , "  text-decoration: none;"
    , "}"
    , "head- > id- {"
    , "  margin-left: 0.45rem;"
    , "}"
    , "main a[href^=\"#\"]:hover,"
    , "main a[href^=\"#\"]:focus-visible,"
    , ".ref-badge.has-preview:hover,"
    , ".ref-badge.has-preview:focus-visible {"
    , "  text-decoration: underline;"
    , "}"
    , ".ref-badge.has-preview {"
    , "  cursor: help;"
    , "}"
    , ".ref-badge-group {"
    , "  user-select: none;"
    , "}"
    , "proof- > p:first-child,"
    , "proof- > details > summary + p {"
    , "  display: inline;"
    , "  margin: 0;"
    , "}"
    , "proof- proof- {"
    , "  display: block;"
    , "  margin: 0.5rem 0 0.5rem 1rem;"
    , "  padding-left: 0.75rem;"
    , "  border-left: 1px solid var(--rule-color);"
    , "}"
    , "proof- > details {"
    , "  margin: 0;"
    , "}"
    , "proof- > details > summary {"
    , "  cursor: pointer;"
    , "  font-weight: 700;"
    , "}"
    , "proof- > details > summary title- {"
    , "  font-weight: 400;"
    , "}"
    , "proof- > details > :not(summary) {"
    , "  margin-top: 0.5rem;"
    , "}"
    , "datatype- > details {"
    , "  margin-top: 0.75rem;"
    , "}"
    , "datatype- > details > summary {"
    , "  cursor: pointer;"
    , "  font-weight: 700;"
    , "}"
    , "datatype- > details > :not(summary) {"
    , "  margin-top: 0.5rem;"
    , "}"
    , ".datatype-derived-facts {"
    , "  margin: 0;"
    , "  padding-left: 1.5rem;"
    , "}"
    , ".datatype-derived-facts > li {"
    , "  margin: 0.35rem 0;"
    , "}"
    , ".reference-preview-store {"
    , "  display: none;"
    , "}"
    , ".reference-preview-popup {"
    , "  position: fixed;"
    , "  z-index: 1000;"
    , "  box-sizing: border-box;"
    , "  width: 44rem;"
    , "  max-width: calc(100vw - 2rem);"
    , "  max-height: 34rem;"
    , "  max-height: min(34rem, calc(100vh - 2rem));"
    , "  overflow: auto;"
    , "  overscroll-behavior: contain;"
    , "  padding: 0.75rem 0.9rem;"
    , "  border: 1px solid var(--preview-border);"
    , "  border-radius: 0.45rem;"
    , "  background: var(--preview-bg);"
    , "  color: var(--page-fg);"
    , "  box-shadow: 0 0.75rem 2.25rem var(--preview-shadow);"
    , "  opacity: 0;"
    , "  pointer-events: none;"
    , "  transform: translateY(0.2rem);"
    , "  transition: opacity 90ms ease, transform 90ms ease;"
    , "}"
    , ".reference-preview-popup[aria-hidden=\"true\"] {"
    , "  visibility: hidden;"
    , "}"
    , ".reference-preview-popup.is-visible {"
    , "  opacity: 1;"
    , "  pointer-events: auto;"
    , "  transform: translateY(0);"
    , "}"
    , ".reference-preview-popup * {"
    , "  box-sizing: border-box;"
    , "}"
    , ".reference-preview-template {"
    , "  display: flex;"
    , "  flex-direction: column;"
    , "  gap: 0.45rem;"
    , "  width: 100%;"
    , "}"
    , ".reference-preview-heading {"
    , "  display: block;"
    , "  margin: 0;"
    , "  width: 100%;"
    , "  color: var(--muted-fg);"
    , "  font-size: 0.82rem;"
    , "  letter-spacing: 0.035em;"
    , "  text-transform: uppercase;"
    , "}"
    , ".reference-preview-heading code {"
    , "  color: var(--page-fg);"
    , "  font-family: \"SFMono-Regular\", Menlo, Consolas, \"Liberation Mono\", monospace;"
    , "  letter-spacing: 0;"
    , "  text-transform: none;"
    , "}"
    , ".reference-preview-heading a {"
    , "  color: inherit;"
    , "  text-decoration: none;"
    , "}"
    , ".reference-preview-heading a:hover,"
    , ".reference-preview-heading a:focus-visible {"
    , "  text-decoration: underline;"
    , "}"
    , ".reference-preview-source {"
    , "  display: block;"
    , "  margin: 0;"
    , "  color: var(--subtle-fg);"
    , "  font-size: 0.78rem;"
    , "  letter-spacing: 0;"
    , "  text-transform: none;"
    , "}"
    , ".reference-preview-body {"
    , "  display: block;"
    , "  clear: both;"
    , "  margin: 0;"
    , "  width: 100%;"
    , "}"
    , ".reference-preview-body p {"
    , "  margin: 0.35rem 0 0;"
    , "}"
    , ".reference-preview-body p:first-child {"
    , "  margin-top: 0;"
    , "}"
    , ".reference-preview-group-template {"
    , "  display: flex;"
    , "  flex-direction: column;"
    , "  gap: 1rem;"
    , "  margin: 0;"
    , "  width: 100%;"
    , "}"
    , ".reference-preview-group-template > .reference-preview-template + .reference-preview-template {"
    , "  padding-top: 1rem;"
    , "  border-top: 1px solid var(--badge-border);"
    , "}"
    , ".reference-preview-statement {"
    , "  display: block;"
    , "  width: 100%;"
    , "  white-space: normal;"
    , "  overflow-wrap: break-word;"
    , "}"
    , ".reference-preview-statement math {"
    , "  max-width: 100%;"
    , "  overflow-x: auto;"
    , "  overflow-y: hidden;"
    , "  vertical-align: middle;"
    , "}"
    , "math[display=\"block\"] {"
    , "  display: block;"
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
    , "    --toc-active-bg: #2b271f;"
    , "    --toc-active-fg: #f0ebe1;"
    , "    --toc-active-accent: #99865a;"
    , "    --preview-bg: #211f1a;"
    , "    --preview-border: #776a50;"
    , "    --preview-shadow: rgba(0, 0, 0, 0.55);"
    , "  }"
    , "}"
    , "@media (max-width: 900px) {"
    , "  body {"
    , "    height: auto;"
    , "    overflow: auto;"
    , "  }"
    , "  .page-layout {"
    , "    grid-template-columns: 1fr;"
    , "    grid-template-rows: auto;"
    , "    gap: 1.5rem;"
    , "    height: auto;"
    , "  }"
    , "  .toc-column {"
    , "    display: none;"
    , "  }"
    , "  main {"
    , "    min-height: auto;"
    , "    overflow: visible;"
    , "  }"
    , "}"
    ]

supportScriptAssetOutputPath :: FilePath
supportScriptAssetOutputPath = "_static" </> "naproche-html.js"

supportScriptAssetRelativePath :: FilePath -> FilePath
supportScriptAssetRelativePath inputPath =
    foldr1 (</>) (replicate depth ".." <> ["_static", "naproche-html.js"])
    where
        outputDir = takeDirectory (replaceExtension inputPath "html")
        depth = length (List.filter (`notElem` [".", ""]) (splitDirectories outputDir))

supportScriptAssetContents :: Text
supportScriptAssetContents = tocScript <> "\n" <> referencePreviewScript


tocScript :: Text
tocScript = Text.unlines
    [ "(function () {"
    , "  const toc = document.querySelector('.toc');"
    , "  const tocList = toc ? toc.querySelector('.toc-list') : null;"
    , "  const filterInput = toc ? toc.querySelector('.toc-filter') : null;"
    , "  const content = document.querySelector('main');"
    , "  if (!toc || !tocList || !content) return;"
    , "  const links = Array.from(tocList.querySelectorAll(':scope > li > a[href^=\"#\"]'));"
    , "  const blocks = Array.from(content.querySelectorAll(':scope > *[id]'));"
    , "  if (!links.length || !blocks.length) return;"
    , "  const linkByTarget = new Map(links.map((link) => [decodeURIComponent(link.hash.slice(1)), link]));"
    , "  const tocEntries = links.map((link) => ({"
    , "    item: link.parentElement,"
    , "    link,"
    , "    label: ((link.querySelector('code') || link.lastElementChild || link).textContent || '').trim().toLowerCase()"
    , "  }));"
    , "  let activeTarget = null;"
    , "  let rafId = 0;"
    , "  let suspendUntil = 0;"
    , ""
    , "  const keepActiveLinkVisible = (link) => {"
    , "    if (!link || performance.now() < suspendUntil) return;"
    , "    const item = link.parentElement;"
    , "    if (item && item.hidden) return;"
    , "    const tocRect = tocList.getBoundingClientRect();"
    , "    const linkRect = link.getBoundingClientRect();"
    , "    const comfortTop = tocRect.top + tocRect.height * 0.2;"
    , "    const comfortBottom = tocRect.bottom - tocRect.height * 0.2;"
    , "    if (linkRect.top < comfortTop || linkRect.bottom > comfortBottom) {"
    , "      link.scrollIntoView({ block: 'nearest', inline: 'nearest' });"
    , "    }"
    , "  };"
    , ""
    , "  const applyFilter = () => {"
    , "    const query = filterInput ? filterInput.value.trim().toLowerCase() : '';"
    , "    for (const { item, label } of tocEntries) {"
    , "      if (!item) continue;"
    , "      item.hidden = query !== '' && !label.includes(query);"
    , "    }"
    , "    if (activeTarget) {"
    , "      keepActiveLinkVisible(linkByTarget.get(activeTarget));"
    , "    }"
    , "  };"
    , ""
    , "  const setActiveTarget = (target) => {"
    , "    if (!target || target === activeTarget) return;"
    , "    const previous = activeTarget ? linkByTarget.get(activeTarget) : null;"
    , "    if (previous) {"
    , "      previous.classList.remove('is-active');"
    , "      previous.removeAttribute('aria-current');"
    , "    }"
    , "    activeTarget = target;"
    , "    const next = linkByTarget.get(target);"
    , "    if (!next) return;"
    , "    next.classList.add('is-active');"
    , "    next.setAttribute('aria-current', 'location');"
    , "    keepActiveLinkVisible(next);"
    , "  };"
    , ""
    , "  const firstTarget = blocks[0].id;"
    , ""
    , "  const findActiveTarget = () => {"
    , "    const contentRect = content.getBoundingClientRect();"
    , "    const topSnapThreshold = 40;"
    , "    for (const block of blocks) {"
    , "      const target = block.id;"
    , "      const rect = block.getBoundingClientRect();"
    , "      if (rect.top >= contentRect.top - 4 && rect.top <= contentRect.top + topSnapThreshold) {"
    , "        return target;"
    , "      }"
    , "    }"
    , "    const activationLine = contentRect.top + contentRect.height * 0.22;"
    , "    let candidate = firstTarget;"
    , "    for (const block of blocks) {"
    , "      const target = block.id;"
    , "      if (block.getBoundingClientRect().top <= activationLine) {"
    , "        candidate = target;"
    , "        continue;"
    , "      }"
    , "      break;"
    , "    }"
    , "    return candidate;"
    , "  };"
    , ""
    , "  const scheduleUpdate = () => {"
    , "    if (rafId) return;"
    , "    rafId = window.requestAnimationFrame(() => {"
    , "      rafId = 0;"
    , "      setActiveTarget(findActiveTarget());"
    , "    });"
    , "  };"
    , ""
    , "  const suspendAutofollow = () => {"
    , "    suspendUntil = performance.now() + 1500;"
    , "  };"
    , ""
    , "  const revealTarget = (target) => {"
    , "    let parent = target.parentElement;"
    , "    while (parent) {"
    , "      if (parent.localName === 'details') {"
    , "        parent.open = true;"
    , "      }"
    , "      parent = parent.parentElement;"
    , "    }"
    , "  };"
    , ""
    , "  const scrollToTarget = (targetId) => {"
    , "    const target = document.getElementById(targetId);"
    , "    if (!target || !content.contains(target)) return false;"
    , "    revealTarget(target);"
    , "    target.scrollIntoView({ block: 'start', inline: 'nearest' });"
    , "    setActiveTarget(targetId);"
    , "    return true;"
    , "  };"
    , ""
    , "  content.addEventListener('scroll', scheduleUpdate, { passive: true });"
    , "  window.addEventListener('resize', scheduleUpdate);"
    , "  tocList.addEventListener('wheel', suspendAutofollow, { passive: true });"
    , "  tocList.addEventListener('touchstart', suspendAutofollow, { passive: true });"
    , "  toc.addEventListener('pointerdown', suspendAutofollow);"
    , "  toc.addEventListener('focusin', suspendAutofollow);"
    , "  if (filterInput) {"
    , "    filterInput.addEventListener('input', applyFilter);"
    , "  }"
    , "  for (const details of content.querySelectorAll('details')) {"
    , "    details.addEventListener('toggle', scheduleUpdate);"
    , "  }"
    , ""
    , "  toc.addEventListener('click', (event) => {"
    , "    const link = event.target.closest('a[href^=\"#\"]');"
    , "    if (!link || !tocList.contains(link)) return;"
    , "    const targetId = decodeURIComponent(link.hash.slice(1));"
    , "    if (!scrollToTarget(targetId)) return;"
    , "    event.preventDefault();"
    , "    suspendUntil = 0;"
    , "    if (location.hash !== '#' + targetId) {"
    , "      try {"
    , "        history.pushState(null, '', '#' + targetId);"
    , "      } catch (_error) {"
    , "        location.hash = targetId;"
    , "      }"
    , "    }"
    , "  });"
    , ""
    , "  window.addEventListener('hashchange', () => {"
    , "    if (location.hash.length <= 1) return;"
    , "    const hashTarget = decodeURIComponent(location.hash.slice(1));"
    , "    if (!scrollToTarget(hashTarget)) scheduleUpdate();"
    , "  });"
    , ""
    , "  if (location.hash.length > 1) {"
    , "    const hashTarget = decodeURIComponent(location.hash.slice(1));"
    , "    window.requestAnimationFrame(() => {"
    , "      applyFilter();"
    , "      if (!scrollToTarget(hashTarget)) scheduleUpdate();"
    , "    });"
    , "    return;"
    , "  }"
    , ""
    , "  applyFilter();"
    , "  scheduleUpdate();"
    , "})();"
    ]

referencePreviewScript :: Text
referencePreviewScript = Text.unlines
    [ "(function () {"
    , "  const content = document.querySelector('main');"
    , "  const popup = document.getElementById('reference-preview-popup');"
    , "  if (!content || !popup) return;"
    , "  let activeTrigger = null;"
    , "  let lastPointer = null;"
    , "  let isPinned = false;"
    , "  let hideTimer = 0;"
    , "  const offset = 14;"
    , "  const margin = 12;"
    , "  const hideDelay = 180;"
    , ""
    , "  const cloneHiddenPreview = (trigger) => {"
    , "    const previewId = trigger.getAttribute('data-preview-id');"
    , "    const template = previewId ? document.getElementById(previewId) : null;"
    , "    if (!template) return null;"
    , "    const clone = template.cloneNode(true);"
    , "    clone.removeAttribute('id');"
    , "    return clone;"
    , "  };"
    , ""
    , "  const buildCurrentPreview = (trigger) => {"
    , "    const targetId = trigger.getAttribute('data-preview-target-id');"
    , "    const target = targetId ? document.getElementById(targetId) : null;"
    , "    if (!target || !content.contains(target)) return null;"
    , "    const template = document.createElement('div');"
    , "    template.className = 'reference-preview-template';"
    , "    const heading = document.createElement('div');"
    , "    heading.className = 'reference-preview-heading';"
    , "    const kind = target.getAttribute('data-preview-kind') || 'Reference';"
    , "    const label = target.getAttribute('data-preview-label') || targetId;"
    , "    const title = target.getAttribute('data-preview-title');"
    , "    heading.append(document.createTextNode(kind + ' '));"
    , "    const code = document.createElement('code');"
    , "    code.textContent = label;"
    , "    heading.append(code);"
    , "    if (title) {"
    , "      heading.append(document.createTextNode(' (' + title + ')'));"
    , "    }"
    , "    template.append(heading);"
    , "    const body = document.createElement('div');"
    , "    body.className = 'reference-preview-body';"
    , "    const statement = document.createElement('div');"
    , "    statement.className = 'reference-preview-statement';"
    , "    const head = Array.from(target.children).find((child) => child.localName === 'head-');"
    , "    const nodes = Array.from(target.childNodes);"
    , "    const start = head ? nodes.indexOf(head) + 1 : 0;"
    , "    for (const node of nodes.slice(start)) {"
    , "      statement.append(node.cloneNode(true));"
    , "    }"
    , "    if (!statement.childNodes.length) return null;"
    , "    body.append(statement);"
    , "    template.append(body);"
    , "    return template;"
    , "  };"
    , ""
    , "  const buildMissingPreview = (item) => {"
    , "    const label = item.getAttribute('data-reference-label') || '';"
    , "    const template = document.createElement('div');"
    , "    template.className = 'reference-preview-template';"
    , "    const heading = document.createElement('div');"
    , "    heading.className = 'reference-preview-heading';"
    , "    heading.append(document.createTextNode('Reference '));"
    , "    const code = document.createElement('code');"
    , "    code.textContent = label;"
    , "    heading.append(code);"
    , "    template.append(heading);"
    , "    const body = document.createElement('div');"
    , "    body.className = 'reference-preview-body';"
    , "    const statement = document.createElement('p');"
    , "    statement.className = 'reference-preview-statement';"
    , "    statement.textContent = 'Preview unavailable.';"
    , "    body.append(statement);"
    , "    template.append(body);"
    , "    return template;"
    , "  };"
    , ""
    , "  const linkGroupHeading = (item, template) => {"
    , "    const href = item.getAttribute('data-preview-link');"
    , "    if (!href) return template;"
    , "    const heading = template.querySelector('.reference-preview-heading');"
    , "    const code = heading ? heading.querySelector('code') : null;"
    , "    if (!heading || !code || code.closest('a')) return template;"
    , "    const link = document.createElement('a');"
    , "    link.href = href;"
    , "    link.append(code.cloneNode(true));"
    , "    code.replaceWith(link);"
    , "    return template;"
    , "  };"
    , ""
    , "  const buildGroupPreview = (trigger) => {"
    , "    if (!trigger.hasAttribute('data-preview-group')) return null;"
    , "    const items = Array.from(trigger.querySelectorAll('.reference-preview-group-items > [data-reference-label]'));"
    , "    if (!items.length) return null;"
    , "    const template = document.createElement('div');"
    , "    template.className = 'reference-preview-group-template';"
    , "    for (const item of items) {"
    , "      const preview = cloneHiddenPreview(item) || buildCurrentPreview(item) || buildMissingPreview(item);"
    , "      template.append(linkGroupHeading(item, preview));"
    , "    }"
    , "    return template;"
    , "  };"
    , ""
    , "  const previewFor = (trigger) => buildGroupPreview(trigger) || cloneHiddenPreview(trigger) || buildCurrentPreview(trigger);"
    , ""
    , "  const clamp = (value, min, max) => Math.min(Math.max(value, min), max);"
    , "  const findTrigger = (target) => target instanceof Element ? target.closest('[data-preview-group], [data-preview-id], [data-preview-target-id]') : null;"
    , "  const clearHideTimer = () => {"
    , "    if (!hideTimer) return;"
    , "    window.clearTimeout(hideTimer);"
    , "    hideTimer = 0;"
    , "  };"
    , ""
    , "  const hidePreview = () => {"
    , "    clearHideTimer();"
    , "    activeTrigger = null;"
    , "    lastPointer = null;"
    , "    isPinned = false;"
    , "    popup.classList.remove('is-visible');"
    , "    popup.setAttribute('aria-hidden', 'true');"
    , "    popup.replaceChildren();"
    , "  };"
    , ""
    , "  const scheduleHide = () => {"
    , "    if (isPinned) return;"
    , "    clearHideTimer();"
    , "    hideTimer = window.setTimeout(() => {"
    , "      hideTimer = 0;"
    , "      if (!isPinned) hidePreview();"
    , "    }, hideDelay);"
    , "  };"
    , ""
    , "  const placePreview = () => {"
    , "    if (!activeTrigger) return;"
    , "    const triggerRect = activeTrigger.getBoundingClientRect();"
    , "    const popupRect = popup.getBoundingClientRect();"
    , "    const fallbackWidth = Math.min(704, Math.max(0, window.innerWidth - margin * 2));"
    , "    const popupWidth = popupRect.width || fallbackWidth;"
    , "    const popupHeight = popupRect.height || 0;"
    , "    const pointer = lastPointer;"
    , "    const anchorX = pointer ? pointer.clientX : triggerRect.left;"
    , "    const anchorY = pointer ? pointer.clientY : triggerRect.bottom;"
    , "    let left = anchorX + (pointer ? offset : 0);"
    , "    let top = anchorY + offset;"
    , "    if (top + popupHeight + margin > window.innerHeight) {"
    , "      const upperAnchor = pointer ? pointer.clientY : triggerRect.top;"
    , "      top = Math.max(margin, upperAnchor - popupHeight - offset);"
    , "    }"
    , "    left = clamp(left, margin, Math.max(margin, window.innerWidth - popupWidth - margin));"
    , "    popup.style.left = `${left}px`;"
    , "    popup.style.top = `${top}px`;"
    , "  };"
    , ""
    , "  const showPreview = (trigger, pointerEvent, pinned = false) => {"
    , "    const source = previewFor(trigger);"
    , "    if (!source) {"
    , "      hidePreview();"
    , "      return;"
    , "    }"
    , "    const wasPinned = isPinned && trigger === activeTrigger;"
    , "    clearHideTimer();"
    , "    activeTrigger = trigger;"
    , "    lastPointer = pointerEvent ? { clientX: pointerEvent.clientX, clientY: pointerEvent.clientY } : null;"
    , "    isPinned = pinned || wasPinned;"
    , "    popup.replaceChildren(source);"
    , "    popup.scrollTop = 0;"
    , "    popup.setAttribute('aria-hidden', 'false');"
    , "    popup.classList.add('is-visible');"
    , "    placePreview();"
    , "  };"
    , ""
    , "  content.addEventListener('pointerover', (event) => {"
    , "    const trigger = findTrigger(event.target);"
    , "    if (!trigger || !content.contains(trigger) || trigger === activeTrigger) return;"
    , "    showPreview(trigger, event);"
    , "  });"
    , ""
    , "  content.addEventListener('pointermove', (event) => {"
    , "    const trigger = findTrigger(event.target);"
    , "    if (!trigger || trigger !== activeTrigger) return;"
    , "    if (isPinned) return;"
    , "    lastPointer = { clientX: event.clientX, clientY: event.clientY };"
    , "    placePreview();"
    , "  });"
    , ""
    , "  content.addEventListener('pointerout', (event) => {"
    , "    const trigger = findTrigger(event.target);"
    , "    if (!trigger || trigger !== activeTrigger) return;"
    , "    if (isPinned) return;"
    , "    const related = event.relatedTarget;"
    , "    if (related instanceof Node && trigger.contains(related)) return;"
    , "    if (related instanceof Node && popup.contains(related)) return;"
    , "    scheduleHide();"
    , "  });"
    , ""
    , "  content.addEventListener('focusin', (event) => {"
    , "    const trigger = findTrigger(event.target);"
    , "    if (!trigger || !content.contains(trigger)) return;"
    , "    showPreview(trigger, null);"
    , "  });"
    , ""
    , "  content.addEventListener('focusout', (event) => {"
    , "    const trigger = findTrigger(event.target);"
    , "    if (trigger && trigger === activeTrigger && !isPinned) scheduleHide();"
    , "  });"
    , ""
    , "  content.addEventListener('click', (event) => {"
    , "    const trigger = findTrigger(event.target);"
    , "    if (!trigger || !trigger.hasAttribute('data-preview-group') || !content.contains(trigger)) return;"
    , "    event.preventDefault();"
    , "    showPreview(trigger, event, true);"
    , "  });"
    , ""
    , "  popup.addEventListener('pointerenter', clearHideTimer);"
    , "  popup.addEventListener('pointerleave', scheduleHide);"
    , ""
    , "  document.addEventListener('click', (event) => {"
    , "    if (!isPinned) return;"
    , "    const target = event.target;"
    , "    if (target instanceof Node && popup.contains(target)) return;"
    , "    if (activeTrigger && target instanceof Node && activeTrigger.contains(target)) return;"
    , "    hidePreview();"
    , "  });"
    , ""
    , "  content.addEventListener('scroll', hidePreview, { passive: true });"
    , "  window.addEventListener('resize', hidePreview);"
    , "  window.addEventListener('hashchange', hidePreview);"
    , "  document.addEventListener('keydown', (event) => {"
    , "    if (event.key === 'Escape') {"
    , "      hidePreview();"
    , "      return;"
    , "    }"
    , "    if (event.key !== 'Enter' && event.key !== ' ') return;"
    , "    const trigger = findTrigger(document.activeElement);"
    , "    if (!trigger || !trigger.hasAttribute('data-preview-group')) return;"
    , "    event.preventDefault();"
    , "    showPreview(trigger, null, true);"
    , "  });"
    , "})();"
    ]

collectReferencedMarkers :: [Block] -> Set Marker
collectReferencedMarkers =
    foldMap collectBlock
    where
        collectBlock :: Block -> Set Marker
        collectBlock = \case
            BlockProof _start proof _end ->
                collectProof proof
            _ ->
                mempty

        collectProof :: Proof -> Set Marker
        collectProof = \case
            Omitted ->
                mempty
            Qed _loc justification ->
                collectJustification justification
            ByCase _loc cases ->
                foldMap collectCase cases
            ByContradiction _loc proof ->
                collectProof proof
            BySetInduction _loc _term proof ->
                collectProof proof
            ByOrdInduction _loc proof ->
                collectProof proof
            Assume _loc _stmt proof ->
                collectProof proof
            FixSymbolic _loc _vars _bound proof ->
                collectProof proof
            FixSuchThat _loc _vars _stmt proof ->
                collectProof proof
            Calc _loc _maybeQuant calc proof ->
                collectCalc calc <> collectProof proof
            TakeVar _loc _vars _bound _stmt justification proof ->
                collectJustification justification <> collectProof proof
            TakeNoun _loc _np justification proof ->
                collectJustification justification <> collectProof proof
            Have _loc _maybeStmt _stmt justification proof ->
                collectJustification justification <> collectProof proof
            Suffices _loc _stmt justification proof ->
                collectJustification justification <> collectProof proof
            Subclaim _loc _stmt subproof proof ->
                collectProof subproof <> collectProof proof
            Define _loc _var _expr proof ->
                collectProof proof
            DefineFunction _loc _fun _arg _value _boundVar _boundExpr proof ->
                collectProof proof
            DefineFunctionLocal _loc _fun _arg _target _domVar _codVar _rules proof ->
                collectProof proof

        collectCase :: Case -> Set Marker
        collectCase Case{caseProof} =
            collectProof caseProof

        collectCalc :: Calc -> Set Marker
        collectCalc = \case
            Equation _expr steps ->
                foldMap (collectJustification . snd) steps
            Biconditionals _formula steps ->
                foldMap (collectJustification . snd) steps

        collectJustification :: Justification -> Set Marker
        collectJustification = \case
            JustificationRef markers ->
                Set.fromList (toList markers)
            JustificationSetExt ->
                mempty
            JustificationEmpty ->
                mempty
            JustificationLocal ->
                mempty

collectMissingHints :: HintMap -> [Block] -> MissingHintMap
collectMissingHints hints = foldMap collectBlock
    where
        noteMissingHint :: HintCategory -> Marker -> Int -> MissingHintMap
        noteMissingHint category marker arity =
            if Map.member (category, marker, arity) hints
                then mempty
                else MissingHintMap (Map.singleton category (Set.singleton marker))

        collectBlock :: Block -> MissingHintMap
        collectBlock = \case
            BlockAxiom _loc _title _marker axiom ->
                collectAxiom axiom
            BlockClaim _kind _loc _title _marker claim ->
                collectClaim claim
            BlockProof _start proof _end ->
                collectProof proof
            BlockDefn _loc _title _marker defn ->
                collectDefn defn
            BlockAbbr _loc _title _marker abbr ->
                collectAbbreviation abbr
            BlockData _loc _title _marker datatype ->
                collectDatatype datatype
            BlockInductive _loc _title _marker ind ->
                collectInductive ind
            BlockSig _loc _title _marker asms sig ->
                collectAsms asms
                    <> collectSignature sig
            BlockStruct _loc _title _marker structDefn ->
                collectStructDefn structDefn

        collectAxiom :: Axiom -> MissingHintMap
        collectAxiom (Axiom asms stmt) =
            collectAsms asms <> collectStmt stmt

        collectClaim :: Claim -> MissingHintMap
        collectClaim (Claim asms stmt) =
            collectAsms asms <> collectStmt stmt

        collectDefn :: Defn -> MissingHintMap
        collectDefn = \case
            Defn asms defnHead stmt ->
                collectAsms asms
                    <> collectDefnHead defnHead
                    <> collectStmt stmt
            DefnFun asms _fun maybeTerm resultTerm ->
                collectAsms asms
                    <> foldMap collectTerm maybeTerm
                    <> collectTerm resultTerm
            DefnOp symb expr ->
                collectSymbolPattern symb
                    <> collectExpr expr

        collectDefnHead :: DefnHead -> MissingHintMap
        collectDefnHead = \case
            DefnAdj maybeNp _var _adj ->
                foldMap collectNounPhraseMaybe maybeNp
            DefnVerb maybeNp _var _verb ->
                foldMap collectNounPhraseMaybe maybeNp
            DefnNoun _var noun ->
                collectVarNoun noun
            DefnSymbolicPredicate _predi marker vars ->
                noteMissingHint PredicateHint marker (length vars)
                    <> foldMap (collectExpr . ExprVar) vars
            DefnRel _x rel params _y ->
                noteMissingHint RelationHint (relationSymbolMarker rel) (length params)

        collectAbbreviation :: Abbreviation -> MissingHintMap
        collectAbbreviation = \case
            AbbreviationAdj _var _adj stmt ->
                collectStmt stmt
            AbbreviationVerb _var _verb stmt ->
                collectStmt stmt
            AbbreviationNoun _var _noun stmt ->
                collectStmt stmt
            AbbreviationRel _x rel params _y stmt ->
                noteMissingHint RelationHint (relationSymbolMarker rel) (length params)
                    <> collectStmt stmt
            AbbreviationFun _fun bodyTerm ->
                collectTerm bodyTerm
            AbbreviationEq symb expr ->
                collectSymbolPattern symb
                    <> collectExpr expr

        collectDatatype :: Datatype -> MissingHintMap
        collectDatatype Datatype{..} =
            collectExpr datatypeHeadExpr
                <> foldMap collectDatatypeClause datatypeClauses

        collectDatatypeClause :: DatatypeClause -> MissingHintMap
        collectDatatypeClause DatatypeClause{..} =
            collectExpr datatypeClauseConstructorExpr
                <> collectExpr datatypeClauseTargetExpr
                <> foldMap (collectExpr . snd) datatypeClausePremises

        collectInductive :: Inductive -> MissingHintMap
        collectInductive Inductive{..} =
            collectSymbolPattern inductiveSymbolPattern
                <> collectExpr inductiveDomain
                <> foldMap collectIntroRule inductiveIntros

        collectIntroRule :: IntroRule -> MissingHintMap
        collectIntroRule IntroRule{..} =
            foldMap collectFormula introConditions
                <> collectFormula introResult

        collectSignature :: Signature -> MissingHintMap
        collectSignature = \case
            SignatureAdj _var adj ->
                collectVarAdj adj
            SignatureVerb _var verb ->
                collectVarVerb verb
            SignatureNoun _var noun ->
                collectVarNoun noun
            SignatureSymbolic symb np ->
                collectSymbolPattern symb
                    <> collectNounPhraseMaybe np

        collectStructDefn :: StructDefn -> MissingHintMap
        collectStructDefn StructDefn{structAssumes} =
            foldMap (collectStmt . snd) structAssumes

        collectProof :: Proof -> MissingHintMap
        collectProof = \case
            Omitted ->
                mempty
            Qed{} ->
                mempty
            ByCase _loc cases ->
                foldMap collectCase cases
            ByContradiction _loc proof ->
                collectProof proof
            BySetInduction _loc maybeTerm proof ->
                foldMap collectTerm maybeTerm
                    <> collectProof proof
            ByOrdInduction _loc proof ->
                collectProof proof
            Assume _loc stmt proof ->
                collectStmt stmt
                    <> collectProof proof
            FixSymbolic _loc _vars bound proof ->
                collectBound bound
                    <> collectProof proof
            FixSuchThat _loc _vars stmt proof ->
                collectStmt stmt
                    <> collectProof proof
            Calc _loc maybeQuant calc proof ->
                foldMap collectCalcQuantifier maybeQuant
                    <> collectCalc calc
                    <> collectProof proof
            TakeVar _loc _vars bound stmt _justification proof ->
                collectBound bound
                    <> collectStmt stmt
                    <> collectProof proof
            TakeNoun _loc np _justification proof ->
                collectNounPhraseList np
                    <> collectProof proof
            Have _loc maybeStmt stmt _justification proof ->
                foldMap collectStmt maybeStmt
                    <> collectStmt stmt
                    <> collectProof proof
            Suffices _loc stmt _justification proof ->
                collectStmt stmt
                    <> collectProof proof
            Subclaim _loc stmt subproof proof ->
                collectStmt stmt
                    <> collectProof subproof
                    <> collectProof proof
            Define _loc _var expr proof ->
                collectExpr expr
                    <> collectProof proof
            DefineFunction _loc _fun _arg value _boundVar boundExpr proof ->
                collectExpr value
                    <> collectExpr boundExpr
                    <> collectProof proof
            DefineFunctionLocal _loc _fun _arg _target _domVar _codVar rules proof ->
                foldMap collectLocalFunctionRule rules
                    <> collectProof proof

        collectLocalFunctionRule :: (Expr, Formula) -> MissingHintMap
        collectLocalFunctionRule (ruleTerm, formula) =
            collectExpr ruleTerm
                <> collectFormula formula

        collectCase :: Case -> MissingHintMap
        collectCase Case{caseOf, caseProof} =
            collectStmt caseOf
                <> collectProof caseProof

        collectCalcQuantifier :: CalcQuantifier -> MissingHintMap
        collectCalcQuantifier (CalcQuantifier _vars bound maybeStmt) =
            collectBound bound
                <> foldMap collectStmt maybeStmt

        collectCalc :: Calc -> MissingHintMap
        collectCalc = \case
            Equation expr steps ->
                collectExpr expr
                    <> foldMap (collectExpr . fst) steps
            Biconditionals phi steps ->
                collectFormula phi
                    <> foldMap (collectFormula . fst) steps

        collectStmt :: Stmt -> MissingHintMap
        collectStmt = \case
            StmtFormula phi ->
                collectFormula phi
            StmtVerbPhrase terms verbPhrase ->
                collectTerms terms
                    <> collectVerbPhrase verbPhrase
            StmtNoun terms np ->
                collectTerms terms
                    <> collectNounPhraseMaybe np
            StmtStruct stmtTerm _structPhrase ->
                collectTerm stmtTerm
            StmtNeg _loc stmt ->
                collectStmt stmt
            StmtExists _loc np ->
                collectNounPhraseList np
            StmtConnected _conn _loc stmt1 stmt2 ->
                collectStmt stmt1
                    <> collectStmt stmt2
            StmtQuantPhrase _loc qp stmt ->
                collectQuantPhrase qp
                    <> collectStmt stmt
            SymbolicQuantified _loc _quant _vars bound suchThat stmt ->
                collectBound bound
                    <> foldMap collectStmt suchThat
                    <> collectStmt stmt

        collectQuantPhrase :: QuantPhrase -> MissingHintMap
        collectQuantPhrase (QuantPhrase _quant np) =
            collectNounPhraseList np

        collectAsm :: Asm -> MissingHintMap
        collectAsm = \case
            AsmSuppose stmt ->
                collectStmt stmt
            AsmLetNoun _vars np ->
                collectNounPhraseMaybe np
            AsmLetIn _vars expr ->
                collectExpr expr
            AsmLetThe _var fun ->
                collectFun fun
            AsmLetEq _var expr ->
                collectExpr expr
            AsmLetStruct{} ->
                mempty

        collectTerm :: Term -> MissingHintMap
        collectTerm = \case
            TermExpr expr ->
                collectExpr expr
            TermFun fun ->
                collectFun fun
            TermIota _loc _var stmt ->
                collectStmt stmt
            TermQuantified _quant _loc np ->
                collectNounPhraseMaybe np

        collectNounPhraseMaybe :: NounPhrase Maybe -> MissingHintMap
        collectNounPhraseMaybe (NounPhrase ls noun _maybeName rs maybeSuchThat) =
            collectAdjLs ls
                <> collectNoun noun
                <> collectAdjRs rs
                <> foldMap collectStmt maybeSuchThat

        collectNounPhraseList :: NounPhrase [] -> MissingHintMap
        collectNounPhraseList (NounPhrase ls noun _names rs maybeSuchThat) =
            collectAdjLs ls
                <> collectNoun noun
                <> collectAdjRs rs
                <> foldMap collectStmt maybeSuchThat

        collectAdjL :: AdjLOf Term -> MissingHintMap
        collectAdjL (AdjL _loc _item args) =
            collectTerms args

        collectAdjR :: AdjROf Term -> MissingHintMap
        collectAdjR = \case
            AdjR _loc _item args ->
                collectTerms args
            AttrRThat verbPhrase ->
                collectVerbPhrase verbPhrase

        collectAdj :: AdjOf Term -> MissingHintMap
        collectAdj (Adj _loc _item args) =
            collectTerms args

        collectVarAdj :: AdjOf VarSymbol -> MissingHintMap
        collectVarAdj _adj =
            mempty

        collectVerb :: VerbOf Term -> MissingHintMap
        collectVerb (Verb _loc _item args) =
            collectTerms args

        collectVarVerb :: VerbOf VarSymbol -> MissingHintMap
        collectVarVerb _verb =
            mempty

        collectVerbPhrase :: VerbPhrase -> MissingHintMap
        collectVerbPhrase = \case
            VPVerb verb ->
                collectVerb verb
            VPAdj adjs ->
                foldMap collectAdj adjs
            VPVerbNot verb ->
                collectVerb verb
            VPAdjNot adjs ->
                foldMap collectAdj adjs

        collectNoun :: NounOf Term -> MissingHintMap
        collectNoun (Noun _loc _item args) =
            collectTerms args

        collectVarNoun :: NounOf VarSymbol -> MissingHintMap
        collectVarNoun _noun =
            mempty

        collectFun :: FunOf Term -> MissingHintMap
        collectFun Fun{funArgs} =
            collectTerms funArgs

        collectBound :: Bound -> MissingHintMap
        collectBound = \case
            Unbounded ->
                mempty
            Bounded _loc _sign rel expr ->
                collectRelation rel
                    <> collectExpr expr

        collectFormula :: Formula -> MissingHintMap
        collectFormula = \case
            FormulaChain chain ->
                collectChain chain
            FormulaPredicate _loc _predi marker exprs ->
                noteMissingHint PredicateHint marker (length exprs)
                    <> collectExprs exprs
            Connected _loc _conn phi psi ->
                collectFormula phi
                    <> collectFormula psi
            FormulaNeg _loc phi ->
                collectFormula phi
            FormulaQuantified _loc _quant _vars bound phi ->
                collectBound bound
                    <> collectFormula phi
            PropositionalConstant{} ->
                mempty

        collectChain :: Chain -> MissingHintMap
        collectChain = \case
            ChainBase lhs _sign rel rhs ->
                collectExprs lhs
                    <> collectRelation rel
                    <> collectExprs rhs
            ChainCons lhs _sign rel chain ->
                collectExprs lhs
                    <> collectRelation rel
                    <> collectChain chain

        collectRelation :: Relation -> MissingHintMap
        collectRelation = \case
            Relation _loc symbol relParams ->
                noteMissingHint RelationHint (relationSymbolMarker symbol) (length relParams)
                    <> collectExprs relParams
            RelationExpr _loc expr ->
                collectExpr expr

        collectExpr :: Expr -> MissingHintMap
        collectExpr = \case
            ExprVar{} ->
                mempty
            ExprInteger{} ->
                mempty
            ExprOp _loc item args ->
                noteMissingHint OperatorHint (mixfixMarker item) (length args)
                    <> collectExprs args
            ExprStructOp _loc symb maybeExpr ->
                noteMissingHint StructOpHint (structMarker symb) (length (maybeToList maybeExpr))
                    <> foldMap collectExpr maybeExpr
            ExprFiniteSet _loc exprs ->
                collectExprs exprs
            ExprSep _loc _var boundExpr stmt ->
                collectExpr boundExpr
                    <> collectStmt stmt
            ExprReplace _loc expr bounds maybeStmt ->
                collectExpr expr
                    <> foldMap (collectExpr . snd) bounds
                    <> foldMap collectStmt maybeStmt
            ExprReplacePred _loc _rangeVar _domVar domExpr stmt ->
                collectExpr domExpr
                    <> collectStmt stmt

        collectSymbolPattern :: SymbolPattern -> MissingHintMap
        collectSymbolPattern (SymbolPattern symbol vars) =
            noteMissingHint OperatorHint (mixfixMarker symbol) (length vars)

        collectAsms :: [Asm] -> MissingHintMap
        collectAsms =
            foldMap collectAsm

        collectTerms :: Foldable t => t Term -> MissingHintMap
        collectTerms =
            foldMap collectTerm

        collectAdjLs :: [AdjLOf Term] -> MissingHintMap
        collectAdjLs =
            foldMap collectAdjL

        collectAdjRs :: [AdjROf Term] -> MissingHintMap
        collectAdjRs =
            foldMap collectAdjR

        collectExprs :: Foldable t => t Expr -> MissingHintMap
        collectExprs =
            foldMap collectExpr

formatMissingHintWarning :: MissingHintMap -> Maybe Text
formatMissingHintWarning missingHints
    | null parts = Nothing
    | otherwise = Just ("WARNING: missing render hints: " <> Text.intercalate "; " parts)
    where
        missingHintMap = unMissingHintMap missingHints

        parts =
            [ label <> "(" <> Text.intercalate ", " (markerText <$> Set.toAscList markers) <> ")"
            | (category, label) <- categoryLabels
            , Just markers <- [Map.lookup category missingHintMap]
            , not (Set.null markers)
            ]

        categoryLabels :: [(HintCategory, Text)]
        categoryLabels =
            [ (OperatorHint, "operators")
            , (RelationHint, "relations")
            , (PredicateHint, "predicates")
            , (StructOpHint, "structops")
            ]


parseHints :: Text -> HintMap
parseHints source = Map.fromList (parseLine <$> zip [1 :: Int ..] relevantLines)
    where
        relevantLines = [line | line <- Text.lines source, not (Text.all isSpace line)]

        parseLine :: (Int, Text) -> ((HintCategory, Marker, Int), RenderHint)
        parseLine (lineNo, line) = case Text.splitOn "\t" line of
            [categoryText, markerName, arityText, templateText] ->
                let category = parseCategory lineNo categoryText
                    marker = Marker markerName
                    arity = parseArity lineNo arityText
                    template = parseTemplate lineNo templateText
                in ((category, marker, arity), RenderHint arity template)
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


renderBlock :: HintMap -> ReferenceContext -> BlockRenderInfo -> Html ()
renderBlock hints references (_index, block, blockId) = case block of
    BlockAxiom _loc title marker axiom ->
        renderCustomBlock blockId "axiom-" "Axiom" (Just marker) title (renderAxiom hints axiom)
    BlockClaim kind _loc title marker claim ->
        renderCustomBlock blockId (claimKindElement kind) (claimKindPrefix kind) (Just marker) title (renderClaim hints claim)
    BlockProof _start proof _end ->
        renderProofBlock hints references proof
    BlockDefn _loc title marker defn ->
        renderCustomBlock blockId "definition-" "Definition" (Just marker) title (renderDefn hints defn)
    BlockAbbr _loc title marker abbr ->
        renderCustomBlock blockId "abbreviation-" "Abbreviation" (Just marker) title (renderAbbreviation hints abbr)
    BlockData _loc title marker datatype ->
        renderCustomBlock blockId "datatype-" "Datatype" (Just marker) title (renderDatatype hints datatype)
    BlockInductive _loc title marker ind ->
        renderCustomBlock blockId "inductive-" "Inductive" (Just marker) title (renderInductive hints ind)
    BlockSig _loc title marker asms sig ->
        renderCustomBlock blockId "signature-" "Signature" (Just marker) title (renderSignatureBlock hints asms sig)
    BlockStruct _loc title marker structDefn ->
        renderCustomBlock blockId "struct-" "Structure" (Just marker) title (renderStructDefn hints structDefn)

renderTocEntry :: (Int, Text, Block) -> Html ()
renderTocEntry (index, blockId, block) =
    li_ do
        a_ [href_ ("#" <> blockId)] do
            span_ (toHtml (blockPrefixText block))
            case formatMarker (blockMarkerOf block) of
                Nothing ->
                    when (blockNeedsIndexLabel block) do
                        code_ (toHtml (Text.pack (show index)))
                Just marker ->
                    code_ (toHtml marker)

includeInToc :: Block -> Bool
includeInToc = \case
    BlockProof{} -> False
    _ -> True

renderCustomBlock :: Text -> Text -> Text -> Maybe Marker -> Maybe BlockTitle -> Html () -> Html ()
renderCustomBlock blockId name prefix mmarker mtitle body =
    term name (id_ blockId : previewTargetAttributes prefix mmarker mtitle) do
        renderBlockLead prefix mmarker mtitle True
        body

previewTargetAttributes :: Text -> Maybe Marker -> Maybe BlockTitle -> [Attributes]
previewTargetAttributes prefix mmarker mtitle =
    case formatMarker mmarker of
        Nothing ->
            []
        Just marker ->
            [ makeAttributes "data-preview-kind" prefix
            , makeAttributes "data-preview-label" marker
            ]
            <> case formatBlockTitle mtitle of
                Nothing ->
                    []
                Just title ->
                    [makeAttributes "data-preview-title" title]

renderProofBlock :: HintMap -> ReferenceContext -> Proof -> Html ()
renderProofBlock hints references proof
    | proofStepCount proof >= proofCollapseThreshold =
        term "proof-" do
            details_ do
                summary_ (renderBlockLead "Proof" Nothing Nothing False)
                renderProof hints references proof
    | otherwise =
        term "proof-" do
            renderBlockLead "Proof" Nothing Nothing True
            renderProof hints references proof

blockAnchorId :: Int -> Block -> Text
blockAnchorId index block =
    case formatMarker (blockMarkerOf block) of
        Just marker -> marker
        Nothing -> sanitizeIdFragment (Text.toLower (blockPrefixText block) <> "-" <> Text.pack (show index))

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
    BlockData _ _ marker _ -> Just marker
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
    BlockData _ title _ _ -> title
    BlockInductive _ title _ _ -> title
    BlockSig _ title _ _ _ -> title
    BlockStruct _ title _ _ -> title

blockNeedsIndexLabel :: Block -> Bool
blockNeedsIndexLabel block = case (formatMarker (blockMarkerOf block), formatBlockTitle (blockTitleOf block)) of
    (Nothing, Nothing) -> True
    _ -> False

renderBlockLead :: Text -> Maybe Marker -> Maybe BlockTitle -> Bool -> Html ()
renderBlockLead prefix mmarker mtitle withTrailingSpace =
    term "head-" do
        toHtml prefix
        case formatMarker mmarker of
            Nothing -> skip
            Just marker ->
                term "id-" (toHtml marker)
        case formatBlockTitle mtitle of
            Nothing ->
                toHtml ("." <> suffix)
            Just title -> do
                toHtml (" (" :: Text)
                term "title-" (toHtml title)
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
    Proposition -> "proposition-"
    Theorem -> "theorem-"
    Lemma -> "lemma-"
    Corollary -> "corollary-"
    PlainClaim -> "claim-"

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
            toHtml ("Suppose " :: Text)
            renderAsmList hints asms
            toHtml (". Then " :: Text)
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
            inlineMath do
                renderSymbolPatternMath hints symb
                moText "="
                renderExprMathRow hints expr
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
    DefnSymbolicPredicate predi marker vars ->
        inlineMath
            ( renderHintedMathRow
                hints
                PredicateHint
                marker
                (ExprVar <$> toList vars)
                (renderPrefixPredicateFallback predi (renderVarMath <$> toList vars))
            )
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
            inlineMath (renderExprMathRow hints expr)
            toHtml ("." :: Text)

renderDatatype :: HintMap -> Datatype -> Html ()
renderDatatype hints Datatype{..} = do
    toHtml ("Datatype of " :: Text)
    inlineMath (renderExprMathRow hints datatypeHeadExpr)
    toHtml ("." :: Text)
    ul_ do
        traverse_ renderDatatypeClause (toList datatypeClauses)
    case datatypeDerivedFacts (Datatype{..}) of
        [] ->
            skip
        derivedFacts ->
            details_ do
                summary_ (toHtml ("Derived facts" :: Text))
                ul_ [class_ "datatype-derived-facts"] do
                    traverse_ (renderDatatypeDerivedFact hints) derivedFacts
    where
        renderDatatypeClause :: DatatypeClause -> Html ()
        renderDatatypeClause DatatypeClause{..} = li_ do
            inlineMath do
                renderRelationApplication hints Positive [datatypeClauseConstructorExpr] (Relation Nowhere ElementSymbol []) [datatypeClauseTargetExpr]
            case datatypeClausePremises of
                [] ->
                    toHtml ("." :: Text)
                premises -> do
                    toHtml (" for " :: Text)
                    joinHtml (toHtml (" and " :: Text)) (renderDatatypePremise <$> premises)
                    toHtml ("." :: Text)

        renderDatatypePremise :: (VarSymbol, Expr) -> Html ()
        renderDatatypePremise (x, domain) =
            inlineMath (renderRelationApplication hints Positive [ExprVar x] (Relation Nowhere ElementSymbol []) [domain])

renderDatatypeDerivedFact :: HintMap -> DatatypeDerivedFact -> Html ()
renderDatatypeDerivedFact hints DatatypeDerivedFact{datatypeDerivedFactMarker, datatypeDerivedFactFormula} =
    li_ (id_ (markerText datatypeDerivedFactMarker) : previewTargetAttributes "Datatype Fact" (Just datatypeDerivedFactMarker) Nothing) do
        term "head-" do
            code_ (toHtml (markerText datatypeDerivedFactMarker))
            toHtml (": " :: Text)
        inlineMath (renderFormulaMath hints datatypeDerivedFactFormula)
        toHtml ("." :: Text)

data DatatypeDerivedFact = DatatypeDerivedFact
    { datatypeDerivedFactMarker :: Marker
    , datatypeDerivedFactFormula :: Formula
    }

data DatatypeRenderInfo = DatatypeRenderInfo
    { datatypeFactBaseText :: Text
    , datatypeCarrierExpr :: Expr
    , datatypeRenderClauses :: NonEmpty DatatypeRenderClause
    }

data DatatypeRenderClause = DatatypeRenderClause
    { datatypeRenderClauseSymbolText :: Text
    , datatypeRenderClauseExpr :: Expr
    , datatypeRenderClauseArgs :: [VarSymbol]
    , datatypeRenderClausePremises :: [(VarSymbol, Expr)]
    }

datatypeDerivedFacts :: Datatype -> [DatatypeDerivedFact]
datatypeDerivedFacts datatype = case datatypeRenderInfo datatype of
    Nothing ->
        []
    Just info ->
        datatypeIntroFacts info
            <> datatypeDistinctFacts info
            <> datatypeInjectiveFacts info
            <> [ DatatypeDerivedFact (datatypeCasesMarker info) (datatypeCasesFormula info)
               , DatatypeDerivedFact (datatypeInductMarker info) (datatypeInductFormula info)
               ]

datatypeRenderInfo :: Datatype -> Maybe DatatypeRenderInfo
datatypeRenderInfo Datatype{datatypeHeadExpr, datatypeClauses} = do
    datatypeFactBaseText <- datatypeFactBase datatypeHeadExpr
    datatypeRenderClauses <- traverse datatypeRenderClause datatypeClauses
    pure DatatypeRenderInfo{datatypeFactBaseText, datatypeCarrierExpr = datatypeHeadExpr, datatypeRenderClauses}

datatypeRenderClause :: DatatypeClause -> Maybe DatatypeRenderClause
datatypeRenderClause DatatypeClause{datatypeClauseConstructorExpr, datatypeClausePremises} = do
    (constructorSymbol, constructorArgs) <- exprOp datatypeClauseConstructorExpr
    datatypeRenderClauseArgs <- traverse exprVar constructorArgs
    let datatypeRenderClauseSymbolText = markerText (mixfixMarker constructorSymbol)
    pure DatatypeRenderClause
        { datatypeRenderClauseSymbolText
        , datatypeRenderClauseExpr = datatypeClauseConstructorExpr
        , datatypeRenderClauseArgs
        , datatypeRenderClausePremises = datatypeClausePremises
        }

datatypeIntroFacts :: DatatypeRenderInfo -> [DatatypeDerivedFact]
datatypeIntroFacts info =
    [ DatatypeDerivedFact (datatypeIntroMarker info clause) (datatypeIntroFormula info clause)
    | clause <- NonEmpty.toList (datatypeRenderClauses info)
    ]

datatypeDistinctFacts :: DatatypeRenderInfo -> [DatatypeDerivedFact]
datatypeDistinctFacts info =
    [ DatatypeDerivedFact (datatypeDistinctMarker info leftClause rightClause) (datatypeDistinctFormula leftClause rightClause)
    | (leftClause, rightClause) <- unorderedPairs (NonEmpty.toList (datatypeRenderClauses info))
    ]

datatypeInjectiveFacts :: DatatypeRenderInfo -> [DatatypeDerivedFact]
datatypeInjectiveFacts info =
    [ DatatypeDerivedFact (datatypeInjectiveMarker info clause) (datatypeInjectiveFormula clause)
    | clause <- NonEmpty.toList (datatypeRenderClauses info)
    , not (null (datatypeRenderClauseArgs clause))
    ]

datatypeIntroMarker :: DatatypeRenderInfo -> DatatypeRenderClause -> Marker
datatypeIntroMarker info clause =
    Marker (datatypeFactBaseText info <> "_" <> datatypeRenderClauseSymbolText clause <> "_intro")

datatypeDistinctMarker :: DatatypeRenderInfo -> DatatypeRenderClause -> DatatypeRenderClause -> Marker
datatypeDistinctMarker info leftClause rightClause =
    Marker
        ( datatypeFactBaseText info
            <> "_"
            <> datatypeRenderClauseSymbolText leftClause
            <> "_"
            <> datatypeRenderClauseSymbolText rightClause
            <> "_distinct"
        )

datatypeInjectiveMarker :: DatatypeRenderInfo -> DatatypeRenderClause -> Marker
datatypeInjectiveMarker info clause =
    Marker (datatypeFactBaseText info <> "_" <> datatypeRenderClauseSymbolText clause <> "_injective")

datatypeCasesMarker :: DatatypeRenderInfo -> Marker
datatypeCasesMarker info =
    Marker (datatypeFactBaseText info <> "_cases")

datatypeInductMarker :: DatatypeRenderInfo -> Marker
datatypeInductMarker info =
    Marker (datatypeFactBaseText info <> "_induct")

datatypeFactBase :: Expr -> Maybe Text
datatypeFactBase expr = do
    (datatypeSymbol, datatypeArgs) <- exprOp expr
    guard (null datatypeArgs)
    pure (markerText (mixfixMarker datatypeSymbol))

datatypeIntroFormula :: DatatypeRenderInfo -> DatatypeRenderClause -> Formula
datatypeIntroFormula info clause =
    forallIfNeeded premiseVars (impliesFrom premiseFormulas conclusion)
    where
        premiseVars = datatypeClausePremiseVars clause
        premiseFormulas = datatypeClausePremiseFormulas clause
        conclusion = elementOfFormula (datatypeRenderClauseExpr clause) (datatypeCarrierExpr info)

datatypeDistinctFormula :: DatatypeRenderClause -> DatatypeRenderClause -> Formula
datatypeDistinctFormula leftClause rightClause =
    forallIfNeeded (leftVars <> rightVars) (notEqualsFormula (datatypeRenderClauseExpr leftClause) rightTerm)
    where
        leftVars = datatypeRenderClauseArgs leftClause
        rightVars = renameDatatypeVars (Set.fromList leftVars) (datatypeRenderClauseArgs rightClause)
        rightTerm = substituteExpr (Map.fromList (zip (datatypeRenderClauseArgs rightClause) (ExprVar <$> rightVars))) (datatypeRenderClauseExpr rightClause)

datatypeInjectiveFormula :: DatatypeRenderClause -> Formula
datatypeInjectiveFormula clause =
    forallIfNeeded (leftVars <> rightVars) (impliesFormula (equalsFormula leftTerm rightTerm) (formulaConjunction equalities))
    where
        leftVars = datatypeRenderClauseArgs clause
        rightVars = renameDatatypeVars (Set.fromList leftVars) leftVars
        leftTerm = datatypeRenderClauseExpr clause
        rightTerm = substituteExpr (Map.fromList (zip leftVars (ExprVar <$> rightVars))) leftTerm
        equalities =
            zipWith (\leftVar rightVar -> equalsFormula (ExprVar leftVar) (ExprVar rightVar)) leftVars rightVars

datatypeCasesFormula :: DatatypeRenderInfo -> Formula
datatypeCasesFormula info =
    forallIfNeeded [witnessVar] (impliesFormula (elementOfFormula (ExprVar witnessVar) (datatypeCarrierExpr info)) (formulaDisjunction disjuncts))
    where
        usedVars = datatypeUsedVars info
        witnessVar = freshDatatypeVar usedVars "x"
        disjuncts = datatypeCaseDisjunct witnessVar <$> NonEmpty.toList (datatypeRenderClauses info)
        datatypeCaseDisjunct x clause =
            existsIfNeeded premiseVars (formulaConjunction (premises <> [equalsFormula (ExprVar x) (datatypeRenderClauseExpr clause)]))
            where
                premiseVars = datatypeClausePremiseVars clause
                premises = datatypeClausePremiseFormulas clause

datatypeInductFormula :: DatatypeRenderInfo -> Formula
datatypeInductFormula info =
    forallIfNeeded [subsetVar] (impliesFrom closureAssumptions conclusion)
    where
        usedVars = datatypeUsedVars info
        subsetVar = freshDatatypeVar usedVars "S"
        witnessVar = freshDatatypeVar (Set.insert subsetVar usedVars) "x"
        conclusion =
            forallIfNeeded [witnessVar]
                (impliesFormula
                    (elementOfFormula (ExprVar witnessVar) (datatypeCarrierExpr info))
                    (elementOfFormula (ExprVar witnessVar) (ExprVar subsetVar))
                )
        closureAssumptions = datatypeInductionClosure info subsetVar <$> NonEmpty.toList (datatypeRenderClauses info)

datatypeInductionClosure :: DatatypeRenderInfo -> VarSymbol -> DatatypeRenderClause -> Formula
datatypeInductionClosure info subsetVar clause =
    forallIfNeeded premiseVars (impliesFrom inductionPremises conclusion)
    where
        premiseVars = datatypeClausePremiseVars clause
        inductionPremises = datatypeInductionPremise info subsetVar <$> datatypeRenderClausePremises clause
        conclusion = elementOfFormula (datatypeRenderClauseExpr clause) (ExprVar subsetVar)

datatypeInductionPremise :: DatatypeRenderInfo -> VarSymbol -> (VarSymbol, Expr) -> Formula
datatypeInductionPremise info subsetVar (x, domain)
    | sameDatatypeCarrier domain (datatypeCarrierExpr info) =
        elementOfFormula (ExprVar x) (ExprVar subsetVar)
    | otherwise =
        elementOfFormula (ExprVar x) domain

datatypeClausePremiseVars :: DatatypeRenderClause -> [VarSymbol]
datatypeClausePremiseVars DatatypeRenderClause{datatypeRenderClausePremises} =
    fst <$> datatypeRenderClausePremises

datatypeClausePremiseFormulas :: DatatypeRenderClause -> [Formula]
datatypeClausePremiseFormulas DatatypeRenderClause{datatypeRenderClausePremises} =
    [ elementOfFormula (ExprVar x) domain
    | (x, domain) <- datatypeRenderClausePremises
    ]

datatypeUsedVars :: DatatypeRenderInfo -> Set VarSymbol
datatypeUsedVars info =
    Set.fromList
        [ var
        | clause <- NonEmpty.toList (datatypeRenderClauses info)
        , var <- datatypeRenderClauseArgs clause <> datatypeClausePremiseVars clause
        ]

sameDatatypeCarrier :: Expr -> Expr -> Bool
sameDatatypeCarrier left right = case (exprOp left, exprOp right) of
    (Just (leftSymbol, []), Just (rightSymbol, [])) ->
        leftSymbol == rightSymbol
    _ ->
        False

exprOp :: Expr -> Maybe (MixfixItem, [Expr])
exprOp = \case
    ExprOp _loc symbol args ->
        Just (symbol, args)
    _ ->
        Nothing

exprVar :: Expr -> Maybe VarSymbol
exprVar = \case
    ExprVar x ->
        Just x
    _ ->
        Nothing

substituteExpr :: Map.Map VarSymbol Expr -> Expr -> Expr
substituteExpr env = \case
    ExprVar x ->
        fromMaybe (ExprVar x) (Map.lookup x env)
    ExprInteger loc n ->
        ExprInteger loc n
    ExprOp loc symbol args ->
        ExprOp loc symbol (substituteExpr env <$> args)
    ExprStructOp loc symbol expr ->
        ExprStructOp loc symbol (substituteExpr env <$> expr)
    ExprFiniteSet loc exprs ->
        ExprFiniteSet loc (substituteExpr env <$> exprs)
    ExprSep loc x bound stmt ->
        ExprSep loc x (substituteExpr env bound) stmt
    ExprReplace loc expr bounds maybeStmt ->
        ExprReplace loc (substituteExpr env expr) ((\(x, bound) -> (x, substituteExpr env bound)) <$> bounds) maybeStmt
    ExprReplacePred loc x y expr stmt ->
        ExprReplacePred loc x y (substituteExpr env expr) stmt

elementOfFormula :: Expr -> Expr -> Formula
elementOfFormula left right =
    FormulaChain (ChainBase (left :| []) Positive (Relation Nowhere ElementSymbol []) (right :| []))

equalsFormula :: Expr -> Expr -> Formula
equalsFormula left right =
    FormulaChain (ChainBase (left :| []) Positive (Relation Nowhere EqSymbol []) (right :| []))

notEqualsFormula :: Expr -> Expr -> Formula
notEqualsFormula left right =
    FormulaChain (ChainBase (left :| []) Positive (Relation Nowhere NeqSymbol []) (right :| []))

impliesFormula :: Formula -> Formula -> Formula
impliesFormula left right =
    Connected Nowhere Implication left right

formulaConjunction :: [Formula] -> Formula
formulaConjunction = \case
    [] ->
        PropositionalConstant Nowhere IsTop
    phi : rest ->
        foldl' (\left right -> Connected Nowhere Conjunction left right) phi rest

formulaDisjunction :: [Formula] -> Formula
formulaDisjunction = \case
    [] ->
        PropositionalConstant Nowhere IsBottom
    phi : rest ->
        foldl' (\left right -> Connected Nowhere Disjunction left right) phi rest

forallIfNeeded :: [VarSymbol] -> Formula -> Formula
forallIfNeeded [] phi = phi
forallIfNeeded vars phi =
    FormulaQuantified Nowhere Universally (NonEmpty.fromList vars) Unbounded phi

existsIfNeeded :: [VarSymbol] -> Formula -> Formula
existsIfNeeded [] phi = phi
existsIfNeeded vars phi =
    FormulaQuantified Nowhere Existentially (NonEmpty.fromList vars) Unbounded phi

impliesFrom :: [Formula] -> Formula -> Formula
impliesFrom [] conclusion = conclusion
impliesFrom premises conclusion = impliesFormula (formulaConjunction premises) conclusion

unorderedPairs :: [a] -> [(a, a)]
unorderedPairs = \case
    [] -> []
    x : xs -> [(x, y) | y <- xs] <> unorderedPairs xs

renameDatatypeVars :: Set VarSymbol -> [VarSymbol] -> [VarSymbol]
renameDatatypeVars _ [] = []
renameDatatypeVars used (x:xs) =
    let x' = freshDatatypeLikeVar used x
    in x' : renameDatatypeVars (Set.insert x' used) xs

freshDatatypeLikeVar :: Set VarSymbol -> VarSymbol -> VarSymbol
freshDatatypeLikeVar used = \case
    NamedVar name ->
        freshDatatypeVar used (name <> "_rhs")
    FreshVar n ->
        freshDatatypeVar used ("_" <> Text.pack (show n) <> "_rhs")

freshDatatypeVar :: Set VarSymbol -> Text -> VarSymbol
freshDatatypeVar used base =
    List.head
        [ NamedVar candidate
        | candidate <- base : [base <> Text.pack (show n) | n <- [(1 :: Int) ..]]
        , NamedVar candidate `Set.notMember` used
        ]

renderInductive :: HintMap -> Inductive -> Html ()
renderInductive hints Inductive{..} = do
    toHtml ("Inductive definition of " :: Text)
    renderSymbolPatternInline hints inductiveSymbolPattern
    toHtml (" over " :: Text)
    inlineMath (renderExprMathRow hints inductiveDomain)
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
            inlineMath (joinHtml (moText ",") (renderStructSymbolName <$> structFixes))
            toHtml ("." :: Text)
    when (not (null structAssumes)) do
        ul_ do
            for_ structAssumes \(marker, stmt) -> li_ do
                toHtml (markerText marker)
                toHtml (": " :: Text)
                renderStmtInline hints stmt
                toHtml ("." :: Text)


buildPreviewMap :: FilePath -> Set Marker -> AnchorMap -> [Block] -> PreviewMap
buildPreviewMap inputPath referencedMarkers anchors blocks =
    Map.fromList
        [ (marker, PreviewEntry{..})
        | (index, target) <- zip [1 :: Int ..] (concatMap referenceTargetsOfBlockRenderInfo blockInfos)
        , let marker = targetMarker target
        , marker `Set.member` referencedMarkers
        , marker `Map.notMember` anchors
        , let previewMarker = marker
        , let previewKind = targetKind target
        , let previewTitle = targetTitle target
        , let previewSourceFile = targetSourceFile target
        , let previewIsImported = previewSourceFile /= inputPath && previewSourceFile /= "<nowhere>"
        , previewIsImported
        , let previewId = "reference-preview-" <> Text.pack (show index)
        , let previewBody = targetBody target
        ]
    where
        indexedBlocks = zip [1 :: Int ..] blocks
        blockInfos = [(index, block, blockAnchorId index block) | (index, block) <- indexedBlocks]

renderPreviewStore :: HintMap -> PreviewMap -> Html ()
renderPreviewStore hints previews =
    div_ [class_ "reference-preview-store", makeAttributes "aria-hidden" "true"] do
        traverse_ (renderPreviewEntry hints) (Map.elems previews)

renderPreviewEntry :: HintMap -> PreviewEntry -> Html ()
renderPreviewEntry hints PreviewEntry{..} =
    div_ [id_ previewId, class_ "reference-preview-template"] do
        div_ [class_ "reference-preview-heading"] do
            toHtml previewKind
            toHtml (" " :: Text)
            code_ (toHtml (markerText previewMarker))
            case previewTitle of
                Nothing ->
                    skip
                Just title -> do
                    toHtml (" (" :: Text)
                    toHtml title
                    toHtml (")" :: Text)
        when previewIsImported do
            div_ [class_ "reference-preview-source"] do
                toHtml ("from " :: Text)
                code_ (toHtml (Text.pack previewSourceFile))
        div_ [class_ "reference-preview-body"] do
            previewBody hints

externalReferenceHref :: FilePath -> PreviewEntry -> Text
externalReferenceHref inputPath PreviewEntry{..} =
    "/" <> routePath <> "#" <> markerText previewMarker
    where
        routePath = Text.pack (prefixRouteNamespace inputPath (dropExtension previewSourceFile))

prefixRouteNamespace :: FilePath -> FilePath -> FilePath
prefixRouteNamespace inputPath sourceRoute =
    case topLevelRouteNamespace inputPath of
        Nothing ->
            sourceRoute
        Just namespace
            | startsWithTopLevelNamespace namespace sourceRoute ->
                sourceRoute
            | otherwise ->
                namespace </> sourceRoute

topLevelRouteNamespace :: FilePath -> Maybe FilePath
topLevelRouteNamespace inputPath =
    case List.filter (/= ".") (splitDirectories inputPath) of
        [] ->
            Nothing
        [_] ->
            Nothing
        namespace : _ ->
            Just namespace

startsWithTopLevelNamespace :: FilePath -> FilePath -> Bool
startsWithTopLevelNamespace namespace sourceRoute =
    case List.filter (/= ".") (splitDirectories sourceRoute) of
        first : _ ->
            first == namespace
        [] ->
            False

renderPreviewBlockBody :: HintMap -> Block -> Html ()
renderPreviewBlockBody hints = \case
    BlockAxiom _loc _title _marker axiom ->
        previewStatement (renderAxiom hints axiom)
    BlockClaim _kind _loc _title _marker claim ->
        previewStatement (renderClaim hints claim)
    BlockDefn _loc _title _marker defn ->
        previewStatement (renderDefn hints defn)
    BlockAbbr _loc _title _marker abbr ->
        previewStatement (renderAbbreviation hints abbr)
    BlockData _loc _title _marker datatype ->
        renderDatatype hints datatype
    BlockInductive _loc _title _marker ind ->
        renderInductive hints ind
    BlockSig _loc _title _marker asms sig ->
        renderSignatureBlock hints asms sig
    BlockStruct _loc _title _marker structDefn ->
        renderStructDefn hints structDefn
    BlockProof{} ->
        skip

previewStatement :: Html () -> Html ()
previewStatement =
    p_ [class_ "reference-preview-statement"]

blockLocationOf :: Block -> Location
blockLocationOf = \case
    BlockAxiom loc _title _marker _axiom -> loc
    BlockClaim _kind loc _title _marker _claim -> loc
    BlockProof start _proof _end -> start
    BlockDefn loc _title _marker _defn -> loc
    BlockAbbr loc _title _marker _abbr -> loc
    BlockData loc _title _marker _datatype -> loc
    BlockInductive loc _title _marker _ind -> loc
    BlockSig loc _title _marker _asms _sig -> loc
    BlockStruct loc _title _marker _structDefn -> loc

referenceTargetsOfBlockRenderInfo :: BlockRenderInfo -> [ReferenceTarget]
referenceTargetsOfBlockRenderInfo (_index, block, blockId) =
    maybeToList blockTarget <> datatypeTargets
    where
        sourceFile = locFile (blockLocationOf block)

        blockTarget = do
            marker <- blockMarkerOf block
            pure ReferenceTarget
                { targetMarker = marker
                , targetAnchorId = blockId
                , targetKind = blockPrefixText block
                , targetTitle = formatBlockTitle (blockTitleOf block)
                , targetSourceFile = sourceFile
                , targetBody = \hints -> renderPreviewBlockBody hints block
                }

        datatypeTargets = case block of
            BlockData _loc _title _marker datatype ->
                [ ReferenceTarget
                    { targetMarker = datatypeDerivedFactMarker
                    , targetAnchorId = markerText datatypeDerivedFactMarker
                    , targetKind = "Datatype Fact"
                    , targetTitle = Nothing
                    , targetSourceFile = sourceFile
                    , targetBody = \hints -> previewStatement (inlineMath (renderFormulaMath hints datatypeDerivedFactFormula))
                    }
                | DatatypeDerivedFact{datatypeDerivedFactMarker, datatypeDerivedFactFormula} <- datatypeDerivedFacts datatype
                ]
            _ ->
                []


renderProof :: HintMap -> ReferenceContext -> Proof -> Html ()
renderProof hints references = \case
    Omitted ->
        p_ "Omitted."
    Qed mloc justification ->
        renderProofTerminal mloc justification
    ByCase _loc cases -> do
        p_ "Proof by cases."
        term "proof-" (traverse_ (renderCase hints references) cases)
    ByContradiction _loc proof -> do
        p_ "Proof by contradiction."
        term "proof-" (renderProof hints references proof)
    BySetInduction _loc maybeTerm proof -> do
        p_ do
            toHtml ("Proof by set induction" :: Text)
            case maybeTerm of
                Nothing -> skip
                Just targetTerm -> do
                    toHtml (" on " :: Text)
                    renderTermInline hints targetTerm
            toHtml ("." :: Text)
        term "proof-" (renderProof hints references proof)
    ByOrdInduction _loc proof -> do
        p_ "Proof by ordinal induction."
        term "proof-" (renderProof hints references proof)
    Assume _loc stmt proof -> do
        p_ do
            toHtml ("Assume " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    FixSymbolic _loc vars bound proof -> do
        p_ do
            toHtml ("Fix " :: Text)
            renderVarListInline vars
            renderBoundInline hints vars bound
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    FixSuchThat _loc vars stmt proof -> do
        p_ do
            toHtml ("Fix " :: Text)
            renderVarListInline vars
            toHtml (" such that " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    Calc _loc maybeQuant calc proof -> do
        renderCalc hints references maybeQuant calc
        renderProofContinuation hints references proof
    TakeVar _loc vars bound stmt justification proof -> do
        p_ do
            toHtml ("Take " :: Text)
            renderVarListInline vars
            renderBoundInline hints vars bound
            toHtml (" such that " :: Text)
            renderStmtInline hints stmt
            renderJustificationSuffix references justification
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    TakeNoun _loc np justification proof -> do
        p_ do
            toHtml ("Take " :: Text)
            renderNounPhraseList hints np
            renderJustificationSuffix references justification
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    Have _loc maybeStmt stmt justification proof -> do
        p_ do
            case maybeStmt of
                Nothing
                    | isContradictionStmt stmt ->
                        toHtml ("Contradiction" :: Text)
                    | isImplicitProofEnd proof ->
                        skip
                    | otherwise ->
                        toHtml ("We have " :: Text)
                Just premise -> do
                    toHtml ("Since " :: Text)
                    renderStmtInline hints premise
                    toHtml (", we have " :: Text)
            unless (isContradictionStmt stmt) do
                renderStmtInline hints stmt
            renderJustificationSuffix references justification
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    Suffices _loc stmt justification proof -> do
        p_ do
            toHtml ("It suffices to show that " :: Text)
            renderStmtInline hints stmt
            renderJustificationSuffix references justification
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    Subclaim _loc stmt subproof proof -> do
        p_ do
            toHtml ("Show " :: Text)
            renderStmtInline hints stmt
            toHtml ("." :: Text)
        term "proof-" (renderProof hints references subproof)
        renderProofContinuation hints references proof
    Define _loc var expr proof -> do
        p_ do
            toHtml ("Let " :: Text)
            renderVarEqInline hints var expr
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    DefineFunction _loc fun arg value boundVar boundExpr proof -> do
        p_ do
            toHtml ("Let " :: Text)
            renderFunctionEqInline hints fun arg value
            toHtml (" for " :: Text)
            renderVarInline boundVar
            toHtml (" in " :: Text)
            inlineMath (renderExprMathRow hints boundExpr)
            toHtml ("." :: Text)
        renderProofContinuation hints references proof
    DefineFunctionLocal _loc fun arg _target domVar codVar rules proof -> do
        p_ do
            toHtml ("Let " :: Text)
            renderFunctionCallInline fun arg
            toHtml (" be locally defined from " :: Text)
            renderVarInline domVar
            toHtml (" to " :: Text)
            renderVarInline codVar
            toHtml ("." :: Text)
        ul_ do
            for_ (toList rules) \(ruleTerm, formula) -> li_ do
                inlineMath (renderExprMathRow hints ruleTerm)
                toHtml (" if " :: Text)
                inlineMath (renderFormulaMath hints formula)
                toHtml ("." :: Text)
        renderProofContinuation hints references proof

    where
        renderProofTerminal :: Maybe Location -> Justification -> Html ()
        renderProofTerminal mloc justification = case (mloc, justification) of
            (Nothing, JustificationEmpty) ->
                skip
            (Just _, JustificationEmpty) ->
                p_ "Trivial."
            _ ->
                p_ do
                    toHtml ("Follows" :: Text)
                    renderJustificationSuffix references justification
                    toHtml ("." :: Text)

renderProofContinuation :: HintMap -> ReferenceContext -> Proof -> Html ()
renderProofContinuation hints references proof =
    unless (isImplicitProofEnd proof) (renderProof hints references proof)

isImplicitProofEnd :: Proof -> Bool
isImplicitProofEnd = \case
    Qed Nothing JustificationEmpty -> True
    _ -> False

isContradictionStmt :: Stmt -> Bool
isContradictionStmt = \case
    StmtFormula (PropositionalConstant _loc IsBottom) -> True
    _ -> False

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

renderCase :: HintMap -> ReferenceContext -> Case -> Html ()
renderCase hints references Case{..} =
    term "proof-" do
        p_ do
            toHtml ("Case " :: Text)
            renderStmtInline hints caseOf
            toHtml ("." :: Text)
        renderProof hints references caseProof

renderCalc :: HintMap -> ReferenceContext -> Maybe CalcQuantifier -> Calc -> Html ()
renderCalc hints references maybeQuant calc = do
    p_ do
        toHtml ("Calculation" :: Text)
        case maybeQuant of
            Nothing -> skip
            Just quant -> do
                toHtml (" for " :: Text)
                renderCalcQuantifierInline hints quant
        toHtml ("." :: Text)
    blockMath (renderCalcMath hints calc)
    let justifications = calcJustifications calc
    when (not (null justifications)) do
        ul_ do
            traverse_ renderStepJustification justifications
    where
        renderStepJustification :: (Int, Justification) -> Html ()
        renderStepJustification (_idx, JustificationEmpty) = skip
        renderStepJustification (idx, jst) = li_ do
            toHtml ("Step " <> Text.pack (show idx) <> ": " :: Text)
            renderJustification references jst
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
    Equation expr steps -> do
        renderExprMathRow hints expr
        for_ (toList steps) \(nextExpr, _jst) -> do
            moText "="
            renderExprMathRow hints nextExpr
    Biconditionals phi steps -> do
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
        renderConnectedStmtInline hints conn stmt1 stmt2
    StmtQuantPhrase _loc qp stmt -> do
        renderQuantPhraseInline hints qp
        toHtml (" " :: Text)
        renderStmtInline hints stmt
    SymbolicQuantified _loc quant vars bound suchThat stmt -> do
        toHtml (quantifierWord quant)
        toHtml (" " :: Text)
        renderBoundSubjectInline hints vars bound
        renderQuantifiedTailInline hints quant suchThat stmt

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

renderConnectedStmtInline :: HintMap -> Connective -> Stmt -> Stmt -> Html ()
renderConnectedStmtInline hints conn stmt1 stmt2 = case conn of
    ExclusiveOr -> do
        toHtml ("either " :: Text)
        renderStmtInline hints stmt1
        toHtml (" or " :: Text)
        renderStmtInline hints stmt2
    NegatedDisjunction -> do
        toHtml ("neither " :: Text)
        renderStmtInline hints stmt1
        toHtml (" nor " :: Text)
        renderStmtInline hints stmt2
    _ -> do
        renderStmtInline hints stmt1
        toHtml (" " :: Text)
        toHtml (connectiveWord conn)
        toHtml (" " :: Text)
        renderStmtInline hints stmt2

renderQuantifiedTailInline :: HintMap -> Quantifier -> Maybe Stmt -> Stmt -> Html ()
renderQuantifiedTailInline hints quant suchThat stmt =
    case quant of
        Universally -> do
            for_ suchThat \suchStmt -> do
                toHtml (" such that " :: Text)
                renderStmtInline hints suchStmt
            toHtml (" we have " :: Text)
            renderStmtInline hints stmt
        Existentially ->
            renderExistentialTailInline hints suchThat stmt
        Nonexistentially ->
            renderExistentialTailInline hints suchThat stmt

renderExistentialTailInline :: HintMap -> Maybe Stmt -> Stmt -> Html ()
renderExistentialTailInline hints suchThat stmt = do
    toHtml (" such that " :: Text)
    case suchThat of
        Nothing ->
            renderStmtInline hints stmt
        Just suchStmt -> do
            renderStmtInline hints suchStmt
            toHtml (" and " :: Text)
            renderStmtInline hints stmt


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
        inlineMath (renderExprMathRow hints expr)
    AsmLetThe var fun -> do
        renderVarInline var
        toHtml (" be " :: Text)
        renderFunInline renderTermInline' fun
            where renderTermInline' = renderTermInline hints
    AsmLetEq var expr -> do
        renderVarEqInline hints var expr
    AsmLetStruct var structPhrase -> do
        renderVarInline var
        toHtml (" be a " :: Text)
        renderStructPhraseInline structPhrase


renderTermInline :: HintMap -> Term -> Html ()
renderTermInline hints = \case
    TermExpr expr ->
        inlineMath (renderExprMathRow hints expr)
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

renderStmtMath :: HintMap -> Stmt -> Html ()
renderStmtMath hints =
    renderStmtMathFragments . stmtMathFragments hints

renderStmtMathFragments :: StmtMathFragments -> Html ()
renderStmtMathFragments =
    go ""
    where
        go :: Text -> StmtMathFragments -> Html ()
        go pending = \case
            [] ->
                flush pending
            StmtMathProse text : rest ->
                go (pending <> text) rest
            StmtMathNode node : rest -> do
                flush pending
                node
                go "" rest

        flush :: Text -> Html ()
        flush text =
            let renderedText = preserveBoundarySpaces text
            in unless (Text.null renderedText) (mtextText renderedText)

        preserveBoundarySpaces :: Text -> Text
        preserveBoundarySpaces text =
            Text.replicate leadingCount nbsp
                <> middleText
                <> Text.replicate trailingCount nbsp
            where
                leadingCount = Text.length (Text.takeWhile (== ' ') text)
                textAfterLeading = Text.drop leadingCount text
                trailingCount = Text.length (Text.takeWhileEnd (== ' ') textAfterLeading)
                middleText = Text.dropEnd trailingCount textAfterLeading
                nbsp = Text.singleton '\160'

stmtMathProse :: Text -> StmtMathFragments
stmtMathProse text
    | Text.null text = []
    | otherwise = [StmtMathProse text]

stmtMathNode :: Html () -> StmtMathFragments
stmtMathNode html =
    [StmtMathNode html]

joinStmtMathFragments :: StmtMathFragments -> [StmtMathFragments] -> StmtMathFragments
joinStmtMathFragments _ [] = []
joinStmtMathFragments separator (first : rest) =
    first <> foldMap (separator <>) rest

stmtMathFragments :: HintMap -> Stmt -> StmtMathFragments
stmtMathFragments hints = \case
    StmtFormula phi ->
        stmtMathNode (renderFormulaMath hints phi)
    StmtVerbPhrase ts vp -> do
        termListMathFragments hints ts
            <> stmtMathProse " "
            <> verbPhraseMathFragments hints (length ts > 1) vp
    StmtNoun ts np -> do
        termListMathFragments hints ts
            <> stmtMathProse (if length ts > 1 then " are a " else " is a ")
            <> nounPhraseMaybeMathFragments hints np
    StmtStruct t structPhrase -> do
        termMathFragments hints t
            <> stmtMathProse " is a "
            <> structPhraseMathFragments structPhrase
    StmtNeg _loc stmt -> do
        stmtMathProse "it is not the case that "
            <> stmtMathFragments hints stmt
    StmtExists _loc np -> do
        stmtMathProse "there exists "
            <> nounPhraseListMathFragments hints np
    StmtConnected conn _loc stmt1 stmt2 -> do
        connectedStmtMathFragments hints conn stmt1 stmt2
    StmtQuantPhrase _loc qp stmt -> do
        quantPhraseMathFragments hints qp
            <> stmtMathProse " "
            <> stmtMathFragments hints stmt
    SymbolicQuantified _loc quant vars bound suchThat stmt -> do
        stmtMathProse (quantifierWord quant <> " ")
            <> boundSubjectMathFragments hints vars bound
            <> quantifiedTailMathFragments hints quant suchThat stmt

quantPhraseMathFragments :: HintMap -> QuantPhrase -> StmtMathFragments
quantPhraseMathFragments hints (QuantPhrase quant np) =
    stmtMathProse (quantifierWord quant <> " ")
        <> nounPhraseListMathFragments hints np

connectedStmtMathFragments :: HintMap -> Connective -> Stmt -> Stmt -> StmtMathFragments
connectedStmtMathFragments hints conn stmt1 stmt2 = case conn of
    ExclusiveOr ->
        stmtMathProse "either "
            <> stmtMathFragments hints stmt1
            <> stmtMathProse " or "
            <> stmtMathFragments hints stmt2
    NegatedDisjunction ->
        stmtMathProse "neither "
            <> stmtMathFragments hints stmt1
            <> stmtMathProse " nor "
            <> stmtMathFragments hints stmt2
    _ ->
        stmtMathFragments hints stmt1
            <> stmtMathProse (" " <> connectiveWord conn <> " ")
            <> stmtMathFragments hints stmt2

quantifiedTailMathFragments :: HintMap -> Quantifier -> Maybe Stmt -> Stmt -> StmtMathFragments
quantifiedTailMathFragments hints quant suchThat stmt =
    case quant of
        Universally ->
            foldMap
                (\suchStmt -> stmtMathProse " such that " <> stmtMathFragments hints suchStmt)
                suchThat
                <> stmtMathProse " we have "
                <> stmtMathFragments hints stmt
        Existentially ->
            existentialTailMathFragments hints suchThat stmt
        Nonexistentially ->
            existentialTailMathFragments hints suchThat stmt

existentialTailMathFragments :: HintMap -> Maybe Stmt -> Stmt -> StmtMathFragments
existentialTailMathFragments hints suchThat stmt =
    stmtMathProse " such that "
        <> case suchThat of
            Nothing ->
                stmtMathFragments hints stmt
            Just suchStmt ->
                stmtMathFragments hints suchStmt
                    <> stmtMathProse " and "
                    <> stmtMathFragments hints stmt

termMathFragments :: HintMap -> Term -> StmtMathFragments
termMathFragments hints = \case
    TermExpr expr ->
        stmtMathNode (renderExprMathRow hints expr)
    TermFun fun -> do
        stmtMathProse "the "
            <> funMathFragments (termMathFragments hints) fun
    TermIota _loc var stmt -> do
        stmtMathProse "the "
            <> stmtMathNode (renderVarMath var)
            <> stmtMathProse " such that "
            <> stmtMathFragments hints stmt
    TermQuantified quant _loc np -> do
        stmtMathProse (termQuantifierWord quant <> " ")
            <> nounPhraseMaybeMathFragments hints np

termListMathFragments :: HintMap -> NonEmpty Term -> StmtMathFragments
termListMathFragments hints =
    joinStmtMathFragments (stmtMathProse " and ") . fmap (termMathFragments hints) . toList

nounPhraseMaybeMathFragments :: HintMap -> NounPhrase Maybe -> StmtMathFragments
nounPhraseMaybeMathFragments hints (NounPhrase ls noun maybeName rs maybeSuchThat) =
    adjListMathFragments renderTermMath' ls
        <> nounMathFragments False renderTermMath' noun
        <> foldMap (\name -> stmtMathProse " " <> stmtMathNode (renderVarMath name)) maybeName
        <> adjRListMathFragments hints rs
        <> foldMap (\stmt -> stmtMathProse " such that " <> stmtMathFragments hints stmt) maybeSuchThat
    where
        renderTermMath' = termMathFragments hints

nounPhraseListMathFragments :: HintMap -> NounPhrase [] -> StmtMathFragments
nounPhraseListMathFragments hints (NounPhrase ls noun names rs maybeSuchThat) =
    adjListMathFragments renderTermMath' ls
        <> nounMathFragments (length names > 1) renderTermMath' noun
        <> if null names
            then []
            else stmtMathProse " " <> stmtMathNode (renderVarListMath (NonEmpty.fromList names))
        <> adjRListMathFragments hints rs
        <> foldMap (\stmt -> stmtMathProse " such that " <> stmtMathFragments hints stmt) maybeSuchThat
    where
        renderTermMath' = termMathFragments hints

adjListMathFragments :: (a -> StmtMathFragments) -> [AdjLOf a] -> StmtMathFragments
adjListMathFragments renderArg adjs =
    if null adjs
        then []
        else joinStmtMathFragments (stmtMathProse " ") (renderAdjLMathFragments renderArg <$> adjs)
            <> stmtMathProse " "

adjRListMathFragments :: HintMap -> [AdjROf Term] -> StmtMathFragments
adjRListMathFragments hints adjs =
    if null adjs
        then []
        else stmtMathProse " "
            <> joinStmtMathFragments (stmtMathProse " and ") (renderAdjRMathFragments hints <$> adjs)

renderAdjLMathFragments :: (a -> StmtMathFragments) -> AdjLOf a -> StmtMathFragments
renderAdjLMathFragments renderArg (AdjL _loc item args) =
    lexicalItemStmtMathFragments renderArg item args

renderAdjRMathFragments :: HintMap -> AdjROf Term -> StmtMathFragments
renderAdjRMathFragments hints = \case
    AdjR _loc item args ->
        lexicalItemStmtMathFragments (termMathFragments hints) item args
    AttrRThat verbPhrase ->
        stmtMathProse "that "
            <> verbPhraseMathFragments hints False verbPhrase

adjMathFragments :: (a -> StmtMathFragments) -> AdjOf a -> StmtMathFragments
adjMathFragments renderArg (Adj _loc item args) =
    lexicalItemStmtMathFragments renderArg item args

verbMathFragments :: Bool -> (a -> StmtMathFragments) -> VerbOf a -> StmtMathFragments
verbMathFragments isPlural renderArg (Verb _loc item args) =
    lexicalItemSgPlStmtMathFragments isPlural renderArg item args

verbPhraseMathFragments :: HintMap -> Bool -> VerbPhrase -> StmtMathFragments
verbPhraseMathFragments hints isPlural = \case
    VPVerb verb ->
        verbMathFragments isPlural (termMathFragments hints) verb
    VPAdj adjs ->
        stmtMathProse (if isPlural then "are " else "is ")
            <> joinStmtMathFragments (stmtMathProse " and ") (adjMathFragments (termMathFragments hints) <$> toList adjs)
    VPVerbNot verb ->
        stmtMathProse (if isPlural then "do not " else "does not ")
            <> verbMathFragments True (termMathFragments hints) verb
    VPAdjNot adjs ->
        stmtMathProse (if isPlural then "are not " else "is not ")
            <> joinStmtMathFragments (stmtMathProse " and ") (adjMathFragments (termMathFragments hints) <$> toList adjs)

nounMathFragments :: Bool -> (a -> StmtMathFragments) -> NounOf a -> StmtMathFragments
nounMathFragments isPlural renderArg (Noun _loc item args) =
    lexicalItemSgPlStmtMathFragments isPlural renderArg item args

funMathFragments :: (a -> StmtMathFragments) -> FunOf a -> StmtMathFragments
funMathFragments renderArg Fun{phrase, funArgs} =
    lexicalItemSgPlStmtMathFragments False renderArg phrase funArgs

structPhraseMathFragments :: StructPhrase -> StmtMathFragments
structPhraseMathFragments item =
    lexicalItemSgPlStmtMathFragments False termMathPlaceholderFragments item []

termMathPlaceholderFragments :: a -> StmtMathFragments
termMathPlaceholderFragments _ =
    stmtMathProse "?"

lexicalItemStmtMathFragments :: (a -> StmtMathFragments) -> LexicalItem -> [a] -> StmtMathFragments
lexicalItemStmtMathFragments renderArg item args =
    patternStmtMathFragments renderArg (lexicalItemPhrase item) args

lexicalItemSgPlStmtMathFragments :: Bool -> (a -> StmtMathFragments) -> LexicalItemSgPl -> [a] -> StmtMathFragments
lexicalItemSgPlStmtMathFragments isPlural renderArg item args =
    patternStmtMathFragments renderArg phrase args
    where
        phrase = if isPlural then pl (lexicalItemSgPlPhrase item) else sg (lexicalItemSgPlPhrase item)

patternStmtMathFragments :: (a -> StmtMathFragments) -> [Maybe Token] -> [a] -> StmtMathFragments
patternStmtMathFragments renderArg patternParts args =
    joinStmtMathFragments (stmtMathProse " ") (go patternParts args)
    where
        go [] [] = []
        go [] (_ : _) = error "renderPatternStmtMath: too many arguments"
        go (Nothing : rest) (arg : restArgs) = renderArg arg : go rest restArgs
        go (Nothing : _) [] = error "renderPatternStmtMath: not enough arguments"
        go (Just tok : rest) restArgs = renderStmtToken tok : go rest restArgs

        renderStmtToken :: Token -> StmtMathFragments
        renderStmtToken = \case
            Word w -> stmtMathProse w
            tok -> stmtMathNode (renderMathToken tok)


renderFormulaMath :: HintMap -> Formula -> Html ()
renderFormulaMath hints = \case
    FormulaChain chain ->
        renderChainMathRow hints chain
    FormulaPredicate _loc predi marker exprs ->
        renderHintedMathRow hints PredicateHint marker (toList exprs) (renderPrefixPredicateFallback predi (renderExprMath hints <$> toList exprs))
    Connected _loc conn phi psi -> do
        renderFormulaMath hints phi
        moText (connectiveSymbol conn)
        renderFormulaMath hints psi
    FormulaNeg _loc phi -> do
        moText "¬"
        renderFormulaMath hints phi
    FormulaQuantified _loc quant vars bound phi -> do
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

renderChainMathRow :: HintMap -> Chain -> Html ()
renderChainMathRow hints chain =
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
renderRelationApplication hints sign lhs rel rhs = case (sign, rel) of
    (Negative, Relation _loc symbol [])
        | Just negated <- negatedRelationSymbol (relationSymbolToken symbol) -> do
            renderExprListMath hints lhs
            moText negated
            renderExprListMath hints rhs
    _ ->
        applySign sign (renderRelationCore hints lhs rel rhs)

applySign :: Sign -> Html () -> Html ()
applySign sign html = case sign of
    Positive -> html
    Negative -> do
        mo_ "¬"
        html

renderRelationCore :: HintMap -> [Expr] -> Relation -> [Expr] -> Html ()
renderRelationCore hints lhs rel rhs = case rel of
    Relation _loc symbol relParams -> do
        renderExprListMath hints lhs
        renderRelationSymbolCore hints symbol relParams
        renderExprListMath hints rhs
    RelationExpr _loc expr -> do
        renderExprListMath hints lhs
        renderExprMath hints expr
        renderExprListMath hints rhs

renderRelationSymbolCore :: HintMap -> RelationSymbol -> [Expr] -> Html ()
renderRelationSymbolCore hints symbol relParams =
    renderHintedMathRow
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

negatedRelationSymbol :: Token -> Maybe Text
negatedRelationSymbol = \case
    Command "in" -> Just "∉"
    Command "ni" -> Just "∌"
    Command "subset" -> Just "⊄"
    Command "subseteq" -> Just "⊈"
    Command "supset" -> Just "⊅"
    Command "supseteq" -> Just "⊉"
    Command "meets" -> Just "⋈̸"
    Symbol "=" -> Just "≠"
    Symbol "<" -> Just "≮"
    Symbol ">" -> Just "≯"
    Symbol "≤" -> Just "≰"
    Symbol "≥" -> Just "≱"
    _ -> Nothing


renderExprMath :: HintMap -> Expr -> Html ()
renderExprMath hints expr = case expr of
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
    ExprFiniteSet{} ->
        mrow_ (renderExprMathRow hints expr)
    ExprSep{} ->
        mrow_ (renderExprMathRow hints expr)
    ExprReplace{} ->
        mrow_ (renderExprMathRow hints expr)
    ExprReplacePred{} ->
        mrow_ (renderExprMathRow hints expr)

renderExprMathRow :: HintMap -> Expr -> Html ()
renderExprMathRow hints = \case
    ExprVar var ->
        renderVarMath var
    ExprInteger _loc n ->
        mnText (Text.pack (show n))
    ExprOp _loc item args ->
        renderHintedMathRow hints OperatorHint (mixfixMarker item) args (renderPatternFallback (mixfixPattern item) (renderExprMath hints <$> args))
    ExprStructOp _loc symb maybeExpr ->
        let marker = structMarker symb
            args = maybeToList maybeExpr
        in renderHintedMathRow hints StructOpHint marker args (renderStructFallback symb (renderExprMath hints <$> args))
    ExprFiniteSet _loc exprs ->
        renderFiniteSetMath hints (toList exprs)
    ExprSep _loc var bound stmt -> do
        moText "{"
        renderVarMath var
        moText "∈"
        renderExprMathRow hints bound
        moText "|"
        renderStmtMath hints stmt
        moText "}"
    ExprReplace _loc expr bounds maybeStmt -> do
        moText "{"
        renderExprMathRow hints expr
        moText "|"
        renderReplaceBoundsMath hints (toList bounds)
        for_ maybeStmt \stmt -> do
            moText "|"
            renderStmtMath hints stmt
        moText "}"
    ExprReplacePred _loc rangeVar domVar domExpr stmt -> do
        moText "{"
        renderVarMath rangeVar
        moText "|"
        moText "∃"
        renderVarMath domVar
        moText "∈"
        renderExprMathRow hints domExpr
        moText "."
        renderStmtMath hints stmt
        moText "}"

renderReplaceBoundsMath :: HintMap -> [(VarSymbol, Expr)] -> Html ()
renderReplaceBoundsMath hints =
    joinHtml (moText ",") . fmap renderBound
    where
        renderBound (var, expr) = do
            renderVarMath var
            moText "∈"
            renderExprMathRow hints expr

renderExprListMath :: HintMap -> [Expr] -> Html ()
renderExprListMath hints =
    joinHtml (moText ",") . fmap (renderExprMath hints)

renderFiniteSetMath :: HintMap -> [Expr] -> Html ()
renderFiniteSetMath hints exprs =
    do
        moText "{"
        renderExprListMath hints exprs
        moText "}"

renderHintedMath :: HintMap -> HintCategory -> Marker -> [Expr] -> Html () -> Html ()
renderHintedMath hints category marker args fallback =
    case Map.lookup (category, marker, length args) hints of
        Nothing ->
            fallback
        Just RenderHint{..}
            | renderHintArity /= length args ->
                error ("Render hint arity mismatch for " <> show category <> " " <> show marker <> ": expected " <> show renderHintArity <> ", got " <> show (length args))
            | otherwise ->
                renderTemplateAsNode renderHintTemplate
    where
        renderedArgs = renderExprMath hints <$> args

        renderTemplateAsNode :: [TemplatePiece] -> Html ()
        renderTemplateAsNode = \case
            [piece] ->
                renderPiece piece
            pieces ->
                mrow_ (traverse_ renderPiece pieces)

        renderPiece :: TemplatePiece -> Html ()
        renderPiece = \case
            Literal text -> toHtmlRaw text
            Slot ix -> case nth (ix - 1) renderedArgs of
                Just html -> html
                Nothing -> error ("Render hint slot out of bounds for " <> show marker <> ": <x" <> show ix <> "/>")

renderHintedMathRow :: HintMap -> HintCategory -> Marker -> [Expr] -> Html () -> Html ()
renderHintedMathRow hints category marker args fallback =
    case Map.lookup (category, marker, length args) hints of
        Nothing ->
            fallback
        Just RenderHint{..}
            | renderHintArity /= length args ->
                error ("Render hint arity mismatch for " <> show category <> " " <> show marker <> ": expected " <> show renderHintArity <> ", got " <> show (length args))
            | otherwise ->
                traverse_ renderPiece renderHintTemplate
    where
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
    traverse_ id (go patternParts renderedArgs)
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
    Variable v -> renderNamedVariableMath v
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
inlineMath inner = math_ inner

blockMath :: Html () -> Html ()
blockMath inner = math_ [displayblock_] inner

renderVarInline :: VarSymbol -> Html ()
renderVarInline = inlineMath . renderVarMath

renderVarMath :: VarSymbol -> Html ()
renderVarMath = \case
    NamedVarAt _loc name ->
        renderNamedVariableMath name
    FreshVarAt _loc n ->
        miText ("_" <> Text.pack (show n))

renderNamedVariableMath :: Text -> Html ()
renderNamedVariableMath rawName =
    case displayVariable rawName of
        VariableDisplay baseText Nothing ->
            miText baseText
        VariableDisplay baseText (Just (VariableTicks tickCount)) ->
            msup_ do
                miText baseText
                renderPrimeSuperscript tickCount
        VariableDisplay baseText (Just (VariableSubscript subscriptText)) ->
            msub_ do
                miText baseText
                renderVariableSubscriptMath subscriptText

renderPrimeSuperscript :: Int -> Html ()
renderPrimeSuperscript tickCount
    | tickCount <= 1 =
        moText "′"
    | otherwise =
        mrow_ (foldMap (const (moText "′")) [1 .. tickCount])

renderVariableSubscriptMath :: Text -> Html ()
renderVariableSubscriptMath subscriptText
    | Text.all isDigit subscriptText =
        mnText subscriptText
    | otherwise =
        miText subscriptText

renderVarEqInline :: HintMap -> VarSymbol -> Expr -> Html ()
renderVarEqInline hints var expr =
    inlineMath do
        renderVarMath var
        moText "="
        renderExprMathRow hints expr

renderFunctionCallInline :: VarSymbol -> VarSymbol -> Html ()
renderFunctionCallInline fun arg =
    inlineMath (renderFunctionCallMath fun arg)

renderFunctionEqInline :: HintMap -> VarSymbol -> VarSymbol -> Expr -> Html ()
renderFunctionEqInline hints fun arg expr =
    inlineMath do
        renderFunctionCallMath fun arg
        moText "="
        renderExprMathRow hints expr

renderFunctionCallMath :: VarSymbol -> VarSymbol -> Html ()
renderFunctionCallMath fun arg = do
    renderVarMath fun
    moText "("
    renderVarMath arg
    moText ")"

renderVarListInline :: NonEmpty VarSymbol -> Html ()
renderVarListInline vars =
    inlineMath (renderVarListMath vars)

renderVarListMath :: NonEmpty VarSymbol -> Html ()
renderVarListMath vars =
    joinHtml (moText ",") (renderVarMath <$> toList vars)

renderBoundInline :: HintMap -> NonEmpty VarSymbol -> Bound -> Html ()
renderBoundInline hints vars = \case
    Unbounded -> skip
    bound -> do
        toHtml (" with " :: Text)
        inlineMath (renderBoundPhraseMath hints vars bound)

renderBoundSubjectInline :: HintMap -> NonEmpty VarSymbol -> Bound -> Html ()
renderBoundSubjectInline hints vars = \case
    Unbounded ->
        renderVarListInline vars
    bound ->
        inlineMath (renderBoundSubjectMath hints vars bound)

renderBoundPhraseMath :: HintMap -> NonEmpty VarSymbol -> Bound -> Html ()
renderBoundPhraseMath hints vars = \case
    Unbounded -> mrow_ skip
    Bounded _loc sign rel expr ->
        renderRelationApplication hints sign (ExprVar <$> toList vars) rel [expr]

renderBoundSubjectMath :: HintMap -> NonEmpty VarSymbol -> Bound -> Html ()
renderBoundSubjectMath hints vars = \case
    Unbounded ->
        renderVarListMath vars
    Bounded _loc sign rel expr ->
        renderRelationApplication hints sign (ExprVar <$> toList vars) rel [expr]

boundSubjectMathFragments :: HintMap -> NonEmpty VarSymbol -> Bound -> StmtMathFragments
boundSubjectMathFragments hints vars = \case
    Unbounded ->
        stmtMathNode (renderVarListMath vars)
    bound ->
        stmtMathNode (renderBoundSubjectMath hints vars bound)

renderSymbolPatternInline :: HintMap -> SymbolPattern -> Html ()
renderSymbolPatternInline hints =
    inlineMath . renderSymbolPatternMath hints

renderSymbolPatternMath :: HintMap -> SymbolPattern -> Html ()
renderSymbolPatternMath hints (SymbolPattern symbol vars) =
    renderHintedMathRow hints OperatorHint (mixfixMarker symbol) (ExprVar <$> vars) (renderPatternFallback (mixfixPattern symbol) (renderVarMath <$> vars))

renderJustification :: ReferenceContext -> Justification -> Html ()
renderJustification references = \case
    JustificationRef markers -> do
        toHtml ("by " :: Text)
        renderMarkerReferences references (toList markers)
    JustificationSetExt ->
        toHtml ("by set extensionality" :: Text)
    JustificationEmpty ->
        skip
    JustificationLocal ->
        toHtml ("by local assumptions" :: Text)

renderJustificationSuffix :: ReferenceContext -> Justification -> Html ()
renderJustificationSuffix _ JustificationEmpty = skip
renderJustificationSuffix references justification = do
    toHtml (" " :: Text)
    renderJustification references justification

renderMarkerReferences :: ReferenceContext -> [Marker] -> Html ()
renderMarkerReferences references markers
    | length markers >= referenceGroupThreshold =
        renderMarkerReferenceGroup references markers
    | otherwise =
        joinHtml (toHtml (", " :: Text)) (renderMarkerReference references <$> markers)

renderMarkerReferenceGroup :: ReferenceContext -> [Marker] -> Html ()
renderMarkerReferenceGroup references markers =
    span_ groupAttributes do
        toHtml ("[...]" :: Text)
        span_ [class_ "reference-preview-group-items", makeAttributes "hidden" "hidden"] do
            traverse_ (renderMarkerReferenceGroupItem references) markers
    where
        referenceCountLabel = Text.pack (show (length markers)) <> " references"

        groupAttributes =
            [ class_ "ref-badge has-preview ref-badge-group"
            , makeAttributes "data-preview-group" "true"
            , makeAttributes "data-reference-label" referenceCountLabel
            , makeAttributes "aria-describedby" "reference-preview-popup"
            , makeAttributes "tabindex" "0"
            , makeAttributes "role" "button"
            , makeAttributes "aria-label" ("Show " <> referenceCountLabel)
            ]

renderMarkerReferenceGroupItem :: ReferenceContext -> Marker -> Html ()
renderMarkerReferenceGroupItem ReferenceContext{..} marker =
    span_ itemAttributes skip
    where
        label = markerText marker
        baseAttributes =
            [ class_ "reference-preview-group-item"
            , makeAttributes "data-reference-label" label
            ]

        itemAttributes =
            baseAttributes <> case Map.lookup marker referenceAnchors of
                Just anchor ->
                    [ makeAttributes "data-preview-link" ("#" <> anchor)
                    , makeAttributes "data-preview-target-id" anchor
                    ]
                Nothing ->
                    case Map.lookup marker referencePreviews of
                        Nothing ->
                            []
                        Just preview ->
                            [ makeAttributes "data-preview-link" (externalReferenceHref referenceInputPath preview)
                            , makeAttributes "data-preview-id" (previewId preview)
                            ]

renderMarkerReference :: ReferenceContext -> Marker -> Html ()
renderMarkerReference ReferenceContext{..} marker =
    case Map.lookup marker referenceAnchors of
        Just anchor ->
            a_ (href_ ("#" <> anchor) : referenceAttributes (Just (currentPreviewAttributes anchor))) (toHtml label)
        Nothing ->
            case Map.lookup marker referencePreviews of
                Nothing ->
                    span_ (referenceAttributes Nothing) (toHtml label)
                Just preview ->
                    a_
                        ( href_ (externalReferenceHref referenceInputPath preview)
                        : referenceAttributes (Just (importedPreviewAttributes preview))
                        )
                        (toHtml label)
    where
        label = markerText marker

        referenceAttributes preview =
            [ class_ (if hasPreview preview then "ref-badge has-preview" else "ref-badge")
            , makeAttributes "data-reference-label" label
            ]
            <> foldMap id preview

        hasPreview =
            \case
                Nothing -> False
                Just _ -> True

        currentPreviewAttributes anchor =
            [ makeAttributes "data-preview-target-id" anchor
            , makeAttributes "aria-describedby" "reference-preview-popup"
            ]

        importedPreviewAttributes entry =
            [ makeAttributes "data-preview-id" (previewId entry)
            , makeAttributes "aria-describedby" "reference-preview-popup"
            ]


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
