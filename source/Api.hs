{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Api
    ( constructTheoryGraph, TheoryGraph
    , tokenize, TokStream
    , scan
    , parse
    , simpleStream
    , builtins
    , ParseException(..)
    , gloss, GlossError(..)
    , generateTasks
    , encodeTasks
    , prepareDumpTasks
    , dumpTask
    , verify, verifyStreaming, ProverAnswer(..), VerificationResult(..)
    , exportMegalodon
    , exportHtml
    , WithCache(..)
    , WithFilter(..)
    , WithOmissions(..)
    , WithProver(..)
    , WithVersion(..)
    , WithLogging(..)
    , WithDump(..)
    , WithMegalodon(..)
    , WithHtml(..)
    , pattern WithoutDump
    , WithParseOnly(..)
    , Options(..)
    , WithDumpPremselTraining(..)
    ) where


import Base
import Checking
import Checking.Cache
import Encoding
import Filter(filterTask)
import Meaning (GlossError(..), glossStep, initialGlossState)
import Megalodon qualified
import Provers
import Render.Html qualified as Html
import Report.Location
import Syntax.Abstract qualified as Raw
import Syntax.Adapt (adaptChunks, scanChunk, ScannedLexicalItem)
import Syntax.Concrete
import Syntax.Internal qualified as Internal
import Syntax.Lexicon (Lexicon, builtins)
import Syntax.Token
import TheoryGraph (TheoryGraph, Precedes(..))
import TheoryGraph qualified
import Tptp.UnsortedFirstOrder qualified as Tptp

import Control.Monad.Logger
import Data.List (intercalate)
import Control.Monad.Reader
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Data.Text.IO qualified as Text
import qualified Data.Text as Text
import GHC.Conc (getNumProcessors, retry)
import System.FilePath.Posix
import Text.Earley (parser, fullParses, Report(..))
import Text.Megaparsec hiding (parse, Token)
import UnliftIO
import UnliftIO.Directory
import UnliftIO.Environment

-- | Follow all @\\import@ statements recursively and build a theory graph from them.
-- Each @\\import@ statement should be on its own separate line and the imports should precede
-- all top-level environments. This process is entirely decoupled from tokenizing and
-- parsing processes.
constructTheoryGraph :: forall io. MonadIO io => FilePath -> io TheoryGraph
constructTheoryGraph root = fmap snd (go mempty (TheoryGraph.singleton root) root)
    where
    go :: Set FilePath -> TheoryGraph -> FilePath -> io (Set FilePath, TheoryGraph)
    go visited graph file =
        if file `Set.member` visited then pure (visited, graph)
        else do
            raw <- findAndReadFile file
            let files = gatherImports raw
            let visited' = Set.insert file visited
            let precedes = [ancestor `Precedes` file | ancestor <- files]
            let graph' = TheoryGraph.fromList precedes <> graph
            results <- go visited' graph' `traverse` files
            let (visited'', graph'') = unzip results
            pure (visited' <> Set.unions visited'', graph' <> TheoryGraph.unions graph'')


-- | Given a filename for a theory, look for it in a set of predetermined places:
-- the current directory, the library, and the debug directory.
findAndReadFile :: MonadIO io => FilePath -> io Text
findAndReadFile path = do
    currentDir <- getCurrentDirectory
    libraryDir <- (?? (currentDir </> "library")) <$> lookupEnv "NAPROCHE_LIB"
    let debugDir = currentDir </> "debug"

    let currentFile = currentDir </> path
    let libraryFile = libraryDir </> path
    let debugFile = debugDir </> path

    existsInCurrent <- doesFileExist currentFile
    existsInLibrary <- doesFileExist libraryFile
    existsInDebug <- doesFileExist debugFile
    liftIO if
        | existsInCurrent -> Text.readFile currentFile
        | existsInLibrary -> Text.readFile libraryFile
        | existsInDebug -> Text.readFile debugFile
        | otherwise -> error $ unlines
            [ "Could not find file from relative path: " <> path
            , "Searched in:"
            , "  " <> currentDir </> path
            , "  " <> libraryDir </> path <> " (which you can override by setting the NAPROCHE_LIB environment variable)"
            , "  " <> debugDir </> path
            ]

lexFile :: MonadIO io => FilePath -> io (Text, [[Located Token]])
lexFile file = do
    raw <- findAndReadFile file
    fileId <- registerFilePath file
    case runLexer fileId file raw of
        Left tokenError ->
            throwIO (TokenError (errorBundlePretty tokenError))
        Right (_imports, chunks) ->
            pure (raw, chunks)

tokenizeChunks :: MonadIO io => FilePath -> io [[Located Token]]
tokenizeChunks file = snd <$> lexFile file

-- | Throws a 'ParseException' when tokenizing fails.
tokenize :: MonadIO io => FilePath -> io TokStream
tokenize file = do
    (raw, chunks) <- lexFile file
    pure (TokStream raw chunks)

-- | Scan the given file for lexical items. The actual parsing process
-- uses 'adaptChunks' instead.
scan :: MonadIO io => FilePath -> io [ScannedLexicalItem]
scan input = do
    tokenStream <- tokenize input
    pure (concatMap scanChunk (unTokStream tokenStream))


-- | Parse a file. Throws a 'ParseException' when tokenizing, scanning, or
-- parsing fails.
parse :: MonadIO io => FilePath -> io [Raw.Block]
parse file = do
    blocksRef <- liftIO (newIORef [])
    parseWith file (\block -> modifyIORef' blocksRef (block :))
    reverse <$> liftIO (readIORef blocksRef)

parseWith :: MonadIO io => FilePath -> (Raw.Block -> IO ()) -> io ()
parseWith file emitBlock =
    withTheoryLexicon file \lexicon chunksByFile -> do
        let parseChunk :: [Located Token] -> ([Raw.Block], Report Text [Located Token])
            parseChunk = fullParses (parser (grammar lexicon))

            parseChunks = \case
                [] -> skip
                toks : restChunks -> do
                    blocks <- parseChunkResult (parseChunk toks)
                    liftIO (traverse_ emitBlock blocks)
                    parseChunks restChunks

        traverse_ (parseChunks . snd) chunksByFile

withTheoryLexicon
    :: MonadIO io
    => FilePath
    -> (Lexicon -> [(FilePath, [[Located Token]])] -> io a)
    -> io a
withTheoryLexicon file action = do
    -- We need to consider the entire theory graph here already
    -- since we can use vocabulary of imported theories.
    theoryGraph <- constructTheoryGraph file
    case TheoryGraph.topSortSeq theoryGraph of
        -- LATER replace with a more helpful error message, like actually showing the cycle properly
        Left cyc -> error ("could not linearize theory graph (likely due to circular dependencies):\n" <> show cyc)
        Right theoryChain -> do
            -- Tokenize once and reuse the cached chunks for both lexicon adaptation
            -- and parsing.
            chunksByFile <- traverse tokenizeChunks theoryChain
            let chunksByFileList = toList chunksByFile
                theoryFiles = toList theoryChain
                chunksWithFiles = zip theoryFiles chunksByFileList

            -- Build the final lexicon strictly so parsing does not force a retained
            -- adaptation thunk over the entire chunk cache.
            let lexicon = foldl' (flip adaptChunks) builtins chunksByFileList
            lexicon `seq` action lexicon chunksWithFiles

parseChunkResult :: MonadIO io => ([Raw.Block], Report Text [Located Token]) -> io [Raw.Block]
parseChunkResult result = case result of
    (_, Report _ es (tok:toks)) -> throwIO (UnconsumedTokens es (tok :| toks))
    ([], _) -> throwIO EmptyParse
    (ambi@(_:_:_), _) -> case nubOrd ambi of
        [block] ->
            pure [trace ("technically ambiguous parse:\n" <> show block) block]
        ambi' -> throwIO (AmbigousParse ambi')
    ([block], _) ->
        pure [block]


simpleStream :: TokStream -> [[Token]]
simpleStream TokStream{unTokStream=chunks} = [unLocated <$> ch | ch <- chunks]


data ParseException
    = UnconsumedTokens [Text] (NonEmpty (Located Token)) -- ^ Expectations and unconsumed tokens.
    | AmbigousParse [Raw.Block]
    | EmptyParse
    | TokenError String

instance Show ParseException where
    show = \case
        UnconsumedTokens es (ltok :| ltoks) ->
            let tok = unLocated ltok
                toks = unLocated <$> ltoks
            in
                "unconsumed " <> describeToken tok <> " at " <> prettyLocation (startPos ltok) <> "\n" <>
                "  " <> unwords (tokToString <$> (tok : take 4 toks)) <> "\n" <>
                "  " <> replicate (length (tokToString tok)) '^' <> "\n" <>
                case es of
                    [] -> "while expecting nothing"
                    _ -> "while expecting one of the following:\n" <> intercalate ", " (Text.unpack <$> nubOrd es)
        AmbigousParse blocks ->
            "ambiguous parse: " <> show blocks
        EmptyParse ->
            "empty parse"
        TokenError err ->
            err -- Re-use pretty printing from Megaparsec.

instance Exception ParseException where


describeToken :: Token -> String
describeToken = \case
    Word _ -> "word"
    Variable _ -> "variable"
    Symbol _ -> "symbol"
    Integer _ -> "integer"
    Command _ -> "command"
    BeginEnv _ -> "begin of environment"
    EndEnv _ -> "end of environment"
    _ -> "delimiter"

-- | gloss generates internal represantation of the LaTeX files.
-- First the file will be parsed and therefore checkt for grammer.
-- 'meaning' then transfer the raw parsed grammer to the internal semantics.
gloss :: MonadIO io => FilePath -> io [Internal.Block]
gloss file = do
    blocksRef <- liftIO (newIORef [])
    glossWith file (\block -> modifyIORef' blocksRef (block :))
    reverse <$> liftIO (readIORef blocksRef)

glossWith :: MonadIO io => FilePath -> (Internal.Block -> IO ()) -> io ()
glossWith file emitBlock = do
    glossStateRef <- liftIO (newIORef initialGlossState)
    parseWith file \rawBlock -> do
        glossState <- readIORef glossStateRef
        case glossStep glossState rawBlock of
            Left err ->
                throwIO err
            Right (glossedBlock, nextGlossState) -> do
                writeIORef glossStateRef nextGlossState
                emitBlock glossedBlock


generateTasks :: (MonadIO io, MonadReader Options io) => FilePath -> io [Internal.Task]
generateTasks file = do
    dumpPremselTraining <- asks withDumpPremselTraining
    blocks <- gloss file
    tasks <- liftIO (check dumpPremselTraining blocks)
    pure (contractionTask <$> tasks)

prepareCache :: MonadIO io => FilePath -> io FilePath
prepareCache file = do
    xdgCache <- getXdgDirectory XdgCache "zf"
    let cacheDir = xdgCache </> takeDirectory file
    let cache    = xdgCache </> file
    createDirectoryIfMissing True cacheDir
    -- Initialize with an empty cache when no cache exists.
    -- If we do not do this opening the cache file will fail.
    unlessM (doesFileExist cache) (putTaskCache cache [])
    pure cache

prepareDumpTasks :: (MonadIO io, MonadReader Options io) => FilePath -> io [(Int, Tptp.Task)]
prepareDumpTasks file = do
    tasks <- generateTasks file
    filterOption <- asks withFilter
    let filteredTasks = case filterOption of
            WithFilter -> filterTask <$> tasks
            WithoutFilter -> tasks
    let indexedTasks = zip [1..] filteredTasks
    cacheOption <- asks withCache
    indexedTasks' <- case cacheOption of
        WithoutCache -> pure indexedTasks
        WithCache -> do
            cache <- prepareCache file
            filterM (\(_, task) -> notInCache cache task) indexedTasks
    pure [(idx, encodeTask task) | (idx, task) <- indexedTasks']


encodeTasks :: (MonadIO io, MonadReader Options io) => FilePath -> io [Tptp.Task]
encodeTasks file = do
    tasks <- generateTasks file
    pure (encodeTask <$> tasks)

data VerificationResult
    = VerificationSuccess
    | VerificationFailure [(Location, Internal.Formula, ProverAnswer)]
    deriving (Show)

isFailure :: (loc, formula, ProverAnswer) -> Bool
isFailure (_loc, _phi, Yes) = False
isFailure (_loc, _phi, _ans) = True

data VerificationWorkItem
    = VerifyTask Int Internal.Task

data VerificationProducerStopped = VerificationProducerStopped
    deriving (Show)

instance Exception VerificationProducerStopped

verifyStreaming :: (MonadUnliftIO io, MonadLogger io, MonadReader Options io) => ProverInstance -> FilePath -> io VerificationResult
verifyStreaming prover file = do
    dumpPremselTraining <- asks withDumpPremselTraining
    filterOption <- asks withFilter
    cacheOption <- asks withCache
    threadCount <- liftIO getNumProcessors
    let workerCount = max 1 (threadCount `div` 2)
    workQueue <- liftIO (newTBQueueIO (fromIntegral workerCount))
    producerDoneVar <- liftIO (newTVarIO False)
    stopVar <- liftIO (newTVarIO False)
    nextTaskIndexVar <- liftIO (newTVarIO 0)
    firstFailureVar <- liftIO (newTVarIO Nothing)
    runInIO <- askRunInIO

    cacheState <- case cacheOption of
        WithoutCache -> pure Nothing
        WithCache -> do
            cacheFile <- prepareCache file
            hashedCache <- getTaskCache cacheFile
            hashedCacheVar <- liftIO (newTVarIO hashedCache)
            successfulTaskHashesByIndexVar <- liftIO (newTVarIO IntMap.empty)
            pure (Just (cacheFile, hashedCache, hashedCacheVar, successfulTaskHashesByIndexVar))

    let applyFilter :: Internal.Task -> Internal.Task
        applyFilter task = case filterOption of
            WithFilter -> filterTask task
            WithoutFilter -> task

        rememberTask :: Int -> Internal.Task -> IO ()
        rememberTask taskIndex task = case cacheState of
            Nothing -> pure ()
            Just (_cacheFile, _initialHashedCache, hashedCacheVar, successfulTaskHashesByIndexVar) ->
                atomically do
                    modifyTVar' hashedCacheVar (IntSet.insert (hash task))
                    modifyTVar' successfulTaskHashesByIndexVar (IntMap.insert taskIndex (hash task))

        rememberFailure :: Int -> (Location, Internal.Formula, ProverAnswer) -> IO ()
        rememberFailure idx failedAnswer = atomically do
            writeTVar stopVar True
            currentFailure <- readTVar firstFailureVar
            case currentFailure of
                Nothing ->
                    writeTVar firstFailureVar (Just (idx, failedAnswer))
                Just (oldIdx, _oldFailedAnswer) ->
                    when (idx < oldIdx) (writeTVar firstFailureVar (Just (idx, failedAnswer)))

        flushCache :: Maybe Int -> IO ()
        flushCache mFirstFailureIndex = case cacheState of
            Nothing -> pure ()
            Just (cacheFile, initialHashedCache, _hashedCacheVar, successfulTaskHashesByIndexVar) -> do
                finalHashedCache <- atomically do
                    successfulTaskHashesByIndex <- readTVar successfulTaskHashesByIndexVar
                    let successfulTaskHashes = IntSet.fromList $
                            case mFirstFailureIndex of
                                Nothing ->
                                    IntMap.elems successfulTaskHashesByIndex
                                Just firstFailureIndex ->
                                    [ successfulTaskHash
                                    | (taskIndex, successfulTaskHash) <- IntMap.toList successfulTaskHashesByIndex
                                    , taskIndex < firstFailureIndex
                                    ]
                    pure (initialHashedCache <> successfulTaskHashes)
                putTaskCacheHashes cacheFile finalHashedCache

        emitTask :: Internal.Task -> IO ()
        emitTask rawTask = do
            -- ASSUMPTION: There is exactly one emitter (the producer pipeline), so
            -- queue insertion order is well-defined by this callback execution order.
            --
            -- If a failure has already been observed, stop emitting immediately.
            stopRequested <- atomically (readTVar stopVar)
            when stopRequested (throwIO VerificationProducerStopped)

            let task = applyFilter (contractionTask rawTask)
            shouldRun <- case cacheState of
                Nothing -> pure True
                Just (_cacheFile, _initialHashedCache, hashedCacheVar, _successfulTaskHashesByIndexVar) ->
                    atomically (IntSet.notMember (hash task) <$> readTVar hashedCacheVar)
            if shouldRun
                then do
                    -- The stop check and enqueue must be in one STM transaction.
                    -- This guarantees that no new task is enqueued after stopVar has become True.
                    enqueueSucceeded <- atomically do
                        stopRequested' <- readTVar stopVar
                        if stopRequested'
                            then pure False
                            else do
                                nextTaskIndex <- readTVar nextTaskIndexVar
                                writeTVar nextTaskIndexVar (nextTaskIndex + 1)
                                writeTBQueue workQueue (VerifyTask nextTaskIndex task)
                                pure True
                    unless enqueueSucceeded (throwIO VerificationProducerStopped)
                else
                    skip

        worker :: IO ()
        worker = do
            mItem <- atomically do
                mQueuedItem <- tryReadTBQueue workQueue
                case mQueuedItem of
                    Just queuedItem ->
                        pure (Just queuedItem)
                    Nothing -> do
                        producerDone <- readTVar producerDoneVar
                        if producerDone
                            then pure Nothing
                            else retry
            case mItem of
                Nothing ->
                    pure ()
                Just (VerifyTask taskIndex task) -> do
                    answer <- runInIO (runProver prover task)
                    if isFailure answer
                        then
                            rememberFailure taskIndex answer
                        else
                            rememberTask taskIndex task
                    worker

        runCheckedBlocks :: IORef CheckingState -> [Internal.Block] -> IO ()
        runCheckedBlocks checkingStateRef blocks = do
            checkingState <- readIORef checkingStateRef
            nextCheckingState <- runCheckingBlocks blocks checkingState
            writeIORef checkingStateRef nextCheckingState

        emitCheckedBlock :: IORef CheckingState -> IORef (Maybe Internal.Block) -> Internal.Block -> IO ()
        emitCheckedBlock checkingStateRef pendingLemmaRef block = do
            pendingLemma <- readIORef pendingLemmaRef
            case pendingLemma of
                Nothing -> case block of
                    Internal.BlockLemma{} ->
                        writeIORef pendingLemmaRef (Just block)
                    _ ->
                        runCheckedBlocks checkingStateRef [block]
                Just lemma -> case block of
                    -- | We have seen a lemma, but not yet emitted it, so we delay emitting it until we see the next block.
                    --   If the next block is a proof, then we emit them together as a checked lemma+proof pair.
                    --   If the next block is not a proof, then we emit the lemma by itself and emit the next block separately.
                    Internal.BlockProof{} -> do
                        writeIORef pendingLemmaRef Nothing
                        runCheckedBlocks checkingStateRef [lemma, block]
                    _ -> do
                        writeIORef pendingLemmaRef Nothing
                        runCheckedBlocks checkingStateRef [lemma]
                        emitCheckedBlock checkingStateRef pendingLemmaRef block

        flushPendingLemma :: IORef CheckingState -> IORef (Maybe Internal.Block) -> IO ()
        flushPendingLemma checkingStateRef pendingLemmaRef = do
            pendingLemma <- readIORef pendingLemmaRef
            for_ pendingLemma \lemma -> do
                writeIORef pendingLemmaRef Nothing
                runCheckedBlocks checkingStateRef [lemma]

        producer :: IO ()
        producer = do
            checkingStateRef <- newIORef (initialCheckingState dumpPremselTraining emitTask)
            pendingLemmaRef <- newIORef Nothing
            glossWith file (emitCheckedBlock checkingStateRef pendingLemmaRef)
            flushPendingLemma checkingStateRef pendingLemmaRef

        withAsyncs :: [IO a] -> ([Async a] -> IO b) -> IO b
        withAsyncs [] inner = inner []
        withAsyncs (action : actions) inner =
            withAsync action \a ->
                withAsyncs actions (\as -> inner (a : as))

    (result, firstFailureIndex) <- liftIO $
        withAsyncs (replicate workerCount worker) \workerAsyncs -> do
            traverse_ link workerAsyncs
            producerResult <- tryAny (producer `catch` \VerificationProducerStopped -> pure ())
            case producerResult of
                Left ex -> do
                    traverse_ cancel workerAsyncs
                    throwIO ex
                Right () -> do
                    -- Shutdown protocol:
                    -- 1) Producer has stopped.
                    -- 2) Mark producer as done.
                    -- 3) Workers drain all queued tasks and then terminate once the queue is empty.
                    atomically (writeTVar producerDoneVar True)
                    traverse_ wait workerAsyncs
                    firstFailure <- atomically (readTVar firstFailureVar)
                    pure case firstFailure of
                        Just (firstFailureIndex', failedAnswer) -> (VerificationFailure [failedAnswer], Just firstFailureIndex')
                        Nothing -> (VerificationSuccess, Nothing)

    liftIO (flushCache firstFailureIndex)
    pure result

verify :: (MonadUnliftIO io, MonadLogger io, MonadReader Options io) => ProverInstance -> FilePath -> io VerificationResult
verify = verifyStreaming

dumpTask :: MonadIO io => FilePath -> Tptp.Task -> io ()
dumpTask file tptp = liftIO (Text.writeFile file (Tptp.toText tptp))

exportMegalodon :: (MonadUnliftIO io) => FilePath -> io Text
exportMegalodon file = do
    blocks <- gloss file
    pure (Megalodon.encodeBlocks blocks)

exportHtml :: MonadUnliftIO io => FilePath -> io Text
exportHtml file = do
    hints <- findAndReadFile "lexicon.tsv"
    (rootBlocks, theoryBlocks) <- parseHtmlBlocks file
    pure (Html.renderDocument file hints rootBlocks theoryBlocks)

parseHtmlBlocks :: MonadIO io => FilePath -> io ([Raw.Block], [Raw.Block])
parseHtmlBlocks file =
    withTheoryLexicon file \lexicon chunksByFile -> do
        let parseChunk :: [Located Token] -> ([Raw.Block], Report Text [Located Token])
            parseChunk = fullParses (parser (grammar lexicon))

        blocksByFile <- for chunksByFile \(path, chunks) -> do
            blocks <- fmap concat $ for chunks \toks ->
                parseChunkResult (parseChunk toks)
            pure (path, blocks)

        rootBlocks <- case [blocks | (path, blocks) <- blocksByFile, path == file] of
            [blocks] -> pure blocks
            _ -> error ("parseHtmlBlocks: could not find token chunks for " <> file)

        pure (rootBlocks, concatMap snd blocksByFile)

-- | Should we use caching?
data WithCache = WithoutCache | WithCache deriving (Show, Eq)

data WithFilter = WithoutFilter | WithFilter deriving (Show, Eq)

-- | Are proof omissions allowed?
data WithOmissions = WithoutOmissions | WithOmissions deriving (Show, Eq)

-- | Which external prover should be used?
data WithProver = WithDefaultProver | WithEprover | WithVampire | WithIprover deriving (Show, Eq)

-- | Should we show the version of the software?
data WithVersion = WithoutVersion | WithVersion deriving (Show, Eq)

data WithLogging = WithoutLogging | WithLogging deriving (Show, Eq)

-- | Should we dump all proof tasks? Where?
newtype WithDump = WithDump FilePath deriving (Show, Eq)

-- | Should we export to Megalodon?
data WithMegalodon = WithMegalodon | WithoutMegalodon deriving (Show, Eq)

-- | Should we export to HTML?
data WithHtml = WithHtml | WithoutHtml deriving (Show, Eq)

pattern WithoutDump :: WithDump
pattern WithoutDump = WithDump ""

data WithParseOnly = WithoutParseOnly | WithParseOnly deriving (Show, Eq)

data Options = Options
    { inputPath :: FilePath
    , withCache :: WithCache
    , withDump :: WithDump
    , withFilter :: WithFilter
    , withLogging :: WithLogging
    , withMemoryLimit :: Provers.MemoryLimit
    , withOmissions :: WithOmissions
    , withParseOnly :: WithParseOnly
    , withProver :: WithProver
    , withTimeLimit :: Provers.TimeLimit
    , withVersion :: WithVersion
    , withMegalodon :: WithMegalodon
    , withHtml :: WithHtml
    , withDumpPremselTraining :: WithDumpPremselTraining
    }
