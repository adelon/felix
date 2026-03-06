{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Golden where


import Api qualified
import Provers qualified
import Tptp.UnsortedFirstOrder (toText)
import Base
import Provers (defaultTimeLimit)

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Text.Lazy.IO qualified as LazyTextIO
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile, findByExtension)
import Text.Pretty.Simple (pShowNoColor)
import UnliftIO
import UnliftIO.Environment
import Api (Options(withDumpPremselTraining))

testOptions :: Api.Options
testOptions = Api.Options
    { Api.withDumpPremselTraining = Api.WithoutDumpPremselTraining
    , Api.withCache = Api.WithoutCache
    , Api.withFilter = Api.WithoutFilter
    , inputPath = error "testOptions: no inputPath"
    , withDump = Api.WithoutDump
    , withLogging = Api.WithoutLogging
    , withMemoryLimit = Provers.defaultMemoryLimit
    , withOmissions = Api.WithOmissions
    , withParseOnly = Api.WithoutParseOnly
    , withProver = Api.WithDefaultProver
    , withTimeLimit = Provers.defaultTimeLimit
    , withVersion = Api.WithoutVersion
    , withMegalodon = Api.WithoutMegalodon
    , withHtml = Api.WithoutHtml
    }

goldenTests :: IO TestTree
goldenTests = runReaderT goldenTestGroup testOptions

goldenTestGroup :: (MonadUnliftIO io, MonadReader Api.Options io) => io TestTree
goldenTestGroup = testGroup "golden tests" <$> sequence
    [ tokenizing
    , scanning
    , parsing
    , glossing
    , generatingTasks
    , encodingTasks
    , verification
    ]


-- | A testing triple consists of a an 'input' file, which is proccesed, resulting
-- in 'output' file, which is then compared to a 'golden' file.
data Triple = Triple
    { input  :: FilePath
    , output :: FilePath
    , golden :: FilePath
    }
    deriving (Show, Eq)


-- | Gathers all the files for the test. We test all examples and everything in @test/pass/@.
-- The golden files for all tests are stored in @test/pass/@, so we need to adjust the filepath
-- of the files from @examples/@.
gatherTriples :: MonadIO io => String -> io [Triple]
gatherTriples stage = do
    inputs <- liftIO (findByExtension [".tex"] "test/examples")
    pure $
        [ Triple{..}
        | input <- inputs
        , let input' = "test" </> "golden" </> takeBaseName input </> stage
        , let golden = input' <.> "golden"
        , let output = input' <.> "out"
        ]

createTripleDirectoriesIfMissing :: MonadIO io => Triple -> io ()
createTripleDirectoriesIfMissing Triple{..} = liftIO $
    createDirectoryIfMissing True (takeDirectory output)

makeGoldenTest :: (MonadUnliftIO io, MonadReader Api.Options io) => String -> (Triple -> io ()) -> io TestTree
makeGoldenTest stage action = do
    triples <- gatherTriples stage
    for triples createTripleDirectoriesIfMissing
    runInIO <- askRunInIO
    pure $ testGroup stage
        [ goldenVsFile
            (takeBaseName input) -- test name
            golden
            output
            (runInIO (action triple))
        | triple@Triple{..} <- triples
        ]

tokenizing :: (MonadUnliftIO io, MonadReader Api.Options io) => io TestTree
tokenizing = makeGoldenTest "tokenizing" $ \Triple{..} -> do
    tokenStream <- Api.tokenize input
    liftIO (LazyTextIO.writeFile output (pShowNoColor (Api.simpleStream tokenStream)))


scanning :: (MonadUnliftIO io, MonadReader Api.Options io) => io TestTree
scanning = makeGoldenTest "scanning" $ \Triple{..} -> do
    lexicalItems <- Api.scan input
    liftIO (LazyTextIO.writeFile output (pShowNoColor lexicalItems))

parsing :: (MonadUnliftIO io, MonadReader Api.Options io) => io TestTree
parsing = makeGoldenTest "parsing" $ \Triple{..} -> do
    parseResult <- Api.parse input
    liftIO (LazyTextIO.writeFile output (pShowNoColor parseResult))


glossing :: (MonadUnliftIO io, MonadReader Api.Options io) => io TestTree
glossing = makeGoldenTest "glossing" $ \Triple{..} -> do
    interpretationResult <- Api.gloss input
    liftIO (LazyTextIO.writeFile output (pShowNoColor interpretationResult))


generatingTasks :: (MonadUnliftIO io, MonadReader Api.Options io) => io TestTree
generatingTasks = makeGoldenTest "generating tasks" $ \Triple{..} -> do
    tasks <- Api.generateTasks input
    liftIO $ LazyTextIO.writeFile output (pShowNoColor tasks)


encodingTasks :: (MonadUnliftIO io, MonadReader Api.Options io) => io TestTree
encodingTasks = makeGoldenTest "encoding tasks" $ \Triple{..} -> do
    tasks <- Api.encodeTasks input
    liftIO (TextIO.writeFile output (Text.intercalate "\n------------------\n" (toText <$> tasks)))


verification :: (MonadUnliftIO io, MonadReader Api.Options io) => io TestTree
verification = makeGoldenTest "verification" $ \Triple{..} -> do
    vampirePathPath <- (?? "vampire") <$> lookupEnv "NAPROCHE_ZF_VAMPIRE"
    let defaultProverInstance = Provers.vampire vampirePathPath Provers.Silent Provers.defaultTimeLimit Provers.defaultMemoryLimit
    answers <- runNoLoggingT (Api.verify defaultProverInstance input)
    liftIO (LazyTextIO.writeFile output (pShowNoColor answers))
