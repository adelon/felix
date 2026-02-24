{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module CommandLine where

import Api
import Base
import Provers qualified
import Version qualified
import Report.Location

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text.IO qualified as Text
import Options.Applicative
import UnliftIO
import UnliftIO.Directory
import UnliftIO.Environment (lookupEnv)
import System.FilePath.Posix

runCommandLine :: IO ()
runCommandLine = do
    options@Options{withLogging = logging} <- execParser (withInfo parseOptions)
    case logging of
        WithoutLogging -> runNoLoggingT (runReaderT run options)
        WithLogging -> runStdoutLoggingT (runReaderT run options)
    where
        withInfo :: Parser a -> ParserInfo a
        withInfo p = info (helper <*> p) (fullDesc <> header "Naproche/ZF")


run :: (MonadUnliftIO io, MonadLogger io, MonadReader Options io) => io ()
run = do
    opts <- ask
    case withVersion opts of
        WithVersion -> liftIO (Text.putStrLn Version.info)
        WithoutVersion -> skip
    case withOmissions opts of
        WithoutOmissions -> liftIO (Text.putStrLn "--safe is not implemented yet.")
        WithOmissions -> skip
    case withDump opts of
        WithoutDump -> skip
        WithDump dir -> do
            liftIO (Text.putStrLn "\ESC[1;36mCreating Dumpfiles.\ESC[0m")
            tasks <- prepareDumpTasks (inputPath opts)
            createDirectoryIfMissing True dir
            forM_ [(dir </> show n <.> "p", task) | (n, task) <- tasks] (uncurry dumpTask)
            liftIO (Text.putStrLn "\ESC[35mDump ready.\ESC[0m")
    case (withParseOnly opts, withMegalodon opts) of
        (WithParseOnly, _) -> do
            _ast <- parse (inputPath opts)
            skip
        (_, WithMegalodon) -> do
            megalodon <- exportMegalodon (inputPath opts)
            let outputFile = "megalodon" </> replaceExtension (inputPath opts) "mg"
            createDirectoryIfMissing True (takeDirectory outputFile)
            liftIO (Text.writeFile outputFile megalodon)
        (WithoutParseOnly, WithoutMegalodon) -> do
            liftIO (Text.putStrLn "\ESC[1;96mStart of verification.\ESC[0m")
            prover <- case withProver opts of
                    WithEprover -> do
                        eproverPath <- getExecutable "eprover" "E" "NAPROCHE_ZF_EPROVER"
                        pure (Provers.eprover eproverPath)
                    WithIprover -> pure Provers.iprover
                    _ -> do -- TODO: when upgrading to GHC 9.12, use OR pattern for WithVampire | WithDefaultProver here and move this entire block to top
                        vampirePathPath <- getExecutable "vampire" "Vampire" "NAPROCHE_ZF_VAMPIRE"
                        pure (Provers.vampire vampirePathPath)
            let proverInstance = prover Provers.Silent (withTimeLimit opts) (withMemoryLimit opts)
            result <- verify proverInstance (inputPath opts)
            liftIO case result of
                VerificationSuccess -> putStrLn "Verification successful."
                VerificationFailure [] -> error "Empty verification fail"
                VerificationFailure ((loc, _phi, proverAnswer) : _) -> case proverAnswer of
                    Yes ->
                        skip
                    No -> do
                        Text.putStrLn $ "Verification failed: prover found countermodel at " <> locationToText loc
                    ContradictoryAxioms -> do
                        Text.putStrLn $ "Verification failed: contradictory axioms at " <> locationToText loc
                    Uncertain -> do
                        Text.putStrLn $ "Verification failed: out of resources at " <> locationToText loc
                    Error label err -> do
                        Text.putStrLn $ "Error at " <> locationToText loc <> ": "
                        Text.putStrLn $ "Task: " <> label
                        Text.putStrLn $ "Error: " <> err


getExecutable :: MonadIO m => String -> String -> String -> m FilePath
getExecutable exeName exeDisplayName envOverride = do
    envPath <- lookupEnv envOverride
    case envPath of
        Just path -> do
            exists <- doesFileExist path
            if exists
                then pure path
                else error $ "The environment variable " <> envOverride <> " for " <> exeDisplayName <> " is set to \"" <> path <> "\", but this executable does not exist. Make sure the environment variable is correct or remove it to look for " <> exeDisplayName <> " on your $PATH instead."
        Nothing -> do
            mPath <- findExecutable exeName
            case mPath of
                Just path -> pure path
                Nothing -> error $ exeDisplayName <> " (" <> exeName <> ") not found on $PATH. Make sure it is installed and available on your $PATH or set the " <> envOverride <> " environment variable to its path."

parseOptions :: Parser Options
parseOptions = do
    inputPath <- strArgument (help "Source file" <> metavar "FILE")
    withCache <- withCacheParser
    withDump <- withDumpParser
    withFilter <- withFilterParser
    withLogging <- withLoggingParser
    withMemoryLimit <- withMemoryLimitParser
    withOmissions <- withOmissionsParser
    withParseOnly <- withParseOnlyParser
    withProver <- withProverParser
    withTimeLimit <- withTimeLimitParser
    withVersion <- withVersionParser
    withMegalodon <- withMegalodonParser
    withDumpPremselTraining <- withDumpPremselTrainingParser
    pure Options{..}

withTimeLimitParser :: Parser Provers.TimeLimit
withTimeLimitParser = Provers.Seconds <$> option auto (long "timelimit" <> short 't' <> value dflt <> help "Time limit for each proof task in seconds.")
    where
        Provers.Seconds dflt = Provers.defaultTimeLimit

withMemoryLimitParser :: Parser Provers.MemoryLimit
withMemoryLimitParser = Provers.Megabytes <$> option auto (long "memlimit" <> short 'm' <> value dflt <> help "Memory limit for each proof task in MB.")
    where
        Provers.Megabytes dflt = Provers.defaultMemoryLimit

withProverParser :: Parser WithProver
withProverParser = flag' WithEprover  (long "eprover" <> help "Use E as external prover.")
    <|> flag' WithVampire  (long "vampire" <> help "Use Vampire as external prover.")
    <|> flag' WithIprover (long "iprover" <> help "Use iProver as external prover.")
    <|> pure WithDefaultProver

withFilterParser :: Parser WithFilter
withFilterParser = flag' WithoutFilter (long "nofilter" <> help "Do not perform relevance filtering.")
    <|> flag' WithFilter (long "filter" <> help "Perform relevance filtering.")
    <|> pure WithoutFilter

withOmissionsParser :: Parser WithOmissions
withOmissionsParser = flag' WithOmissions (long "unsafe" <> help "Allow proof omissions (on by default).")
    <|> flag' WithoutOmissions (long "safe" <> help "Disallow proof omissions.")
    <|> pure WithOmissions

withParseOnlyParser :: Parser WithParseOnly
withParseOnlyParser = flag' WithParseOnly (long "parseonly" <> help "Only parse and show the resulting AST (off by default).")
    <|> pure WithoutParseOnly

withVersionParser :: Parser WithVersion
withVersionParser = flag' WithVersion (long "version" <> help "Show the current version (off by default).")
    <|> pure WithoutVersion

withLoggingParser :: Parser WithLogging
withLoggingParser = flag' WithLogging (long "log" <> help "Enable logging (off by default).")
    <|> pure WithoutLogging

withCacheParser :: Parser WithCache
withCacheParser = flag' WithoutCache (long "uncached" <> help "Do not use caching.")
    <|> flag' WithCache (long "cached" <> help "Use caching (default).")
    <|> pure WithCache

withDumpParser :: Parser WithDump
withDumpParser = WithDump <$> strOption (long "dump" <> metavar "DUMPDIR" <> help "Dump all proof tasks in a separate directory. Run with --unchached to dump all tasks.")
    <|> pure WithoutDump

withDumpPremselTrainingParser :: Parser WithDumpPremselTraining
withDumpPremselTrainingParser = flag' WithDumpPremselTraining (long "premseldump" <> help "Dump training data for premise selection.")
    <|> pure WithoutDumpPremselTraining

withMegalodonParser :: Parser WithMegalodon
withMegalodonParser = flag' WithMegalodon (long "megalodon" <> help "Export to Megalodon (experimental).")
    <|> pure WithoutMegalodon
