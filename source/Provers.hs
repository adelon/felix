{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Provers where

import Base
import Encoding
import Syntax.Internal (Formula, Task(..), isIndirect)
import Report.Location

import Control.Exception (evaluate)
import Control.Monad.Logger
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time
import System.Exit (ExitCode)
import System.IO (Handle, hClose, hIsEOF, hSetEncoding, utf8)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, proc, waitForProcess)
import TextBuilder
import Text.Megaparsec
import Text.Megaparsec.Char qualified as Char
import UnliftIO.Async (concurrently)

type Prover = Verbosity -> TimeLimit -> MemoryLimit -> ProverInstance

-- | Prover responses are stored as a list of prefixes.
data ProverInstance = Prover
    { proverName :: String
    , proverPath :: FilePath
    , proverArgs :: [String]
    , proverSaysYes :: [Text]
    , proverSaysNo :: [Text]
    , proverDoesNotKnow :: [Text]
    , proverWarnsContradiction :: [Text]
    } deriving (Show, Eq)


data Verbosity = Silent | Verbose deriving (Show, Eq)
newtype TimeLimit = Seconds Word64 deriving (Show, Eq, Num)
newtype MemoryLimit = Megabytes Word64 deriving (Show, Eq)

toSeconds :: TimeLimit -> String
toSeconds (Seconds secs) = show secs

toMegabytes :: MemoryLimit -> String
toMegabytes (Megabytes mbs) = show mbs

defaultTimeLimit :: TimeLimit
defaultTimeLimit = Seconds 10

defaultMemoryLimit :: MemoryLimit
defaultMemoryLimit = Megabytes 5000


eproverDev :: ProverInstance
eproverDev = eprover "eprover" Silent defaultTimeLimit defaultMemoryLimit

eprover :: FilePath -> Prover
eprover path verbosity timeLimit memoryLimit = Prover
    { proverName = "eprover"
    , proverPath = path
    , proverArgs =
        [ "--tptp3-format"
        , "--auto"
        , case verbosity of
            Silent  -> "--silent"
            Verbose -> "--verbose"
        , "--soft-cpu-limit=" <> toSeconds timeLimit
        , "--cpu-limit=" <> toSeconds (timeLimit + 5)
        , "--memory-limit=" <> toMegabytes memoryLimit
        ]
    , proverSaysYes = ["# SZS status Theorem"]
    , proverSaysNo = ["# SZS status CounterSatisfiable"]
    , proverDoesNotKnow = ["# SZS status ResourceOut", "# SZS status GaveUp"]
    , proverWarnsContradiction = ["# SZS status ContradictoryAxioms"]
    }


vampire :: FilePath -> Prover
vampire path _verbosity timeLimit memoryLimit = Prover
    { proverName = "vampire"
    , proverPath = path
    , proverArgs =
        [ "--mode", "casc"
        , "--time_limit", toSeconds timeLimit
        , "--memory_limit", toMegabytes memoryLimit
        , "--cores", "2"
        ]
    , proverSaysYes = ["% SZS status Theorem"]
    , proverSaysNo = ["% SZS status CounterSatisfiable"]
    , proverDoesNotKnow = ["% SZS status Timeout"]
    , proverWarnsContradiction = ["% SZS status ContradictoryAxioms"]
    }

-- WIP: setting up a clausifier
iprover :: Prover
iprover _verbosity timeLimit _memoryLimit = Prover
    { proverName = "iProver"
    , proverPath = "iproveropt"
    , proverArgs =
        [ "--time_out_real " <> toSeconds timeLimit
        ]
    , proverSaysYes = ["% SZS status Theorem"]
    , proverSaysNo = ["% SZS status CounterSatisfiable"]
    , proverDoesNotKnow = ["% SZS status Unknown"]
    , proverWarnsContradiction = []
    }

-- | 'Error' contains the error message of the prover verbatim.
data ProverAnswer
    = Yes
    | No
    | ContradictoryAxioms
    | Uncertain
    | Error Text Text
    deriving (Show, Eq)

nominalDiffTimeToText :: NominalDiffTime -> Text
nominalDiffTimeToText delta = TextBuilder.toText (nominalDiffTimeToTextBuilder delta)

nominalDiffTimeToTextBuilder :: NominalDiffTime -> TextBuilder
nominalDiffTimeToTextBuilder delta = case hours of
        0 -> padded minutes <> ":" <> padded restSeconds <> "." <> padded restCentis
        _ -> padded hours   <> ":" <> padded restMinutes <> ":" <> padded restSeconds
    where
        padded n = if n < 10 then char '0' <> decimal n else decimal n
        centiseconds = truncate (100 * nominalDiffTimeToSeconds delta) :: Int
        (seconds, restCentis) = divMod centiseconds 100
        (minutes, restSeconds) = divMod seconds 60
        (hours, restMinutes)   = divMod minutes 60

timeDifferenceToText :: UTCTime -> UTCTime -> Text
timeDifferenceToText startTime endTime = nominalDiffTimeToText (diffUTCTime endTime startTime)


runProver :: (MonadIO io, MonadLogger io) => ProverInstance -> Task -> io (Location, Formula, ProverAnswer)
runProver prover@Prover{..} task = do
    startTime <- liftIO getCurrentTime
    proverAnswer <- liftIO if
        | proverName == "vampire" -> do
            (_exitCode, answer) <- runVampireProcess prover task
            pure answer
        | otherwise -> do
            (_exitCode, answer, answerErr) <- runProverProcess proverPath proverArgs task
            pure (recognizeAnswer prover task answer answerErr)
    endTime <- liftIO getCurrentTime
    let duration = timeDifferenceToText startTime endTime

    logInfoN
        let conjLine = encodeConjectureLine (taskConjectureLabel task) (taskLocation task) (taskDirectness task) (taskConjecture task)
        in  duration <> " " <> toText conjLine

    pure (taskLocation task, taskConjecture task, proverAnswer)

runProverProcess :: FilePath -> [String] -> Task -> IO (ExitCode, Text, Text)
runProverProcess path args task = do
    (Just hin, Just hout, Just herr, ph) <-
        createProcess (proc path args){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}

    hSetEncoding hin utf8
    hSetEncoding hout utf8
    hSetEncoding herr utf8
    writeTask hin task
    hClose hin
    let consumeStrict h = do
            txt <- TextIO.hGetContents h
            _ <- evaluate (Text.length txt)
            pure txt
    (out, err) <- concurrently (consumeStrict hout) (consumeStrict herr)
    exitCode <- waitForProcess ph
    pure (exitCode, out, err)


runVampireProcess :: ProverInstance -> Task -> IO (ExitCode, ProverAnswer)
runVampireProcess Prover{proverPath, proverArgs} task = do
    (Just hin, Just hout, Just herr, ph) <-
        createProcess (proc proverPath proverArgs){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}

    hSetEncoding hin utf8
    hSetEncoding hout utf8
    hSetEncoding herr utf8
    writeTask hin task
    hClose hin

    statusRef <- newIORef Nothing
    let observeLine line = atomicModifyIORef' statusRef \currentStatus ->
            case currentStatus of
                Just _ -> (currentStatus, ())
                Nothing -> (parseMaybe vampireStatusParser line, ())

    (out, err) <- concurrently (consumeLines hout observeLine) (consumeLines herr observeLine)
    exitCode <- waitForProcess ph
    firstStatus <- readIORef statusRef
    pure (exitCode, vampireStatusAnswer task out err firstStatus)


consumeLines :: Handle -> (Text -> IO ()) -> IO Text
consumeLines h onLine = go mempty
    where
        go builder = do
            endOfHandle <- hIsEOF h
            if endOfHandle then pure (toText builder)
            else do
                line <- TextIO.hGetLine h
                onLine line
                let builder' = builder <> text line <> char '\n'
                builder' `seq` go builder'


-- | Parse the answer of a prover based on the configured prefixes of responses.
recognizeAnswer :: ProverInstance -> Task -> Text -> Text -> ProverAnswer
recognizeAnswer prover@Prover{..} task answer answerErr =
    if
        | proverName == "vampire" -> recognizeVampireAnswer prover task answer answerErr
        | otherwise ->
            let
                matches prefixes   = any (\l -> any (`Text.isPrefixOf` l) prefixes) (Text.lines answer)
                saidYes            = matches proverSaysYes
                saidNo             = matches proverSaysNo
                doesNotKnow        = matches proverDoesNotKnow
                warned             = matches proverWarnsContradiction
            in if
                | saidYes || (warned && isIndirect task) -> Yes
                | saidNo -> No
                | doesNotKnow -> Uncertain
                | warned -> ContradictoryAxioms
                | otherwise -> Error (Text.pack(show (taskConjectureLabel task))) (answer <> answerErr)




recognizeVampireAnswer :: ProverInstance -> Task -> Text -> Text -> ProverAnswer
recognizeVampireAnswer _prover task answer answerErr =
    let
        firstStatus = firstJust (parseMaybe vampireStatusParser) (Text.lines answer <> Text.lines answerErr)
    in vampireStatusAnswer task answer answerErr firstStatus


vampireStatusAnswer :: Task -> Text -> Text -> Maybe Text -> ProverAnswer
vampireStatusAnswer task answer answerErr mStatus = case mStatus of
    Nothing ->
        Error (Text.pack(show (taskConjectureLabel task))) (answer <> answerErr)
    Just status -> case status of
        "Theorem" -> Yes
        "CounterSatisfiable" -> No
        "Timeout" -> Uncertain
        "ContradictoryAxioms" ->
            if isIndirect task then Yes else ContradictoryAxioms
        _ ->
            Error (Text.pack(show (taskConjectureLabel task))) (answer <> answerErr)


-- | Parse a Vampire SZS status line.
--
-- Recognizes both standard lines like:
--   % SZS status Timeout for 123
-- and lines prefixed by worker ids (seen with portfolio output), e.g.:
--   % (2581105)SZS status Timeout for
vampireStatusParser :: Parsec Void Text Text
vampireStatusParser = do
    _ <- Char.char '%'
    Char.hspace
    optional do
        _ <- Char.char '('
        _ <- some Char.digitChar
        _ <- Char.char ')'
        Char.hspace
    _ <- chunk "SZS"
    Char.hspace1
    _ <- chunk "status"
    Char.hspace1
    status <- takeWhile1P (Just "SZS status") (\c -> c /= ' ' && c /= '\t')
    _ <- takeRest
    pure status
