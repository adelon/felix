{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- This module defines the lexer and its associated data types.
-- The lexer takes `Text` as input and produces a stream of tokens
-- annotated with positional information. This information is bundled
-- together with the original raw input for producing error messages.
--
-- The lexer perfoms some normalizations to make describing the grammar easier.
-- Words outside of math environments are case-folded. Some commands are analysed
-- as variable tokens and are equivalent to their respective unicode variants
-- (α, β, γ, ..., 𝔸, 𝔹, ℂ, ...). Similarly, @\\begin{...}@ and @\\end{...}@ commands
-- are each parsed as single tokens.
--
module Syntax.Token
    ( Token(..)
    , VariableDisplay(..)
    , VariableSuffix(..)
    , displayVariable
    , renderVariableText
    , tokToString
    , tokToText
    , TokStream(..)
    , Located(..)
    , runLexer
    , gatherImports
    ) where


import Base hiding (many)

import Report.Location

import Control.Monad.Combinators
import Control.Monad.State.Strict
import Data.Char (isAlphaNum)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Prettyprinter (Pretty(..))
import Text.Megaparsec hiding (Token, Label, label)
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Tptp.UnsortedFirstOrder (isAsciiLetter, isAsciiAlphaNumOrUnderscore)


runLexer :: FileId -> String -> Text -> Either (ParseErrorBundle Text Void) ([FilePath], [[Located Token]])
runLexer fileId file raw = runParser (evalStateT document (initLexerState fileId)) file raw


type Lexer = StateT LexerState (Parsec Void Text)


data LexerState = LexerState
    { frames :: !(NonEmpty Frame)
    , currentFileId :: !FileId
    } deriving (Show, Eq)

data Frame
    = TopText
    | MathFrame !Text
    | TextFrame !Int
    deriving (Show, Eq)

initLexerState :: FileId -> LexerState
initLexerState fileId = LexerState (TopText :| []) fileId

topFrameOf :: LexerState -> Frame
topFrameOf LexerState{frames = frame :| _} = frame

topFrame :: Lexer Frame
topFrame = gets topFrameOf

pushFrame :: Frame -> LexerState -> LexerState
pushFrame frame st@LexerState{frames = top :| rest} =
    st{frames = frame :| (top : rest)}

popFrame :: LexerState -> LexerState
popFrame st@LexerState{frames = _ :| []} = st
popFrame st@LexerState{frames = _ :| (top : rest)} =
    st{frames = top :| rest}

modifyTopFrame :: (Frame -> Frame) -> LexerState -> LexerState
modifyTopFrame f st@LexerState{frames = top :| rest} =
    st{frames = f top :| rest}

-- Token recognizers only emit tokens; this is the single place that changes
-- lexical context.
advance :: Token -> LexerState -> LexerState
advance tok st =
    case (topFrameOf st, tok) of
        (TopText, BeginEnv "math") ->
            pushFrame (MathFrame "math") st
        (TextFrame{}, BeginEnv "math") ->
            pushFrame (MathFrame "math") st
        (TopText, BeginEnv "align*") ->
            pushFrame (MathFrame "align*") st
        (TextFrame{}, BeginEnv "align*") ->
            pushFrame (MathFrame "align*") st
        (MathFrame env, EndEnv env')
            | env == env' ->
                popFrame st
        (MathFrame{}, BeginEnv "text") ->
            pushFrame (TextFrame 1) st
        (TextFrame n, InvisibleBraceL) ->
            modifyTopFrame (const (TextFrame (n + 1))) st
        (TextFrame n, InvisibleBraceR)
            | n > 1 ->
                modifyTopFrame (const (TextFrame (n - 1))) st
        (TextFrame 1, EndEnv "text") ->
            popFrame st
        _ ->
            st

-- |
-- A token stream as input stream for a parser. Contains the raw input
-- before tokenization as 'Text' for showing error messages.
--
data TokStream = TokStream
    { rawInput :: !Text
    , unTokStream :: ![[Located Token]]
    } deriving (Show, Eq)

instance Semigroup TokStream where
    TokStream raw1 toks1 <> TokStream raw2 toks2 = TokStream (raw1 <> raw2) (toks1 <> toks2)

instance Monoid TokStream where
    mempty = TokStream mempty mempty

-- | A LaTeX token.
-- Invisible delimiters 'InvisibleBraceL' and 'InvisibleBraceR' are
-- unescaped braces used for grouping in TEX (@{@),
-- visibles braces are escaped braces (@\\{@).
data Token
    = Word !Text
    | Variable !Text
    | Symbol !Text
    | Integer !Int
    | Command !Text
    | Label Text -- ^ A /@\\label{...}@/ command (case-sensitive).
    | Ref (NonEmpty Text) -- ^ A /@\\ref{...}@/ command (case-sensitive).
    | BeginEnv !Text
    | EndEnv !Text
    | ParenL | ParenR
    | BracketL | BracketR
    | VisibleBraceL | VisibleBraceR
    | InvisibleBraceL | InvisibleBraceR
    deriving (Show, Eq, Ord, Generic, Hashable)

instance IsString Token where
    fromString w = Word (Text.pack w)

data VariableDisplay = VariableDisplay
    { variableBaseText :: !Text
    , variableSuffix :: !(Maybe VariableSuffix)
    } deriving (Show, Eq, Ord)

data VariableSuffix
    = VariableSubscript !Text
    | VariableTicks !Int
    deriving (Show, Eq, Ord)

displayVariable :: Text -> VariableDisplay
displayVariable rawName =
    case splitVariableBase rawName of
        Nothing ->
            VariableDisplay rawName Nothing
        Just (baseText, suffixText) ->
            VariableDisplay baseText (displayVariableSuffix suffixText)

renderVariableText :: Text -> Text
renderVariableText rawName =
    case displayVariable rawName of
        VariableDisplay baseText Nothing ->
            baseText
        VariableDisplay baseText (Just (VariableTicks n)) ->
            baseText <> Text.replicate n "'"
        VariableDisplay baseText (Just (VariableSubscript subscriptText)) ->
            baseText <> renderSubscriptText subscriptText

splitVariableBase :: Text -> Maybe (Text, Text)
splitVariableBase rawName =
    matchBlackboardBase rawName <|> matchGreekBase rawName <|> matchSingleLetterBase rawName

matchBlackboardBase :: Text -> Maybe (Text, Text)
matchBlackboardBase rawName = do
    suffixText <- Text.stripPrefix "bb" rawName
    case Text.uncons suffixText of
        Just (upper, rest)
            | 'A' <= upper && upper <= 'Z' ->
                Just ("bb" <> Text.singleton upper, rest)
        _ ->
            Nothing

matchGreekBase :: Text -> Maybe (Text, Text)
matchGreekBase rawName =
    asum
        [ (\suffixText -> (rendered, suffixText)) <$> Text.stripPrefix prefix rawName
        | (prefix, rendered) <- greekVariables
        ]

matchSingleLetterBase :: Text -> Maybe (Text, Text)
matchSingleLetterBase rawName = do
    (baseChar, suffixText) <- Text.uncons rawName
    pure (Text.singleton baseChar, suffixText)

displayVariableSuffix :: Text -> Maybe VariableSuffix
displayVariableSuffix suffixText
    | Text.null suffixText =
        Nothing
    | Text.all (== '_') suffixText =
        Just (VariableTicks (Text.length suffixText))
    | otherwise =
        Just (VariableSubscript (Text.replace "_" "'" suffixText))

renderSubscriptText :: Text -> Text
renderSubscriptText subscriptText
    | Text.length subscriptText == 1 =
        "_" <> subscriptText
    | otherwise =
        "_{" <> subscriptText <> "}"

greekVariables :: [(Text, Text)]
greekVariables =
    [ ("alpha", "α"), ("beta", "β"), ("gamma", "γ"), ("delta", "δ")
    , ("epsilon", "ε"), ("zeta", "ζ"), ("eta", "η"), ("theta", "θ")
    , ("iota", "ι"), ("kappa", "κ"), ("lambda", "λ"), ("mu", "μ")
    , ("nu", "ν"), ("xi", "ξ"), ("pi", "π"), ("rho", "ρ"), ("sigma", "σ")
    , ("tau", "τ"), ("upsilon", "υ"), ("phi", "φ"), ("chi", "χ")
    , ("psi", "ψ"), ("omega", "ω")
    , ("Gamma", "Γ"), ("Delta", "Δ"), ("Theta", "Θ"), ("Lambda", "Λ")
    , ("Xi", "Ξ"), ("Pi", "Π"), ("Sigma", "Σ"), ("Upsilon", "Υ")
    , ("Phi", "Φ"), ("Psi", "Ψ"), ("Omega", "Ω")
    ]

tokToText :: Token -> Text
tokToText = \case
    Word w -> w
    Variable v -> renderVariableText v
    Symbol s -> s
    Integer n -> Text.pack (show n)
    Command cmd -> Text.cons '\\' cmd
    Label m -> "\\label{" <> m <> "}"
    Ref ms -> "\\ref{" <> Text.intercalate ", " (toList ms) <> "}"
    BeginEnv "math" -> "$"
    EndEnv "math" -> "$"
    BeginEnv env -> "\\begin{" <> env <> "}"
    EndEnv env -> "\\end{" <> env <> "}"
    ParenL -> "("
    ParenR -> ")"
    BracketL -> "["
    BracketR -> "]"
    VisibleBraceL -> "\\{"
    VisibleBraceR -> "\\}"
    InvisibleBraceL -> "{"
    InvisibleBraceR -> "}"

tokToString :: Token -> String
tokToString = Text.unpack . tokToText

instance Pretty Token where
    pretty = \case
        Word w -> pretty w
        Variable v -> pretty (renderVariableText v)
        Symbol s -> pretty s
        Integer n -> pretty n
        Command cmd -> "\\" <> pretty cmd
        Label m -> "\\label{" <> pretty m <> "}"
        Ref m -> "\\ref{" <> pretty m <> "}"
        BeginEnv env -> "\\begin{" <> pretty env <> "}"
        EndEnv env -> "\\end{" <> pretty env <> "}"
        ParenL -> "("
        ParenR -> ")"
        BracketL -> "["
        BracketR -> "]"
        VisibleBraceL -> "\\{"
        VisibleBraceR -> "\\}"
        InvisibleBraceL -> "{"
        InvisibleBraceR -> "}"


data Located a = Located
    { startPos :: !Location
    , unLocated :: !a
    , postWhitespace :: Whitespace
    } deriving (Show, Functor)

data Whitespace = NoSpace | Space deriving (Show)

collapseWhitespace :: [Whitespace] -> Whitespace
collapseWhitespace = \case
    Space : _ -> Space
    NoSpace : ws -> collapseWhitespace ws
    [] -> NoSpace

instance Eq a  => Eq  (Located a) where (==) = (==) `on` unLocated
instance Ord a => Ord (Located a) where compare = compare `on` unLocated


data DocumentTokens = DocumentTokens
    { docImportBlock :: ![FilePath]
    , docEnvironments :: ![(Text, [Located Token])]
    } deriving (Show, Eq)

document :: Lexer ([FilePath], [[Located Token]])
document = do
    is <- importBlock
    es <- many environment
    eof
    return (is, es)


importBlock :: Lexer [FilePath]
importBlock = do
    void (skipManyTill skipChar importLineOrBeginEnv)
    many (importLine <* whitespace)
    where
        -- When skipping to the import block, we first need to try parsing whitespace to properly handle comments and avoid picking up a commented import line at the start of the import block.
        skipChar, importLineOrBeginEnv :: Lexer ()
        skipChar = comment <|> void anySingle
        importLineOrBeginEnv = lookAhead (void (Char.string "\\import{") <|> void beginToplevelEnvironment)

        importLine :: Lexer FilePath = do
            Char.string "\\import{"
            path <- some (satisfy isTheoryNameChar)
            Char.char '}'
            pure path

        isTheoryNameChar :: Char -> Bool
        isTheoryNameChar c = isAlphaNum c || c `elem` (".-_/" :: [Char])

-- TODO remove once we have a proper build system and incremental compilation
gatherImports :: Text -> [FilePath]
gatherImports raw = case runParser (evalStateT importBlock (initLexerState (FileId maxBound))) "TODO filename" raw of
    Left err -> error (errorBundlePretty err)
    Right paths -> paths


beginToplevelEnvironment :: Lexer (Located Text)
beginToplevelEnvironment = lexeme do
    Char.string "\\begin{"
    env :: Text <- asum (Char.string <$> ["definition", "theorem", "lemma", "axiom", "proof", "corollary", "proposition", "claim", "abbreviation", "datatype", "inductive", "signature", "struct"])
    Char.char '}'
    pure env

-- | Parses tokens, switching tokenizing frames when encountering math and text environments.
environment :: Lexer [Located Token]
environment = do
    env <- skipManyTill (comment <|> void anySingle) beginToplevelEnvironment
    lts <- go (unLocated env) id
    pure ((BeginEnv <$> env) : lts)
    where
        go env f = do
            frame <- topFrame
            r <- optional (nextTokenFor frame)
            case r of
                Nothing ->
                    pure (f [])
                Just t@Located{unLocated = EndEnv env'}
                    | frame == TopText && env == env' ->
                        pure (f [t])
                Just t -> do
                    modify' (advance (unLocated t))
                    go env (f . (t:))
{-# INLINE environment #-}

nextTokenFor :: Frame -> Lexer (Located Token)
nextTokenFor = \case
    TopText -> normalToken
    MathFrame{} -> mathToken
    TextFrame n -> textToken n

-- | Parses a single normal-mode token.
normalToken :: Lexer (Located Token)
normalToken =
    word <|> symbol <|> beginMath <|> beginAlign <|> subEnvironment <|> opening <|> closing <|> label <|> ref <|> end <|> command

-- | Parses a single math mode token.
mathToken :: Lexer (Located Token)
mathToken =
    var <|> symbol <|> number <|> beginCases <|> endAlign <|> endCases <|> opening <|> closing <|> beginText <|> beginExplanation <|> endMath <|> command

beginText :: Lexer (Located Token)
beginText = lexeme do
    Char.string "\\text{" <|> Char.string "\\textbox{"
    pure (BeginEnv "text")

-- | Same as text modulo spacing, so we treat it synonymously
beginExplanation :: Lexer (Located Token)
beginExplanation = lexeme do
    Char.string "\\explanation{"
    pure (BeginEnv "text")

subEnvironment :: Lexer (Located Token)
subEnvironment = beginOrEnd ["enumerate", "subproof", "byCase"]
    where
        beginOrEnd envs = asum [beginEnv env <|> endEnv env | env <- envs]
        beginEnv env = lexeme do
            Char.string ("\\begin{" <> env <> "}")
            pure (BeginEnv env)
        endEnv env = lexeme do
            Char.string ("\\end{" <> env <> "}")
            pure (EndEnv env)

-- | Normal mode embedded into math mode via @\text{...}@.
textToken :: Int -> Lexer (Located Token)
textToken n = word <|> symbol <|> textEnd <|> beginMath <|> beginAlign <|> opening' <|> closing' <|> ref <|> command
    where
        textEnd = lexeme do
            guard (n == 1)
            Char.char '}'
            pure (EndEnv "text")

        opening' = lexeme (group <|> optional (Char.string "\\left") *> (brace <|> paren <|> bracket))
            where
                brace = VisibleBraceL <$ lexeme (Char.string "\\{")
                group = InvisibleBraceL <$ lexeme (Char.char '{')
                paren = ParenL <$ lexeme (Char.char '(')
                bracket = BracketL <$ lexeme (Char.char '[')

        closing' = lexeme (group <|> optional (Char.string "\\right") *> (brace <|> paren <|> bracket))
            where
                brace = VisibleBraceR <$ lexeme (Char.string "\\}")
                group = InvisibleBraceR <$ lexeme (Char.char '}')
                paren = ParenR <$ lexeme (Char.char ')')
                bracket = BracketR <$ lexeme (Char.char ']')


-- | Parses a single begin math token.
beginMath :: Lexer (Located Token)
beginMath = lexeme do
    Char.string "\\(" <|> Char.string "\\[" <|> Char.string "$"
    pure (BeginEnv "math")

beginAlign :: Lexer (Located Token)
beginAlign = lexeme do
    Char.string "\\begin{align*}"
    pure (BeginEnv "align*")

beginCases :: Lexer (Located Token)
beginCases = lexeme do
    Char.string "\\begin{cases}"
    pure (BeginEnv "cases")

-- | Parses a single end math token.
endMath :: Lexer (Located Token)
endMath = lexeme do
    Char.string "\\)" <|> Char.string "\\]" <|> Char.string "$"
    pure (EndEnv "math")

endAlign :: Lexer (Located Token)
endAlign = lexeme do
    Char.string "\\end{align*}"
    pure (EndEnv "align*")

endCases :: Lexer (Located Token)
endCases = lexeme do
    Char.string "\\end{cases}"
    pure (EndEnv "cases")


-- | Parses the end of an environment.
-- Commits only after having seen "\end{".
end :: Lexer (Located Token)
end = lexeme do
    notFollowedBy (Char.string "\\end{cases}")
    Char.string "\\end{"
    env <- some (Char.letterChar <|> Char.char '*')
    Char.char '}'
    pure (EndEnv (Text.pack env))



-- | Parses a word. Words are returned casefolded, since we want to ignore their case later on.
word :: Lexer (Located Token)
word = lexeme do
    w <- some (Char.letterChar <|> Char.char '\'' <|> Char.char '-')
    let t = Word (Text.toCaseFold (Text.pack w))
    pure t

number :: Lexer (Located Token)
number = lexeme $ Integer <$> Lexer.decimal


var :: Lexer (Located Token)
var = lexeme (fmap Variable var')
    where
    var' = do
        alphabeticPart <- letter <|> bb <|> greek
        variationPart  <- subscript <|> ticked <|> pure ""
        pure (alphabeticPart <> variationPart)

    subscript :: Lexer Text
    subscript = do
        Char.char '_'
        unbraced <|> braced <|> text
        where
            unbraced = Text.singleton <$> Char.alphaNumChar
            braced = Text.pack <$> (Char.char '{' *> some (Char.alphaNumChar <|> tick) <* Char.char '}')
            text = Char.string "\\text" *> braced -- for rendering the subscript in roman type

    -- A bit of a hack to fit the TPTP format.
    tick :: Lexer Char
    tick = '_' <$ Char.char '\''

    ticked :: Lexer Text
    ticked = do
        ticks <- some tick
        pure (Text.pack ticks)

    letter :: Lexer Text
    letter = fmap Text.singleton Char.letterChar

    greek :: Lexer Text
    greek = try do
        Char.char '\\'
        l <- symbolParser greeks
        notFollowedBy Char.letterChar
        pure l

    greeks :: [Text]
    greeks =
        [ "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta"
        , "iota", "kappa", "lambda", "mu", "nu", "xi", "pi", "rho", "sigma"
        , "tau", "upsilon", "phi", "chi", "psi", "omega"
        , "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon"
        , "Phi", "Psi", "Omega"
        ]

    bb :: Lexer Text
    bb = do
        Char.string "\\mathbb{"
        l <-  symbolParser bbs
        Char.char '}'
        pure $ "bb" <> l

    bbs :: [Text]
    bbs = Text.singleton <$> ['A'..'Z']


    symbolParser :: [Text] -> Lexer Text
    symbolParser symbols = asum (fmap Char.string symbols)


symbol :: Lexer (Located Token)
symbol = lexeme do
    symb <- some (satisfy (`elem` symbols))
    pure (Symbol (Text.pack symb))
        where
        symbols :: [Char]
        symbols = ".,:;!?@=≠+-/|^><≤≥*&≈⊂⊃⊆⊇∈“”‘’"

-- | Parses a TEX-style command.
command :: Lexer (Located Token)
command = lexeme do
    Char.char '\\'
    cmd <- some Char.letterChar
    pure (Command (Text.pack cmd))

-- | Parses a label command and extracts its marker.
label :: Lexer (Located Token)
label = lexeme do
    Char.string "\\label{"
    m <- marker
    Char.char '}'
    pure (Label m)

-- | Parses a label command and extracts its marker.
ref :: Lexer (Located Token)
ref = lexeme do
    -- @\\cref@ is from @cleveref@ and @\\hyperref@ is from @hyperref@
    cmd <- Char.string "\\ref{" <|> Char.string "\\cref{" <|> Char.string "\\hyperref["
    ms <- NonEmpty.fromList <$> marker `sepBy1` Char.char ','
    case cmd of
        "\\hyperref[" -> Char.string "]{" *> some (satisfy (/= '}')) *> Char.char '}' *> pure (Ref ms)
        _ -> Char.char '}' *> pure (Ref ms)

marker :: Lexer Text
marker = do
    c <- satisfy isAsciiLetter
    cs <- takeWhileP Nothing isAsciiAlphaNumOrUnderscore
    pure (Text.cons c cs)

-- | Parses an opening delimiter.
opening :: Lexer (Located Token)
opening = lexeme (group <|> optional (Char.string "\\left") *> (paren <|> brace <|> bracket))
    where
        brace = VisibleBraceL <$ lexeme (Char.string "\\{")
        group = InvisibleBraceL <$ lexeme (Char.char '{')
        paren = ParenL <$ lexeme (Char.char '(')
        bracket = BracketL <$ lexeme (Char.char '[')

-- | Parses a closing delimiter.
closing :: Lexer (Located Token)
closing = lexeme (group <|> optional (Char.string "\\right") *> (paren <|> brace <|> bracket))
    where
        brace = VisibleBraceR <$ lexeme (Char.string "\\}")
        group = InvisibleBraceR <$ lexeme (Char.char '}')
        paren = ParenR <$ lexeme (Char.char ')')
        bracket = BracketR <$ lexeme (Char.char ']')

-- | Turns a Lexer into one that tracks the source position of the token
-- and consumes trailing whitespace.
lexeme :: Lexer a -> Lexer (Located a)
lexeme p = do
    fileId <- gets currentFileId
    start <- getSourcePos
    t <- p
    w <- whitespace
    pure (Located (fromSourcePos fileId start) t w)

space :: Lexer Whitespace
space = Space <$ (Char.char ' ' <|> Char.char '\n' <|> Char.char '\r')
    <|> Space <$ (Char.string "\\ " <|> Char.string "\\\\" <|> Char.string "\\!" <|> Char.string "\\," <|> Char.string "\\:" <|> Char.string "\\;" <|> Char.string "\\;")

whitespace :: Lexer Whitespace
whitespace = do
    ws <- many (spaces <|> NoSpace <$ comment)
    pure (collapseWhitespace ws)
    where
        spaces = collapseWhitespace <$> some space

comment :: Lexer ()
comment = Lexer.skipLineComment "%"
