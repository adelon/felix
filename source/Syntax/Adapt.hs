{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Syntax.Adapt where

import Base
import Syntax.Abstract
import Syntax.Lexicon
import Report.Location

import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Text.Regex.Applicative qualified as RE
import Text.Regex.Applicative (RE)

scanChunk :: [Located Token] -> [ScannedLexicalItem]
scanChunk ltoks =
    let toks = unLocated <$> ltoks
        matchOrErr re env pos = RE.match re toks ?? error ("could not find lexical pattern in " <> env <> " at " <> prettyLocation pos)
    in case ltoks of
        Located{startPos = pos, unLocated = BeginEnv "definition"} : _ ->
            matchOrErr definition "definition" pos
        Located{startPos = pos, unLocated = BeginEnv "signature"} : _ ->
            matchOrErr signatureExtension "signature" pos
        Located{startPos = pos, unLocated = BeginEnv "abbreviation"} : _ ->
            matchOrErr abbreviation "abbreviation" pos
        Located{startPos = pos, unLocated = (BeginEnv "struct")} :_ ->
            matchOrErr structRE "struct definition" pos
        Located{startPos = pos, unLocated = (BeginEnv "inductive")} :_ ->
            matchOrErr inductive "inductive definition" pos
        _ -> []

adaptChunks :: [[Located Token]] -> Lexicon -> Lexicon
adaptChunks = extendLexicon . concatMap scanChunk

data ScannedLexicalItem
    = ScanAdj LexicalPhrase Marker
    | ScanFun LexicalPhrase Marker
    | ScanNoun LexicalPhrase Marker
    | ScanStructNoun LexicalPhrase Marker
    | ScanVerb LexicalPhrase Marker
    | ScanRelationSymbol Token Marker
    | ScanFunctionSymbol Pattern Marker
    | ScanPrefixPredicate PrefixPredicate Marker
    | ScanStructOp Text -- we an use the command text as export name.
    deriving (Show, Eq)

skipUntilNextLexicalEnv :: RE Token [Token]
skipUntilNextLexicalEnv = many (RE.psym otherToken)
    where
        otherToken tok = tok /= BeginEnv "definition" && tok /= BeginEnv "struct" && tok /= BeginEnv "abbreviation"

notEndOfLexicalEnvToken :: RE Token Token
notEndOfLexicalEnvToken = RE.psym innerToken
    where
        innerToken tok = tok /= EndEnv "definition" && tok /= EndEnv "struct" && tok /= EndEnv "abbreviation"

definition ::  RE Token [ScannedLexicalItem]
definition = do
    RE.sym (BeginEnv "definition")
    RE.few notEndOfLexicalEnvToken
    m <- labelRE
    RE.few RE.anySym
    lexicalItem <- headRE
    RE.few RE.anySym
    RE.sym (EndEnv "definition")
    skipUntilNextLexicalEnv
    pure [lexicalItem m]

abbreviation ::  RE Token [ScannedLexicalItem]
abbreviation = do
    RE.sym (BeginEnv "abbreviation")
    RE.few RE.anySym
    m <- labelRE
    RE.few RE.anySym
    lexicalItem <- headRE
    RE.few RE.anySym
    RE.sym (EndEnv "abbreviation")
    skipUntilNextLexicalEnv
    pure [lexicalItem m]

signatureExtension ::  RE Token [ScannedLexicalItem]
signatureExtension = do
    RE.sym (BeginEnv "signature")
    RE.few notEndOfLexicalEnvToken
    m <- labelRE
    RE.few RE.anySym
    lexicalItem <- sigHeadRE
    RE.few RE.anySym
    RE.sym (EndEnv "signature")
    skipUntilNextLexicalEnv
    pure [lexicalItem m]

signatureExtensionAtom ::  RE Token [ScannedLexicalItem]
signatureExtensionAtom = do
    RE.sym (BeginEnv "signatureatom")
    RE.few notEndOfLexicalEnvToken
    m <- labelRE
    RE.few RE.anySym
    lexicalItem <- sigPred
    RE.few RE.anySym
    RE.sym (EndEnv "signatureatom")
    skipUntilNextLexicalEnv
    pure [lexicalItem m]

labelRE :: RE Token Marker
labelRE = RE.msym \case
    Label m -> Just (Marker m)
    _ -> Nothing

-- | 'RE' that matches the head of a definition.
headRE :: RE Token (Marker -> ScannedLexicalItem)
-- Note that @<|>@ is left biased for 'RE', so we can just
-- place 'adj' before 'verb' and do not have to worry about
-- overlapping patterns.
headRE = ScanNoun <$> nounRE
    <|> ScanAdj <$> adjRE
    <|> ScanVerb <$> verbRE
    <|> ScanFun <$> funRE
    <|> ScanRelationSymbol . fst <$> relationSymbolRE
    <|> ScanFunctionSymbol <$> functionSymbolRE
    <|> ScanPrefixPredicate <$> prefixPredicate

sigHeadRE :: RE Token (Marker -> ScannedLexicalItem)
sigHeadRE = ScanFunctionSymbol <$> sigFunctionSymbolRE

sigFunctionSymbolRE :: RE Token Pattern
sigFunctionSymbolRE = do
    RE.sym (BeginEnv "math")
    toks <- RE.few nonDefinitionKeyword
    RE.sym (EndEnv "math")
    pure (makeFunctionSymbol toks)

sigPred :: RE Token (Marker -> ScannedLexicalItem)
sigPred = ScanNoun . toLexicalPhrase <$> (math var *> can *> be *> an *> patRE <* iff)
    <|> ScanAdj . toLexicalPhrase <$> (math var *> can *> be *> patRE <* iff)
    -- <|> ScanVerbInfinitive . toLexicalPhrase <$> (math var *> can *> pat <* iff)

inductive :: RE Token [ScannedLexicalItem]
inductive = do
    RE.sym (BeginEnv "inductive")
    RE.few notEndOfLexicalEnvToken
    m <- labelRE
    RE.few RE.anySym
    lexicalItem <- functionSymbolInductive
    RE.few RE.anySym
    RE.sym (EndEnv "inductive")
    skipUntilNextLexicalEnv
    pure [ScanFunctionSymbol lexicalItem m]

structRE :: RE Token [ScannedLexicalItem]
structRE = do
    RE.sym (BeginEnv "struct")
    RE.few RE.anySym
    m <- labelRE
    RE.few RE.anySym
    lexicalItem <- ScanStructNoun . toLexicalPhrase <$> (an *> structPat <* math var)
    RE.few RE.anySym
    lexicalItems <- structOps <|> pure []
    RE.sym (EndEnv "struct")
    skipUntilNextLexicalEnv
    pure (lexicalItem m : lexicalItems)

structOps :: RE Token [ScannedLexicalItem]
structOps = do
    RE.sym (BeginEnv "enumerate")
    lexicalItems <- many structOp
    RE.sym (EndEnv "enumerate")
    RE.few RE.anySym
    pure lexicalItems

structOp :: RE Token ScannedLexicalItem
structOp = do
    RE.sym (Command "item")
    op <- math command
    pure (ScanStructOp op)

nounRE :: RE Token LexicalPhrase
nounRE = toLexicalPhrase <$> (math var *> is *> an *> patRE <* iff)

adjRE :: RE Token LexicalPhrase
adjRE = toLexicalPhrase <$> (math var *> is *> patRE <* iff)

verbRE :: RE Token LexicalPhrase
verbRE = toLexicalPhrase <$> (math var *> patRE <* iff)

funRE :: RE Token LexicalPhrase
funRE = toLexicalPhrase <$> (the *> patRE <* (is <|> comma))

relationSymbolRE :: RE Token (Token, Int)
relationSymbolRE = do
    beginMath
    var
    rel <- symbol
    k <- params
    var
    endMath
    iff
    pure (rel, k)
        where
        params :: RE Token Int
        params = do
            vars <- many (RE.sym InvisibleBraceL *> var <* RE.sym InvisibleBraceR)
            pure (length vars)

functionSymbolRE :: RE Token Pattern
functionSymbolRE = do
    RE.sym (BeginEnv "math")
    toks <- RE.few nonDefinitionKeyword
    RE.sym (Symbol "=")
    pure (makeFunctionSymbol toks)

makeFunctionSymbol :: [Token] -> Pattern
makeFunctionSymbol = \case
    -- TODO proper error messages with more info (location, etc.)
    [] -> error "Malformed function pattern: no pattern"
    [Variable _] -> error "Malformed function: bare variable. This will cause infinite left recursion in the grammar and cause the parser to hang!"
    [Variable _, ParenL, Variable _, ParenR] -> error "Malformed function: redefinition of function application. The notation _(_) is reserved for set-theoretic function application."
    toks -> patternFromHoley (fromToken <$> toks)
    where
        fromToken = \case
            Variable _ -> Nothing -- Variables become slots.
            tok -> Just tok -- Everything else is part of the pattern.

functionSymbolInductive :: RE Token Pattern
functionSymbolInductive = do
    RE.sym (BeginEnv "math")
    toks <- RE.few nonDefinitionKeyword
    RE.sym (Command "subseteq")
    pure (patternFromHoley (fromToken <$> toks))
    where
        fromToken = \case
            Variable _ -> Nothing -- Variables become slots.
            tok -> Just tok -- Everything else is part of the pattern.

prefixPredicate :: RE Token PrefixPredicate
prefixPredicate = math prfx <* iff
    where
        prfx = do
            r <- command
            args <- many (RE.sym InvisibleBraceL *> var <* RE.sym InvisibleBraceR)
            pure (PrefixPredicate r (length args))


command :: RE Token Text
command = RE.msym \case
    Command cmd -> Just cmd
    _ -> Nothing

var :: RE Token Token
var = RE.psym isVar


nonDefinitionKeyword :: RE Token Token
nonDefinitionKeyword = RE.psym (`notElem` keywords)
    where
        keywords =
            [ Word "if"
            , Word "iff"
            , Symbol "="
            , Command "iff"
            , BeginEnv "math"
            , EndEnv "math"
            ]


patRE :: RE Token [Token]
patRE = many (RE.psym isLexicalPhraseToken <|> math var)

structPat :: RE Token [Token]
structPat = many (RE.psym isLexicalPhraseToken)

beginMath, endMath :: RE Token ()
beginMath = void (RE.sym (BeginEnv "math"))
endMath = void (RE.sym (EndEnv "math"))

math :: RE Token a -> RE Token a
math re = beginMath *> re <* endMath

-- | We allow /conditional perfection/: the first /@if@/ in a definition is interpreted as /@iff@/.
iff :: RE Token ()
iff = void (RE.sym (Word "if")) -- Using @void@ is faster (only requires recognition).
    <|> void (RE.sym (Word "iff"))
    <|> void (RE.string [Word "if", Word "and", Word "only", Word "if"])
    <|> void (RE.sym (Word "denote"))
    <|> void (RE.sym (Word "stand") *> RE.sym (Word "for"))
{-# INLINE iff #-}

an :: RE Token ()
an = void (RE.sym (Word "a"))
    <|> void (RE.sym (Word "an"))
{-# INLINE an #-}

is :: RE Token ()
is = void (RE.sym (Word "is") <|> RE.sym (Word "denotes"))
{-# INLINE is #-}

can :: RE Token ()
can = void (RE.sym (Word "can"))
{-# INLINE can #-}

be :: RE Token ()
be = void (RE.sym (Word "be"))
{-# INLINE be #-}

the :: RE Token ()
the = void (RE.sym (Word "the"))
{-# INLINE the #-}

comma :: RE Token ()
comma = void (RE.sym (Symbol ","))
{-# INLINE comma #-}


isVar :: Token -> Bool
isVar = \case
    Variable _ -> True
    _token -> False

isCommand :: Token -> Bool
isCommand = \case
    Command _ -> True
    _token -> False

isNotDefnToken :: Token -> Bool
isNotDefnToken = \case
    BeginEnv "definition" -> False
    EndEnv "definition" -> False
    _token -> True

isLexicalPhraseToken :: Token -> Bool
isLexicalPhraseToken = \case
    Word w -> w `Set.notMember` keywords
    --
    -- Simple commands (outside of math-mode) are allowed. This is useful
    -- for defining lexical phrases containing symbolic expressions such as
    -- `X is \Ttwo{}`, where `\Ttwo` is a macro that expands to `T_2`.
    -- We also allow these macros to take arguments, hence the need to
    -- allow grouping delimiters. They can also be used to escape the end
    -- of the command for correct spacing, as in the above example.
    --
    Command _cmd -> True
    InvisibleBraceL -> True
    InvisibleBraceR -> True
    --
    -- No other tokens may occur in lexical phrases. In particular, no `_dot`
    -- token may occur, limiting the lexical phrase to a single sentence.
    -- Commas occurring in variable lists should be placed
    -- within the math environment. Thus `$a,b$ are coprime iff`,
    -- not `$a$,`$b$` are coprime iff`.
    --
    _token -> False
    where
        keywords = Set.fromList ["a", "an", "is", "are", "if", "iff", "denote", "stand", "let"]


toLexicalPhrase :: [Token] -> LexicalPhrase
toLexicalPhrase toks = component <$> toks
    where
        component = \case
            Variable _ -> Nothing
            tok        -> Just tok


symbol :: RE Token Token
symbol = RE.msym $ \tok -> case tok of
    Command _ -> Just tok
    Symbol _ -> Just tok
    _tok -> Nothing


-- | Basic paradigms for pluralizations of nominals.
guessNounPlural :: LexicalPhrase -> SgPl LexicalPhrase
guessNounPlural item = SgPl item (pluralize item)
    where
        pluralize :: LexicalPhrase -> LexicalPhrase
        pluralize = \case
            Just (Word w) : pat'@(Just w' : _) | isPreposition w' -> Just (Word (Text.snoc w 's')) : pat'
            tok : Just (Word w) : pat'@(Just w' : _) | isPreposition w' -> tok : Just (Word (Text.snoc w 's')) : pat'
            tok1 : tok2 : Just (Word w) : pat'@(Just w' : _) | isPreposition w' -> tok1 : tok2 : Just (Word (Text.snoc w 's')) : pat'
            [Just (Word w)] -> [Just (Word (Text.snoc w 's'))]
            [tok, Just (Word w)] -> [tok, Just (Word (Text.snoc w 's'))]
            [tok, tok', Just (Word w)] -> [tok, tok', Just (Word (Text.snoc w 's'))]
            pat' -> pat'

guessVerbPlural :: LexicalPhrase -> SgPl LexicalPhrase
guessVerbPlural item = SgPl item itemPl
    where
        itemPl = case item of
            Just (Word v) : rest -> case Text.unsnoc v of
                Just (v', 's') -> Just (Word v') : rest
                _ -> item
            _ -> item

isAdjR :: LexicalPhrase -> Bool
isAdjR item = containsPreposition item || containsSlot item
    where
        containsPreposition, containsSlot :: LexicalPhrase -> Bool
        containsPreposition = any isPreposition . catMaybes
        containsSlot = (Nothing `elem`)

isPreposition :: Token -> Bool
isPreposition w = Set.member w (Set.map Word prepositions)

-- | Right-biased set insertion, keeping the original set
-- when inserting already present elements. @insertR@ is unfortunately
-- a hidden function, even in @Data.Set.Internal@, so we approximate it
-- here. In theory one could avoid the indirection of first forming the singleton
-- set on the rhs.
insertR' :: Ord a => a -> Set a -> Set a
insertR' x xs = xs `Set.union` Set.singleton x -- @union@ is left-biased.

insertMapR :: Ord k => k -> a -> Map k a -> Map k a
insertMapR k x xs = xs `Map.union` Map.singleton k x -- @union@ is left-biased.


insertR :: Ord k => k -> a -> Map k a -> Map k a
insertR k x xs = xs `Map.union` Map.singleton k x -- @union@ is left-biased.

-- | Takes the scanned lexical phrases and inserts them in the correct
-- places in a lexicon.
extendLexicon :: [ScannedLexicalItem] -> Lexicon -> Lexicon
extendLexicon [] lexicon = lexicon
-- Note that we only consider 'sg' in the 'Ord' instance of SgPl, so that
-- known irregular plurals are preserved.
extendLexicon (scan : scans) lexicon@Lexicon{..} = case scan of
    ScanAdj item m ->
        let li = mkLexicalItem item m
            pat = lexicalItemPattern li
        in if isAdjR item
            then
                let (items', patterns') = insertItem pat li lexiconAdjRs lexiconAllPatterns
                in extendLexicon scans lexicon{lexiconAdjRs = items', lexiconAllPatterns = patterns'}
            else
                let (items', patterns') = insertItem pat li lexiconAdjLs lexiconAllPatterns
                in extendLexicon scans lexicon{lexiconAdjLs = items', lexiconAllPatterns = patterns'}
    ScanFun item m ->
        let li = mkLexicalItemSgPl (guessNounPlural item) m
            pat = baseSgPlPattern li
            (items', patterns') = insertItem pat li lexiconFuns lexiconAllPatterns
        in extendLexicon scans lexicon{lexiconFuns = items', lexiconAllPatterns = patterns'}
    ScanVerb item m ->
        let li = mkLexicalItemSgPl (guessVerbPlural item) m
            pat = baseSgPlPattern li
            (items', patterns') = insertItem pat li lexiconVerbs lexiconAllPatterns
        in extendLexicon scans lexicon{lexiconVerbs = items', lexiconAllPatterns = patterns'}
    ScanNoun item m ->
        let li = mkLexicalItemSgPl (guessNounPlural item) m
            pat = baseSgPlPattern li
            (items', patterns') = insertItem pat li lexiconNouns lexiconAllPatterns
        in extendLexicon scans lexicon{lexiconNouns = items', lexiconAllPatterns = patterns'}
    ScanStructNoun item m ->
        let li = mkLexicalItemSgPl (guessNounPlural item) m
            pat = baseSgPlPattern li
            (items', patterns') = insertItem pat li lexiconStructNouns lexiconAllPatterns
        in extendLexicon scans lexicon{lexiconStructNouns = items', lexiconAllPatterns = patterns'}
    ScanRelationSymbol item m ->
        let sym = RelationSymbol item m
            pat = relationSymbolPattern sym
            (items', patterns') = insertItem pat sym lexiconRelationSymbols lexiconAllPatterns
        in extendLexicon scans lexicon{lexiconRelationSymbols = items', lexiconAllPatterns = patterns'}
    ScanFunctionSymbol pat m ->
        if mixfixPatternExists pat lexiconAllPatterns
            then warnExists pat (extendLexicon scans lexicon)
            else extendLexicon scans lexicon
                { lexiconMixfixTable = Seq.adjust (Map.insert pat (MixfixItem pat m NonAssoc)) 9 lexiconMixfixTable
                , lexiconAllPatterns = Set.insert pat lexiconAllPatterns
                }
    ScanStructOp op ->
        let sym = StructSymbol op
            pat = structSymbolPattern sym
            (items', patterns') = insertStruct pat sym lexiconStructFun lexiconAllPatterns
        in extendLexicon scans lexicon{lexiconStructFun = items', lexiconAllPatterns = patterns'}
    ScanPrefixPredicate tok m ->
        let pat = prefixPattern tok
            (items', patterns') = insertPrefix pat (tok, m) lexiconPrefixPredicates lexiconAllPatterns
        in extendLexicon scans lexicon{lexiconPrefixPredicates = items', lexiconAllPatterns = patterns'}

mixfixPatternExists :: Pattern -> Set Pattern -> Bool
mixfixPatternExists pat patterns = Set.member pat patterns

insertItem :: Pattern -> a -> [a] -> Set Pattern -> ([a], Set Pattern)
insertItem pat item items patterns =
    if Set.member pat patterns then warnExists pat (items, patterns) else (item : items, Set.insert pat patterns)

insertStruct :: Pattern -> StructSymbol -> [StructSymbol] -> Set Pattern -> ([StructSymbol], Set Pattern)
insertStruct pat item items patterns =
    if Set.member pat patterns then warnExists pat (items, patterns) else (item : items, Set.insert pat patterns)

insertPrefix :: Pattern -> (PrefixPredicate, Marker) -> [(PrefixPredicate, Marker)] -> Set Pattern -> ([(PrefixPredicate, Marker)], Set Pattern)
insertPrefix pat item items patterns =
    if Set.member pat patterns then warnExists pat (items, patterns) else (item : items, Set.insert pat patterns)

baseSgPlPattern :: LexicalItemSgPl -> Pattern
baseSgPlPattern = sg . lexicalItemSgPlPattern

prefixPattern :: PrefixPredicate -> Pattern
prefixPattern (PrefixPredicate cmd _arity) =
    TokenCons (Command cmd) End

warnExists :: Pattern -> a -> a
warnExists pat = trace ("% WARNING: Lexical pattern already exists: " <> show pat)
