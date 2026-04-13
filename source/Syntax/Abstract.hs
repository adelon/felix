{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Data types for the abstract syntax tree and helper functions
-- for constructing the lexicon.
--
module Syntax.Abstract
    ( module Syntax.Abstract
    , module Syntax.LexicalPhrase
    , module Syntax.Token
    ) where


import Base
import Syntax.LexicalPhrase (LexicalPhrase, SgPl(..), unsafeReadPhraseSgPl, unsafeReadPhrase)
import Syntax.Token (Token(..), Located(..))
import Report.Location

import Text.Earley.Mixfix (Holey)
import Data.Text qualified as Text

-- | Local "variable-like" symbols that can be captured by binders.
data VarSymbol
    = NamedVarAt Location Text -- ^ A named variable.
    | FreshVarAt Location Int -- ^ A nameless (implicit) variable. Should only come from desugaring.
    deriving (Generic)

pattern NamedVar :: Text -> VarSymbol
pattern NamedVar x <- NamedVarAt _ x where
    NamedVar x = NamedVarAt Nowhere x

pattern FreshVar :: Int -> VarSymbol
pattern FreshVar n <- FreshVarAt _ n where
    FreshVar n = FreshVarAt Nowhere n

{-# COMPLETE NamedVarAt, FreshVarAt #-}
{-# COMPLETE NamedVar, FreshVar #-}

instance Show VarSymbol where
    showsPrec d = \case
        NamedVarAt _ x ->
            showParen (d > 10) (showString "NamedVar " . showsPrec 11 x)
        FreshVarAt _ n ->
            showParen (d > 10) (showString "FreshVar " . showsPrec 11 n)

instance Eq VarSymbol where
    NamedVarAt _ x == NamedVarAt _ y = x == y
    FreshVarAt _ n == FreshVarAt _ m = n == m
    _ == _ = False

instance Ord VarSymbol where
    compare (NamedVarAt _ x) (NamedVarAt _ y) = compare x y
    compare NamedVarAt{} FreshVarAt{} = LT
    compare FreshVarAt{} NamedVarAt{} = GT
    compare (FreshVarAt _ n) (FreshVarAt _ m) = compare n m

instance Hashable VarSymbol where
    hashWithSalt s = \case
        NamedVarAt _ x -> hashWithSalt s (0 :: Int, x)
        FreshVarAt _ n -> hashWithSalt s (1 :: Int, n)

instance IsString VarSymbol where
    fromString v = NamedVar $ Text.pack v

instance Locatable VarSymbol where
    locate = \case
        NamedVarAt l _ -> l
        FreshVarAt l _ -> l

data Expr
    = ExprVar VarSymbol
    | ExprInteger Location Int
    | ExprOp Location MixfixItem [Expr]
    | ExprStructOp Location StructSymbol (Maybe Expr)
    | ExprFiniteSet Location (NonEmpty Expr)
    | ExprSep Location VarSymbol Expr Stmt
    -- ^ Of the form /@{x ∈ X | P(x)}@/.
    | ExprReplace Location Expr (NonEmpty (VarSymbol,Expr)) (Maybe Stmt)
    -- ^ E.g.: /@{ f(x, y) | x ∈ X, y ∈ Y | P(x, y) }@/.
    | ExprReplacePred Location VarSymbol VarSymbol Expr Stmt
    -- ^ E.g.: /@{ y | \\exists x\\in X. P(x, y) }@/.
    deriving (Show, Eq, Ord)

instance Locatable Expr where
    locate = \case
        ExprVar x -> locate x
        ExprInteger l _ -> l
        ExprOp l _ _ -> l
        ExprStructOp l _ _ -> l
        ExprFiniteSet l _ -> l
        ExprSep l _ _ _ -> l
        ExprReplace l _ _ _ -> l
        ExprReplacePred l _ _ _ _ -> l


data LexicalItem = LexicalItem Pattern Marker deriving (Show, Generic)

instance Eq LexicalItem where
    LexicalItem p _ == LexicalItem p' _ = p == p'

instance Ord LexicalItem where
    compare (LexicalItem p _) (LexicalItem p' _) = compare p p'

instance Hashable LexicalItem where
    hashWithSalt s (LexicalItem p _) = hashWithSalt s p

data LexicalItemSgPl = LexicalItemSgPl (SgPl Pattern) Marker deriving (Show, Generic)

instance Eq LexicalItemSgPl where
    LexicalItemSgPl p _ == LexicalItemSgPl p' _ = sg p == sg p'

instance Ord LexicalItemSgPl where
    compare (LexicalItemSgPl p _) (LexicalItemSgPl p' _) = compare (sg p) (sg p')

instance Hashable LexicalItemSgPl where
    hashWithSalt s (LexicalItemSgPl p _) = hashWithSalt s (sg p)

data Associativity
  = LeftAssoc
  | NonAssoc
  | RightAssoc
  deriving (Eq, Show, Ord, Generic, Hashable)

data MixfixItem = MixfixItem Pattern Marker Associativity deriving (Eq, Show, Ord, Generic, Hashable)

data Pattern = End | HoleCons Pattern | TokenCons Token Pattern deriving (Eq, Show, Ord, Generic, Hashable)

type FunctionSymbol = MixfixItem

data RelationSymbol = RelationSymbol Token Marker deriving (Show, Eq, Ord, Generic, Hashable)

newtype StructSymbol = StructSymbol { unStructSymbol :: Text } deriving newtype (Show, Eq, Ord, Hashable)

pattern ElementSymbol, NotElementSymbol :: RelationSymbol
pattern ElementSymbol = RelationSymbol (Command "in") "elem"
pattern NotElementSymbol = RelationSymbol (Command "notin") "notelem"

pattern EqSymbol, NeqSymbol, SubseteqSymbol :: RelationSymbol
pattern EqSymbol = RelationSymbol (Symbol "=") "eq"
pattern NeqSymbol = RelationSymbol (Command "neq") "neq"
pattern SubseteqSymbol = RelationSymbol (Command "subseteq") "subseteq"

-- | The predefined @cons@ function symbol used for desugaring finite set expressions.
pattern ConsSymbol :: FunctionSymbol
pattern ConsSymbol =
    MixfixItem
        (TokenCons (Command "cons")
            (TokenCons InvisibleBraceL
                (HoleCons
                    (TokenCons InvisibleBraceR
                        (TokenCons InvisibleBraceL
                            (HoleCons
                                (TokenCons InvisibleBraceR End)))))))
        "cons"
        NonAssoc

-- | The predefined @pair@ function symbol used for desugaring tuple notation..
pattern PairSymbol :: FunctionSymbol
pattern PairSymbol =
    MixfixItem
        (TokenCons (Command "pair")
            (TokenCons InvisibleBraceL
                (HoleCons
                    (TokenCons InvisibleBraceR
                        (TokenCons InvisibleBraceL
                            (HoleCons
                                (TokenCons InvisibleBraceR End)))))))
        "pair"
        NonAssoc

-- | Function application /@f(x)@/ desugars to /@\apply{f}{x}@/.
pattern ApplySymbol :: FunctionSymbol
pattern ApplySymbol =
    MixfixItem
        (TokenCons (Command "apply")
            (TokenCons InvisibleBraceL
                (HoleCons
                    (TokenCons InvisibleBraceR
                        (TokenCons InvisibleBraceL
                            (HoleCons
                                (TokenCons InvisibleBraceR End)))))))
        "apply"
        NonAssoc

pattern DomSymbol :: FunctionSymbol
pattern DomSymbol =
    MixfixItem
        (TokenCons (Command "dom")
            (TokenCons InvisibleBraceL
                (HoleCons
                    (TokenCons InvisibleBraceR End))))
        "dom"
        NonAssoc

pattern CarrierSymbol :: StructSymbol
pattern CarrierSymbol = StructSymbol "carrier"

patternFromHoley :: Holey Token -> Pattern
patternFromHoley = foldr step End
    where
        step = \case
            Nothing -> HoleCons
            Just tok -> TokenCons tok

patternToHoley :: Pattern -> Holey Token
patternToHoley = \case
    End -> []
    HoleCons pat -> Nothing : patternToHoley pat
    TokenCons tok pat -> Just tok : patternToHoley pat

mixfixPattern :: MixfixItem -> Pattern
mixfixPattern (MixfixItem pat _ _) = pat

mixfixMarker :: MixfixItem -> Marker
mixfixMarker (MixfixItem _ m _) = m

mixfixAssoc :: MixfixItem -> Associativity
mixfixAssoc (MixfixItem _ _ assoc) = assoc

mkMixfixItem :: Holey Token -> Marker -> Associativity -> MixfixItem
mkMixfixItem pat m assoc = MixfixItem (patternFromHoley pat) m assoc

lexicalItemPattern :: LexicalItem -> Pattern
lexicalItemPattern (LexicalItem pat _) = pat

lexicalItemMarker :: LexicalItem -> Marker
lexicalItemMarker (LexicalItem _ m) = m

lexicalItemPhrase :: LexicalItem -> LexicalPhrase
lexicalItemPhrase = patternToHoley . lexicalItemPattern

lexicalItemSgPlPattern :: LexicalItemSgPl -> SgPl Pattern
lexicalItemSgPlPattern (LexicalItemSgPl pat _) = pat

lexicalItemSgPlMarker :: LexicalItemSgPl -> Marker
lexicalItemSgPlMarker (LexicalItemSgPl _ m) = m

lexicalItemSgPlPhrase :: LexicalItemSgPl -> SgPl LexicalPhrase
lexicalItemSgPlPhrase = fmap patternToHoley . lexicalItemSgPlPattern

mkLexicalItem :: LexicalPhrase -> Marker -> LexicalItem
mkLexicalItem pat m = LexicalItem (patternFromHoley pat) m

mkLexicalItemSgPl :: SgPl LexicalPhrase -> Marker -> LexicalItemSgPl
mkLexicalItemSgPl pat m = LexicalItemSgPl (patternFromHoley <$> pat) m

relationSymbolToken :: RelationSymbol -> Token
relationSymbolToken (RelationSymbol tok _) = tok

relationSymbolMarker :: RelationSymbol -> Marker
relationSymbolMarker (RelationSymbol _ m) = m

relationSymbolPattern :: RelationSymbol -> Pattern
relationSymbolPattern rel =
    HoleCons (TokenCons (relationSymbolToken rel) (HoleCons End))

structSymbolPattern :: StructSymbol -> Pattern
structSymbolPattern (StructSymbol c) = TokenCons (Command c) End

patternToken :: Pattern -> Maybe Token
patternToken = \case
    TokenCons tok End -> Just tok
    _ -> Nothing

markerFromToken :: Token -> Marker
markerFromToken = \case
    Word w -> Marker w
    Symbol s -> Marker s
    Command c -> Marker c
    Integer n -> Marker (Text.pack (show n))
    tok -> error ("markerFromToken: unsupported token " <> show tok)

pattern ExprConst :: Location -> Token -> Expr
pattern ExprConst l c <- ExprOp l (MixfixItem (TokenCons c End) _ NonAssoc) []
    where
        ExprConst l c = ExprOp l (MixfixItem (TokenCons c End) (markerFromToken c) NonAssoc) []

pattern ExprApp :: Location -> Expr -> Expr -> Expr
pattern ExprApp loc e1 e2 = ExprOp loc ApplySymbol [e1, e2]

pattern ExprPair :: Location -> Expr -> Expr -> Expr
pattern ExprPair loc e1 e2 = ExprOp loc PairSymbol [e1, e2]

-- | Tuples are interpreted as nested pairs:
-- the triple /@(a, b, c)@/ is interpreted as
-- /@(a, (b, c))@/.
-- This means that the product operation should also
-- be right associative, so that /@(a, b, c)@/ can
-- form elements of /@A\times B\times C@/.
makeTuple :: Location -> NonEmpty Expr -> Expr
makeTuple l = \case
    e :| [] -> e
    e :| (e' : es) -> ExprPair l e (makeTuple l (e' :| es))


data Chain
    = ChainBase (NonEmpty Expr) Sign Relation (NonEmpty Expr) -- left arguments, possibly empty list of parameters, right arguments
    | ChainCons (NonEmpty Expr) Sign Relation Chain
    deriving (Show, Eq, Ord)

instance Locatable Chain where
    locate (ChainBase lhs _ _ _) = locate lhs
    locate (ChainCons lhs _ _ _) = locate lhs

data Relation
    = Relation Location RelationSymbol [Expr] -- ^  E.g.: /@x \in X@/, potentially with parameters in braces
    | RelationExpr Location Expr   -- ^  E.g.: /@x \mathrel{R} y@/
    deriving (Show, Eq, Ord)

instance Locatable Relation where
    locate = \case
        Relation l _ _ -> l
        RelationExpr l _ -> l

data Sign = Positive | Negative deriving (Show, Eq, Ord)

data Formula
    = FormulaChain Chain
    | FormulaPredicate Location PrefixPredicate Marker (NonEmpty Expr)
    | Connected Location Connective Formula Formula
    | FormulaNeg Location Formula
    | FormulaQuantified Location Quantifier (NonEmpty VarSymbol) Bound Formula
    | PropositionalConstant Location PropositionalConstant
    deriving (Show, Eq, Ord)

instance Locatable Formula where
    locate = \case
        FormulaChain chain -> locate chain
        FormulaPredicate l _ _ _ -> l
        Connected l _ _ _ -> l
        FormulaNeg l _ -> l
        FormulaQuantified l _ _ _ _ -> l
        PropositionalConstant l _ -> l

data PropositionalConstant = IsBottom | IsTop
    deriving (Show, Eq, Ord, Generic, Hashable)

data PrefixPredicate
    = PrefixPredicate Text Int
    deriving (Show, Eq, Ord, Generic, Hashable)


data Connective
    = Conjunction
    | Disjunction
    | Implication
    | Equivalence
    | ExclusiveOr
    | NegatedDisjunction
    deriving (Show, Eq, Ord, Generic, Hashable)



mixfixLoc :: Locatable a => Holey (Located Token) -> [a] -> Location
mixfixLoc parts args0 = go parts args0
    where
        go [] _ = Nowhere
        go (Just ltok : _parts') _args' = startPos ltok
        go (Nothing : parts') (a : args')
            | locate a == Nowhere = go parts' args'
            | otherwise = locate a
        go (Nothing : parts') [] = go parts' []

makeConnective :: Holey (Located Token) -> [Formula] -> Formula
makeConnective parts@[Nothing, Just Located{unLocated = Command "implies"}, Nothing] [f1, f2] = Connected (mixfixLoc parts [f1, f2]) Implication  f1 f2
makeConnective parts@[Nothing, Just Located{unLocated = Command "land"}, Nothing] [f1, f2] = Connected (mixfixLoc parts [f1, f2]) Conjunction f1 f2
makeConnective parts@[Nothing, Just Located{unLocated = Command "lor"}, Nothing] [f1, f2] = Connected (mixfixLoc parts [f1, f2]) Disjunction f1 f2
makeConnective parts@[Nothing, Just Located{unLocated = Command "iff"}, Nothing] [f1, f2] = Connected (mixfixLoc parts [f1, f2]) Equivalence f1 f2
makeConnective parts@[Just Located{unLocated = Command "lnot"}, Nothing] [f1] = FormulaNeg (mixfixLoc parts [f1]) f1
makeConnective pat _ = error ("makeConnective does not handle the following connective correctly: " <> show pat)



type StructPhrase = LexicalItemSgPl

-- | For example 'an integer' would be
-- > Noun (unsafeReadPhrase "integer[/s]") []
type Noun = NounOf Term
data NounOf a
    = Noun Location LexicalItemSgPl [a]
    deriving (Show, Eq, Ord)

instance Locatable (NounOf a) where
    locate (Noun l _ _) = l




type NounPhrase t = NounPhraseOf t Term
-- NOTE: 'NounPhraseOf' is only used with arguments of type 'Term',
-- but keeping the argument parameter @a@ allows the 'Show' and 'Eq'
-- instances to remain decidable.
data NounPhraseOf t a
    = NounPhrase [AdjLOf a] (NounOf a) (t VarSymbol) [AdjROf a] (Maybe Stmt)

instance (Show a, Show (t VarSymbol)) => Show (NounPhraseOf t a) where
    show (NounPhrase ls n vs rs ms) =
        "NounPhrase ("
            <> show ls <> ") ("
            <> show n <> ") ("
            <> show vs <> ") ("
            <> show rs <> ") ("
            <> show ms <> ")"

instance (Eq a, Eq (t VarSymbol)) => Eq (NounPhraseOf t a) where
    NounPhrase ls n vs rs ms == NounPhrase ls' n' vs' rs' ms' =
        ls == ls' && n == n' && vs == vs' && rs == rs' && ms == ms'

-- This is arbitrary and ugly, but it's useful to have a somewhat
-- usable Ord instance for all raw syntax (e.g. for 'nubOrd').
instance (Ord a, Ord (t VarSymbol)) => Ord (NounPhraseOf t a) where
    NounPhrase ls n vs rs ms `compare` NounPhrase ls' n' vs' rs' ms' =
        case compare n n' of
            GT -> case compare ls ls' of
                GT -> case compare rs rs' of
                    GT -> case compare ms ms' of
                        GT -> compare vs vs'
                        ordering -> ordering
                    ordering -> ordering
                ordering -> ordering
            ordering -> ordering

-- | @Nameless a@ is quivalent to @Const () a@ (from "Data.Functor.Const").
-- It describes a container that is unwilling to actually contain something.
-- @Nameless@ lets us treat nouns with no names, one name, or many names uniformly.
-- Thus @NounPhraseOf Nameless a@ is a noun phrase without a name and with arguments
-- of type @a@.
data Nameless a = Nameless deriving (Show, Eq, Ord)


-- | Left adjectives modify nouns from the left side,
-- e.g. /@even@/, /@continuous@/, and /@σ-finite@/.
type AdjL = AdjLOf Term
data AdjLOf a
    = AdjL Location LexicalItem [a]
    deriving (Show, Eq, Ord)

instance Locatable (AdjLOf a) where
    locate (AdjL l _ _) = l


-- | Right attributes consist of basic right adjectives, e.g.
-- /@divisible by ?@/, or /@of finite type@/ and verb phrases
-- marked with /@that@/, such as /@integer that divides n@/.
-- In some cases these right attributes may be followed
-- by an additional such-that phrase.
type AdjR = AdjROf Term
data AdjROf a
    = AdjR Location LexicalItem [a]
    | AttrRThat VerbPhrase
    deriving (Show, Eq, Ord)

instance Locatable (AdjROf a) where
    locate (AdjR l _ _) = l
    locate (AttrRThat vp) = locate vp

-- | Adjectives for parts of the AST where adjectives are not used
-- to modify nouns and the L/R distinction does not matter, such as
-- when then are used together with a copula (like /@n is even@/).
type Adj = AdjOf Term
data AdjOf a
    = Adj Location LexicalItem [a]
    deriving (Show, Eq, Ord)

instance Locatable (AdjOf a) where
    locate (Adj l _ _) = l


type Verb = VerbOf Term
data VerbOf a
    = Verb Location LexicalItemSgPl [a]
    deriving (Show, Eq, Ord)

instance Locatable (VerbOf a) where
    locate (Verb l _ _) = l


type Fun = FunOf Term
data FunOf a
    = Fun {loc :: Location, phrase :: LexicalItemSgPl, funArgs :: [a]}
    deriving (Show, Eq, Ord)

instance Locatable (FunOf a) where
    locate = (.loc)


type VerbPhrase = VerbPhraseOf Term
data VerbPhraseOf a
    = VPVerb (VerbOf a)
    | VPAdj (NonEmpty (AdjOf a)) -- ^ @x is foo@ / @x is foo and bar@
    | VPVerbNot (VerbOf a)
    | VPAdjNot (NonEmpty (AdjOf a)) -- ^ @x is not foo@ / @x is neither foo nor bar@
    deriving (Show, Eq, Ord)

instance Locatable (VerbPhraseOf a) where
    locate = \case
        VPVerb v -> locate v
        VPAdj adjs -> locate adjs
        VPVerbNot v -> locate v
        VPAdjNot adjs -> locate adjs


data Quantifier
    = Universally
    | Existentially
    | Nonexistentially
    deriving (Show, Eq, Ord)

data QuantPhrase = QuantPhrase Quantifier (NounPhrase []) deriving (Show, Eq, Ord)


data Term
    = TermExpr Expr
    -- ^ A symbolic expression.
    | TermFun Fun
    -- ^ Definite noun phrase, e.g. /@the derivative of $f$@/.
    | TermIota Location VarSymbol Stmt
    -- ^ Definite descriptor, e.g. /@an $x$ such that ...@//
    | TermQuantified Quantifier Location (NounPhrase Maybe)
    -- ^ Indefinite quantified notion, e.g. /@every even integer that divides $k$ ...@/.
    deriving (Show, Eq, Ord)

instance Locatable Term where
    locate :: Term -> Location
    locate (TermExpr e) = locate e
    locate (TermFun f) = f.loc
    locate (TermIota l _ _) = l
    locate (TermQuantified _ l _) = l


data Stmt
    = StmtFormula {formula :: Formula} -- ^ E.g.: /@We have \<Formula\>@/.
    | StmtVerbPhrase {args :: NonEmpty Term, verb :: VerbPhrase} -- ^ E.g.: /@\<Term\> and \<Term\> \<verb\>@/.
    | StmtNoun {args :: NonEmpty Term, noun :: (NounPhrase Maybe)} -- ^ E.g.: /@\<Term\> is a(n) \<NP\>@/.
    | StmtStruct {arg :: Term, struct :: StructPhrase}
    | StmtNeg {loc :: Location, stmt :: Stmt} -- ^ E.g.: /@It is not the case that \<Stmt\>@/.
    | StmtExists {loc :: Location, np :: NounPhrase []} -- ^ E.g.: /@There exists a(n) \<NP\>@/.
    | StmtConnected {conn :: Connective, mloc :: Maybe Location, stmt1 :: Stmt, stmt2 :: Stmt}
    | StmtQuantPhrase {loc :: Location, qp :: QuantPhrase, stmt :: Stmt}
    | SymbolicQuantified {loc :: Location, quant :: Quantifier, vars :: NonEmpty VarSymbol, b :: Bound, suchThat :: Maybe Stmt, stmt :: Stmt}
    deriving (Show, Eq, Ord)

instance Locatable Stmt where
    locate :: Stmt -> Location
    locate StmtFormula{formula = phi} = locate phi
    locate StmtConnected{mloc = Just p} = p
    locate StmtConnected{mloc = Nothing, stmt1 = s} = locate s
    locate StmtVerbPhrase{args = a :| _} = locate a
    locate StmtNoun{args = a :| _} = locate a
    locate StmtStruct{arg = a} = locate a
    locate StmtNeg{loc = p} = p
    locate StmtExists{loc = p} = p
    locate StmtQuantPhrase{loc = p} = p
    locate SymbolicQuantified{loc = p} = p

data Bound = Unbounded | Bounded Location Sign Relation Expr deriving (Show, Eq, Ord)

instance Locatable Bound where
    locate = \case
        Unbounded -> Nowhere
        Bounded l _ _ _ -> l

pattern SymbolicForall :: Location -> NonEmpty VarSymbol -> Bound -> Maybe Stmt -> Stmt -> Stmt
pattern SymbolicForall loc vs bound suchThat have = SymbolicQuantified loc Universally vs bound suchThat have

pattern SymbolicExists :: Location -> NonEmpty VarSymbol -> Bound -> Stmt -> Stmt
pattern SymbolicExists loc vs bound suchThat = SymbolicQuantified loc Existentially vs bound Nothing suchThat

makeSymbolicNotExists :: Location -> NonEmpty VarSymbol -> Bound -> Stmt -> Stmt
makeSymbolicNotExists p vs bound st = StmtNeg p (SymbolicExists p vs bound st)

data Asm
    = AsmSuppose Stmt
    | AsmLetNoun (NonEmpty VarSymbol) (NounPhrase Maybe) -- ^ E.g.: /@let k be an integer@/
    | AsmLetIn (NonEmpty VarSymbol) Expr -- ^ E.g.: /@let $k\in\integers$@/
    | AsmLetThe VarSymbol Fun -- ^ E.g.: /@let $g$ be the derivative of $f$@/
    | AsmLetEq VarSymbol Expr -- ^ E.g.: /@let $m = n + k$@/
    | AsmLetStruct VarSymbol StructPhrase -- ^ E.g.: /@let $A$ be a monoid@/
    deriving (Show, Eq, Ord)

data Axiom = Axiom [Asm] Stmt
    deriving (Show, Eq, Ord)

data Claim = Claim [Asm] Stmt
    deriving (Show, Eq, Ord)

-- | The head of the definition describes the part before the /@iff@/,
-- i.e. the definiendum. An optional noun-phrase corresponds to an optional
-- type annotation for the 'Term' of the head. The last part of the head
-- is the lexical phrase that is defined.
--
-- > "A natural number   $n$        divides $m$   iff   ..."
-- >  ^^^^^^^^^^^^^^^^   ^^^        ^^^^^^^^^^^         ^^^
-- >  type annotation    variable   verb                definiens
-- >  (a noun phrase)               (all args are vars) (a statement)
--
data DefnHead
    = DefnAdj (Maybe (NounPhrase Maybe)) VarSymbol (AdjOf VarSymbol)
    | DefnVerb (Maybe (NounPhrase Maybe)) VarSymbol (VerbOf VarSymbol)
    | DefnNoun VarSymbol (NounOf VarSymbol)
    | DefnSymbolicPredicate PrefixPredicate Marker (NonEmpty VarSymbol)
    | DefnRel VarSymbol RelationSymbol [VarSymbol] VarSymbol
    -- ^ E.g.: /@$x \subseteq y$ iff [...@/
    deriving (Show, Eq, Ord)

data Defn
    = Defn [Asm] DefnHead Stmt
    | DefnFun [Asm] (FunOf VarSymbol) (Maybe Term) Term
    -- ^ A 'DefnFun' consists of the functional noun (which must start with /@the@/)
    -- and an optional specification of a symbolic equivalent. The symbolic equivalent
    -- does not need to have the same variables as the full functional noun pattern.
    --
    -- > "The tensor product of $U$ and $V$ over $K$, $U\tensor V$, is ..."
    -- >  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^     ^^^
    -- >  definiendum                                 symbolic eqv.    definiens
    -- >  (a functional noun)                         (an exression)   (a term)
    --
    | DefnOp SymbolPattern Expr
    deriving (Show, Eq, Ord)

data CalcQuantifier
    = CalcQuantifier (NonEmpty VarSymbol) Bound (Maybe Stmt)
    deriving (Show, Eq, Ord)

data Proof
    = Omitted
    | Qed (Maybe Location) Justification
    -- ^ Ends of a proof, leaving automation to discharge the current goal using the given justification.
    | ByCase Location [Case]
    | ByContradiction Location Proof
    | BySetInduction Location (Maybe Term) Proof
    -- ^ ∈-induction.
    | ByOrdInduction Location Proof
    -- ^ Transfinite induction for ordinals.
    | Assume Location Stmt Proof
    | FixSymbolic Location (NonEmpty VarSymbol) Bound Proof
    | FixSuchThat Location (NonEmpty VarSymbol) Stmt Proof
    | Calc Location (Maybe CalcQuantifier) Calc Proof
    -- ^ Simplify goals that are implications or disjunctions.
    | TakeVar Location (NonEmpty VarSymbol) Bound Stmt Justification Proof
    | TakeNoun Location (NounPhrase []) Justification Proof
    | Have Location (Maybe Stmt) Stmt Justification Proof
    -- ^ /@Since \<stmt\>, we have \<stmt\> by \<ref\>.@/
    | Suffices Location Stmt Justification Proof
    -- ^ /@It suffices to show that [...]. [...]@/
    | Subclaim Location Stmt Proof Proof
    -- ^ A claim is a sublemma with its own proof:
    --  /@Show \<goal stmt\>. \<steps\>. \<continue other proof\>.@/
    | Define Location VarSymbol Expr Proof
    -- ^ Local definition.
    --
    | DefineFunction Location VarSymbol VarSymbol Expr VarSymbol Expr Proof
    -- ^ Local function definition, e.g. /@Let $f(x) = e$ for $x\\in d$@/.
    -- The first 'VarSymbol' is the newly defined symbol, the second one is the argument.
    -- The first 'Expr' is the value, the final variable and expr specify a bound (the domain of the function).




    | DefineFunctionLocal Location VarSymbol VarSymbol Expr VarSymbol VarSymbol (NonEmpty (Expr, Formula)) Proof
    -- ^ Local function definition, but in this case we give the domain and target an the rules for $xs$ in some sub domains.
    --
    deriving (Show, Eq, Ord)

-- | An inline justification.
data Justification
    = JustificationRef (NonEmpty Marker)
    | JustificationSetExt
    | JustificationEmpty
    | JustificationLocal -- ^ Use only local assumptions
    deriving (Show, Eq, Ord)


-- | A case of a case split.
data Case = Case
    { caseOf :: Stmt
    , caseProof :: Proof
    } deriving (Show, Eq, Ord)

data Calc
    = Equation Expr (NonEmpty (Expr, Justification))
    -- ^ A chain of equalities. Each claimed equality has a (potentially empty) justification.
    -- For example: @a &= b \\explanation{by \\cref{a_eq_b}} &= c@
    -- would be (modulo expr constructors)
    -- @Equation "a" [("b", JustificationRef "a_eq_b"), ("c", JustificationEmpty)]@.
    | Biconditionals Formula (NonEmpty (Formula, Justification))
    deriving (Show, Eq, Ord)


data Abbreviation
    = AbbreviationAdj VarSymbol (AdjOf VarSymbol) Stmt
    | AbbreviationVerb VarSymbol (VerbOf VarSymbol) Stmt
    | AbbreviationNoun VarSymbol (NounOf VarSymbol) Stmt
    | AbbreviationRel VarSymbol RelationSymbol [VarSymbol] VarSymbol Stmt
    | AbbreviationFun (FunOf VarSymbol) Term
    | AbbreviationEq SymbolPattern Expr
    deriving (Show, Eq, Ord)

data Datatype
    = Datatype
        { datatypeHeadExpr :: Expr
        , datatypeClauses :: NonEmpty DatatypeClause
        }
    deriving (Show, Eq, Ord)

data DatatypeClause = DatatypeClause
    { datatypeClauseConstructorExpr :: Expr
    , datatypeClauseTargetExpr :: Expr
    , datatypeClausePremises :: [(VarSymbol, Expr)]
    }
    deriving (Show, Eq, Ord)

data Inductive = Inductive
    { inductiveSymbolPattern :: SymbolPattern
    , inductiveDomain :: Expr
    , inductiveIntros :: NonEmpty IntroRule
    }
    deriving (Show, Eq, Ord)

data IntroRule = IntroRule
    { introConditions :: [Formula] -- The inductively defined set may only appear as an argument of monotone operations on the rhs.
    , introResult :: Formula -- TODO Refine.
    }
    deriving (Show, Eq, Ord)


data SymbolPattern = SymbolPattern FunctionSymbol [VarSymbol]
    deriving (Show, Eq, Ord)

data Signature
    = SignatureAdj  VarSymbol (AdjOf  VarSymbol)
    | SignatureVerb VarSymbol (VerbOf VarSymbol)
    | SignatureNoun VarSymbol (NounOf VarSymbol)
    | SignatureSymbolic SymbolPattern (NounPhrase Maybe)
    -- ^ /@$\<symbol\>(\<vars\>)$ is a \<noun\>@/
    deriving (Show, Eq, Ord)


data StructDefn = StructDefn
    { structPhrase :: StructPhrase
    -- ^ E.g.: @partial order@ or @abelian group@.\
    , structParents :: [StructPhrase]
    -- ^ Structural parents
    , structLabel :: VarSymbol
    , structFixes :: [StructSymbol]
    -- ^ List of text for commands representing constants not inherited from its parents,
    -- e.g.: @\sqsubseteq@ or @\inv@.
    , structAssumes :: [(Marker, Stmt)]
    }
    deriving (Show, Eq, Ord)

newtype Marker = Marker Text deriving (Show, Eq, Ord, Generic)

deriving newtype instance Hashable Marker

instance IsString Marker where
    fromString str = Marker (Text.pack str)

type BlockTitle = [Token]

data ClaimKind
    = Proposition
    | Theorem
    | Lemma
    | Corollary
    | PlainClaim
    deriving (Show, Eq, Ord)

data Block
    = BlockAxiom Location (Maybe BlockTitle) Marker Axiom
    | BlockClaim ClaimKind Location (Maybe BlockTitle) Marker Claim
    | BlockProof Location Proof Location -- ^ Proof start and ending location.
    | BlockDefn Location (Maybe BlockTitle) Marker Defn
    | BlockAbbr Location (Maybe BlockTitle) Marker Abbreviation
    | BlockData Location (Maybe BlockTitle) Marker Datatype
    | BlockInductive Location (Maybe BlockTitle) Marker Inductive
    | BlockSig Location (Maybe BlockTitle) Marker [Asm] Signature
    | BlockStruct Location (Maybe BlockTitle) Marker StructDefn
    deriving (Show, Eq, Ord)
