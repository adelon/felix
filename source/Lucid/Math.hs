{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A small @lucid2@ DSL for the
-- [MathML Core specification](https://w3c.github.io/mathml-core/).
--
-- Naming follows the usual Lucid convention:
--
-- * element constructors end in @_@, for example 'math_' or 'mfrac_'
-- * attribute helpers also end in @_@, for example 'display_' or 'scriptlevel_'
--
-- The API mostly mirrors MathML attribute names directly. A few HTML helpers are
-- re-exported from "Lucid.Html5" because MathML Core uses them unchanged:
-- 'a_', 'href_', 'width_', 'height_', and 'rowspan_'.
--
-- The module aims to cover MathML Core as defined by the current W3C Editor's
-- Draft. Some legacy MathML attributes are also exposed when MathML Core keeps
-- them valid for compatibility, even if the Core spec does not define special
-- behavior for them.
--
-- Example:
--
-- @
-- example :: Html ()
-- example = math_ [display_ "block"] do
--     mfrac_ do
--         mn_ "1"
--         msup_ (mi_ "x" *> mn_ "2")
-- @
module Lucid.Math
    ( module Lucid.Math
    -- * Re-exports from "Lucid.Html5".
    , module Export
    ) where

import Data.Bool (Bool)
import Data.Text (Text)
import Control.Monad (Monad)

import Lucid.Base
import Lucid.Html5 as Export (a_, height_, href_, rowspan_, width_)


-- * The top-level @math@ MathML element

-- | Top-level element for MathML content. All mathematical content must be
-- wrapped by this element. There should not be a nested 'math_' within it.
math_ :: Term arg result => arg -> result
math_ = term "math"

-- | Value of the top-level @display@ attribute on 'math_'.
-- MathML Core uses @\"block\"@ and @\"inline\"@.
display_ :: Text -> Attributes
display_ = makeAttributes "display"

-- | Shortcut for @display_ "block"@.
-- Displays the formula outside the current span of text as a separate block.
displayblock_ :: Attributes
displayblock_ = display_ "block"

-- | Shortcut for @display_ "inline"@.
-- Displays the formula within the current span of text.
displayinline_ :: Attributes
displayinline_ = display_ "inline"

-- | Text alternative for assistive technologies on the top-level 'math_'.
alttext_ :: Text -> Attributes
alttext_ = makeAttributes "alttext"


-- * Global attributes


-- | Presentational hint for whether a formula should use display style.
-- In display style, layout generally uses larger operators and less compact
-- script placement than inline style.
displaystyle_ :: Bool -> Attributes
displaystyle_ value = makeAttributes "displaystyle" (if value then "true" else "false")

-- | Presentational hint for the @math-depth@ of an element. This accepts the
-- full MathML syntax, including absolute values such as @\"0\"@ and relative
-- values such as @\"+1\"@ or @\"-1\"@.
scriptlevel_ :: Text -> Attributes
scriptlevel_ = makeAttributes "scriptlevel"

-- | Math color hint, usually a CSS color value.
mathcolor_ :: Text -> Attributes
mathcolor_ = makeAttributes "mathcolor"

-- | Math background hint, usually a CSS color value.
mathbackground_ :: Text -> Attributes
mathbackground_ = makeAttributes "mathbackground"

-- | Math size hint.
-- Typical values are CSS-like lengths or percentages such as @\"1.2em\"@ or
-- @\"90%\"@.
mathsize_ :: Text -> Attributes
mathsize_ = makeAttributes "mathsize"

-- | Semantic intent annotation.
-- MathML Core keeps this attribute valid, but does not define rendering
-- behavior specific to it.
intent_ :: Text -> Attributes
intent_ = makeAttributes "intent"

-- | Semantic argument annotation.
-- MathML Core keeps this attribute valid, but does not define rendering
-- behavior specific to it.
arg_ :: Text -> Attributes
arg_ = makeAttributes "arg"


-- | Math variant hint.
-- In MathML Core, only @mathvariant="normal"@ on 'mi_' has specified behavior;
-- other values are legacy/full-MathML territory and may be ignored by Core
-- renderers. For stylistic alphabets, using the intended Unicode character is
-- usually the more robust choice.
mathvariant_ :: Text -> Attributes
mathvariant_ = makeAttributes "mathvariant"

-- * Basic MathML elements and attributes

-- | Group elements.
mrow_ :: Term arg result => arg -> result
mrow_ = term "mrow"

-- | Text embedded in MathML.
mtext_ :: Term arg result => arg -> result
mtext_ = term "mtext"

-- | Math identifier.
mi_ :: Term arg result => arg -> result
mi_ = term "mi"

-- | Math operators, including delimiters.
mo_ :: Term arg result => arg -> result
mo_ = term "mo"

-- | Math numbers.
mn_ :: Term arg result => arg -> result
mn_ = term "mn"

-- | String literals.
ms_ :: Term arg result => arg -> result
ms_ = term "ms"

-- | Blank space element with a size specified by its attributes. Takes
-- 'width_', 'height_', and 'depth_' attributes.
mspace_ :: Monad m => [Attributes] -> HtmlT m ()
mspace_ = makeElementNoEnd "mspace"

-- | Makes its content invisible. Can be used for alignment.
mphantom_ :: Term arg result => arg -> result
mphantom_ = term "mphantom"

-- | Style change container.
mstyle_ :: Term arg result => arg -> result
mstyle_ = term "mstyle"

-- | Error message box.
merror_ :: Term arg result => arg -> result
merror_ = term "merror"

-- | Adjust the size and position of its contents.
mpadded_ :: Term arg result => arg -> result
mpadded_ = term "mpadded"

-- | The @depth@ attribute. Useful in combination with 'mspace_' and 'mpadded_'.
-- See also 'Lucid.Html5.width_' and 'Lucid.Html5.height_'.
depth_ :: Text -> Attributes
depth_ = makeAttributes "depth"

-- | Horizontal offset. Useful in combination with 'mpadded_'.
lspace_ :: Text -> Attributes
lspace_ = makeAttributes "lspace"

-- | Vertical offset. Useful in combination with 'mpadded_'.
voffset_ :: Text -> Attributes
voffset_ = makeAttributes "voffset"


-- * Operator attributes

-- | Classification of an operator as prefix, infix, or postfix.
newtype OperatorForm = OperatorForm Text

pattern Prefix :: OperatorForm
pattern Prefix = OperatorForm "prefix"

pattern Infix :: OperatorForm
pattern Infix = OperatorForm "infix"

pattern Postfix :: OperatorForm
pattern Postfix = OperatorForm "postfix"

-- | The @form@ attribute for 'mo_'.
-- This overrides whether an operator should be treated as prefix, infix, or
-- postfix for spacing and dictionary lookup.
form_ :: OperatorForm -> Attributes
form_ (OperatorForm form) = makeAttributes "form" form

-- | Whether an operator should be treated as a fence character.
fence_ :: Bool -> Attributes
fence_ value = makeAttributes "fence" (if value then "true" else "false")

-- | Whether an operator should be treated as a separator.
separator_ :: Bool -> Attributes
separator_ value = makeAttributes "separator" (if value then "true" else "false")

-- | Whether an operator may stretch to match the size of surrounding content.
stretchy_ :: Bool -> Attributes
stretchy_ value = makeAttributes "stretchy" (if value then "true" else "false")

-- | Whether stretching should be symmetric around the math axis.
symmetric_ :: Bool -> Attributes
symmetric_ value = makeAttributes "symmetric" (if value then "true" else "false")

-- | Whether an operator should use large-operator styling in display style.
largeop_ :: Bool -> Attributes
largeop_ value = makeAttributes "largeop" (if value then "true" else "false")

-- | Whether limits may move beside the operator in more compact layouts.
movablelimits_ :: Bool -> Attributes
movablelimits_ value = makeAttributes "movablelimits" (if value then "true" else "false")

-- | Right operator spacing. Values use MathML/CSS length syntax.
rspace_ :: Text -> Attributes
rspace_ = makeAttributes "rspace"

-- | Maximum stretched size for stretchable operators.
maxsize_ :: Text -> Attributes
maxsize_ = makeAttributes "maxsize"

-- | Minimum stretched size for stretchable operators.
minsize_ :: Text -> Attributes
minsize_ = makeAttributes "minsize"


-- * Accents, subscripts, multiscripts, fractions, and roots

-- | Whether the overscript or underscript should be treated as an accent.
accent_ :: Bool -> Attributes
accent_ value = makeAttributes "accent" (if value then "true" else "false")

-- | Whether the underscript should be treated as an accent.
accentunder_ :: Bool -> Attributes
accentunder_ value = makeAttributes "accentunder" (if value then "true" else "false")

-- | Fraction.
mfrac_ :: Term arg result => arg -> result
mfrac_ = term "mfrac"

-- | The @linethickness@ attribute. Useful in combination with 'mfrac_'.
linethickness_ :: Text -> Attributes
linethickness_ = makeAttributes "linethickness"

-- | Square root. For other radicals see 'mroot_'.
msqrt_ :: Term arg result => arg -> result
msqrt_ = term "msqrt"

-- | Generalized radical with arbitrary indices.
-- Its second child is the index of the radical.
mroot_ :: Term arg result => arg -> result
mroot_ = term "mroot"

-- | Place accents or limits above an element.
mover_ :: Term arg result => arg -> result
mover_ = term "mover"

-- | Place accents or limits below an element.
munder_ :: Term arg result => arg -> result
munder_ = term "munder"

-- | Place accents or limits below and above an element.
munderover_ :: Term arg result => arg -> result
munderover_ = term "munderover"

-- | Subscript.
msub_ :: Term arg result => arg -> result
msub_ = term "msub"

-- | Superscript.
msup_ :: Term arg result => arg -> result
msup_ = term "msup"

-- | Subscript and superscript.
msubsup_ :: Term arg result => arg -> result
msubsup_ = term "msubsup"

-- | Presubscripts and tensor notations.
mmultiscripts_ :: Term arg result => arg -> result
mmultiscripts_ = term "mmultiscripts"

-- | Switch from postscripts to prescripts within 'mmultiscripts_'.
-- A typical structure is base, post-sub/superscripts, then 'mprescripts_',
-- then pre-sub/superscripts.
mprescripts_ :: Monad m => [Attributes] -> HtmlT m ()
mprescripts_ = makeElementNoEnd "mprescripts"

-- * Tables and matrices

-- | Table or raw matrix.
mtable_ :: Term arg result => arg -> result
mtable_ = term "mtable"

-- | Table or matrix row.
mtr_ :: Term arg result => arg -> result
mtr_ = term "mtr"

-- | Table or matrix entry.
mtd_ :: Term arg result => arg -> result
mtd_ = term "mtd"

-- | The @columnspan@ attribute. Useful in combination with 'mtd_'.
-- See also 'Lucid.Html5.rowspan_'.
-- Note that MathML does not use HTML5's @colspan@.
columnspan_ :: Text -> Attributes
columnspan_ = makeAttributes "columnspan"


-- * Linking and actions

-- | Interactive action container.
maction_ :: Term arg result => arg -> result
maction_ = term "maction"

-- | Legacy @actiontype@ attribute for 'maction_'.
-- MathML Core keeps it valid for compatibility, but does not define interactive
-- behavior for specific values.
actiontype_ :: Text -> Attributes
actiontype_ = makeAttributes "actiontype"

-- | Legacy @selection@ attribute for 'maction_'.
-- MathML Core keeps it valid for compatibility, but does not define interactive
-- behavior specific to it.
selection_ :: Text -> Attributes
selection_ = makeAttributes "selection"


-- * Semantics

-- | Associate annotations with a MathML expression.
-- This is typically used with 'annotation_' or 'annotationXml_' plus an
-- 'encoding_' attribute describing the annotation format.
semantics_ :: Term arg result => arg -> result
semantics_ = term "semantics"

-- | Text annotation child of 'semantics_'.
annotation_ :: Term arg result => arg -> result
annotation_ = term "annotation"

-- | XML annotation child of 'semantics_'.
annotationXml_ :: Term arg result => arg -> result
annotationXml_ = term "annotation-xml"

-- | Encoding label for 'annotation_' and 'annotationXml_'.
-- Typical values are MIME-like strings such as @\"application/x-tex\"@.
encoding_ :: Text -> Attributes
encoding_ = makeAttributes "encoding"
