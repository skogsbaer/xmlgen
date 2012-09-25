{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, BangPatterns,
             UndecidableInstances, OverlappingInstances, CPP #-}
-- | This module provides combinators for generating XML documents.
--
-- As an example, suppose you want to generate the following XML document:
--
-- > <?xml version="1.0"?>
-- > <people>
-- >   <person age="32">Stefan</person>
-- >   <person age="4">Judith</person>
-- > </people>
--
-- Then you could use the following Haskell code:
--
--
-- @
-- let people = [(\"Stefan\", \"32\"), (\"Judith\", \"4\")]
-- in 'doc' 'defaultDocInfo' $
--      'xelem' \"people\" $
--        'xelems' $ map (\(name, age) -> 'xelem' \"person\" ('xattr' \"age\" age '<#>' 'xtext' name)) people
-- @

module Text.XML.Generator (

  -- * General
    Xml
  -- * Documents
  , Doc, DocInfo(..), doc, defaultDocInfo
  -- * Namespaces
  , Namespace, Prefix, Uri
  , namespace, noNamespace, defaultNamespace
  -- * Elements
  , Elem, xelem, xelemQ, xelemEmpty, xelemQEmpty, AddChildren
  , xelems, noElems, xelemWithText, (<>), (<#>)
  -- * Attributes
  , Attr, xattr, xattrRaw, xattrQ, xattrQRaw
  , xattrs, noAttrs
  -- * Text
  , RawTextContent, TextContent
  , xtext, xtextRaw, xentityRef
  -- * Other
  , xempty , Misc(xprocessingInstruction, xcomment)
  -- * Rendering
  , xrender
  , XmlOutput(fromBuilder), Renderable
  -- * XHTML documents
  , xhtmlFramesetDocInfo, xhtmlStrictDocInfo, xhtmlTransitionalDocInfo
  , xhtmlRootElem

) where

import Prelude hiding (elem)
import Control.Monad.Reader (Reader(..), ask, asks, runReader)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid hiding (mconcat)
import qualified Data.Monoid as M

import Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder as Blaze
import Blaze.ByteString.Builder.Char.Utf8

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Char (isPrint, ord)
import qualified Data.String as S

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

#ifdef MIN_VERSION_base

#if MIN_VERSION_base(4,5,0)
#define BASE_AT_LEAST_4_5_0_0
#endif

#else

-- Fallback for ghci
#if __GLASGOW_HASKELL__ >= 704
#define BASE_AT_LEAST_4_5_0_0
#endif

#endif

--
-- Basic definitions
--

-- | A piece of XML at the element level.
newtype Elem = Elem { unElem :: Builder }

-- | A piece of XML at the attribute level.
newtype Attr = Attr { unAttr :: Builder }

-- | A piece of XML at the document level.
newtype Doc = Doc { unDoc :: Builder }

-- | Namespace prefix.
type Prefix = String

-- | Namespace URI.
type Uri = String -- must not be empty

-- | Type for representing presence or absence of an XML namespace.
data Namespace
    = NoNamespace
    | DefaultNamespace
    | QualifiedNamespace Prefix Uri
    deriving (Show, Eq)

-- | Constructs a qualified XML namespace.
--   The given URI must not be the empty string.
namespace :: Prefix -> Uri -> Namespace
namespace p u = if null u
            then error "Text.XML.Generator.ns: namespace URI must not be empty"
            else QualifiedNamespace p u

-- | A 'Namespace' value denoting the absence of any XML namespace information.
noNamespace :: Namespace
noNamespace = NoNamespace

-- | A 'Namespace' value denoting the default namespace.
--
-- * For elements, this is the namespace currently mapped to the empty prefix.
--
-- * For attributes, the default namespace does not carry any namespace information.
defaultNamespace :: Namespace
defaultNamespace = DefaultNamespace

data NsEnv = NsEnv { ne_namespaceMap :: Map.Map Prefix Uri
                   , ne_noNamespaceInUse :: Bool }

emptyNsEnv :: NsEnv
emptyNsEnv = NsEnv Map.empty False

-- | The type @Xml t@ represent a piece of XML of type @t@, where @t@
--   is usually one of 'Elem', 'Attr', or 'Doc'.
newtype Xml t = Xml { unXml :: Reader NsEnv (t, NsEnv) }

runXml :: NsEnv -> Xml t -> (t, NsEnv)
runXml nsEnv (Xml x) = runReader x nsEnv

-- | An empty, polymorphic piece of XML.
xempty :: Renderable t => Xml t
xempty = Xml $
    do env <- ask
       return (mkRenderable mempty, env)

--
-- Document
--

-- | The 'DocInfo' type contains all information of an XML document except the root element.
data DocInfo
    = DocInfo
      { docInfo_standalone :: Bool          -- ^ Value of the @standalone@ attribute in the @\<?xml ... ?\>@ header
      , docInfo_docType    :: Maybe String  -- ^ Document type (N.B.: rendering does not escape this value)
      , docInfo_preMisc    :: Xml Doc       -- ^ Content before the root element
      , docInfo_postMisc   :: Xml Doc       -- ^ Content after the root element
      }

-- | The default document info (standalone, without document type, without content before/after the root element).
defaultDocInfo :: DocInfo
defaultDocInfo = DocInfo { docInfo_standalone = True
                         , docInfo_docType    = Nothing
                         , docInfo_preMisc    = xempty
                         , docInfo_postMisc   = xempty }

-- | Constructs an XML document from a 'DocInfo' value and the root element.
doc :: DocInfo -> Xml Elem -> Xml Doc
doc di rootElem = Xml $
    do let prologBuf = fromString "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"" <>
                       fromString (if standalone then "yes" else "no") <>
                       fromString "\"?>\n" <>
                       case mDocType of
                         Nothing -> mempty
                         Just s -> fromString s <> fromString "\n"
       env <- ask
       let Doc preBuf = fst $ runXml env preMisc
           Elem elemBuf = fst $ runXml env rootElem
           Doc postBuf = fst $ runXml env postMisc
       return $ (Doc $ prologBuf `mappend` preBuf `mappend` elemBuf `mappend` postBuf, env)
    where
       standalone = docInfo_standalone di
       mDocType = docInfo_docType di
       preMisc = docInfo_preMisc di
       postMisc = docInfo_postMisc di

--
-- Names
--

class Name n where
    nameBuilder :: n -> Builder

instance Name String where
    nameBuilder = fromString

instance Name T.Text where
    nameBuilder = fromText

instance Name TL.Text where
    nameBuilder = fromLazyText

--
-- Text content
--

-- | Construction of text content not subject to escaping.
class RawTextContent t where
    rawTextBuilder :: t -> Builder

-- | Construction of text content subject to escaping.
class RawTextContent t => TextContent t where
    escape :: t -> t
    textBuilder :: TextContent t => t -> Builder
    textBuilder = rawTextBuilder . escape

instance RawTextContent String where
    rawTextBuilder = fromString

instance TextContent String where
    escape = genericEscape foldr showString showChar

instance RawTextContent T.Text where
    rawTextBuilder = fromText

instance TextContent T.Text where
    escape = genericEscape T.foldr T.append T.cons

instance RawTextContent TL.Text where
    rawTextBuilder = fromLazyText

instance TextContent TL.Text where
    escape = genericEscape TL.foldr TL.append TL.cons

instance RawTextContent BS.ByteString where
    rawTextBuilder = fromByteString

instance RawTextContent BSL.ByteString where
    rawTextBuilder = fromLazyByteString

--
-- Attributes
--

-- | Construct a simple-named attribute by escaping its value.
xattr :: (Name n, TextContent t) => n -> t -> Xml Attr
xattr = xattrQ DefaultNamespace

-- | Construct a simple-named attribute without escaping its value.
-- /Note:/ attribute values are quoted with double quotes.
xattrRaw :: (Name n, RawTextContent t) => n -> t -> Xml Attr
xattrRaw = xattrQRaw DefaultNamespace

-- | Construct an attribute by escaping its value.
xattrQ :: (Name n, TextContent t) => Namespace -> n -> t -> Xml Attr
xattrQ ns key value = xattrQRaw' ns (nameBuilder key) (textBuilder value)

-- | Construct an attribute without escaping its value.
-- /Note:/ attribute values are quoted with double quotes.
xattrQRaw :: (Name n, RawTextContent t) => Namespace -> n -> t -> Xml Attr
xattrQRaw ns key value = xattrQRaw' ns (nameBuilder key) (rawTextBuilder value)

xattrQRaw' :: Namespace -> Builder -> Builder -> Xml Attr
xattrQRaw' ns' key valueBuilder = Xml $
    do uriMap' <- ask
       let (mDecl, prefix, uriMap) = extendNsEnv True uriMap' ns'
           nsDeclBuilder =
               case mDecl of
                 Nothing -> mempty
                 Just (p, u) ->
                     let uriBuilder = fromString u
                         prefixBuilder =
                             if null p then mempty else colonBuilder `mappend` fromString p
                     in spaceBuilder `mappend` nsDeclStartBuilder
                        `mappend` prefixBuilder `mappend` startBuilder `mappend` uriBuilder
                        `mappend` endBuilder
           prefixBuilder =
               if null prefix
                  then spaceBuilder
                  else spaceBuilder `mappend` fromString prefix `mappend` colonBuilder
           builder = nsDeclBuilder `mappend` prefixBuilder `mappend`
                     key `mappend` startBuilder `mappend`
                     valueBuilder `mappend` endBuilder
       return $ (Attr builder, uriMap)
    where
      spaceBuilder = fromString " "
      startBuilder = fromString "=\""
      endBuilder = fromString "\""
      nsDeclStartBuilder = fromString "xmlns"
      colonBuilder = fromString ":"

-- |  Merge a list of attributes into a single piece of XML at the attribute level.
xattrs :: [Xml Attr] -> Xml Attr
xattrs = M.mconcat

-- | The empty attribute list.
noAttrs :: Xml Attr
noAttrs = xempty

instance Monoid (Xml Attr) where
    mempty = noAttrs
    mappend x1 x2 = Xml $
        do env <- ask
           let (Attr b1, env') = runXml env x1
           let (Attr b2, env'') = runXml env' x2
           return $ (Attr $ b1 `mappend` b2, env'')

--
-- Elements
--

-- | Class for adding children to an element.
--
-- The various instances of this class allow the addition of different kinds
-- of children.
class AddChildren c where
    addChildren :: c -> NsEnv -> Builder

instance AddChildren (Xml Attr) where
    addChildren attrs uriMap =
       let (Attr builder', _) = runXml uriMap attrs
       in builder' <> fromString "\n>"

instance AddChildren (Xml Elem) where
    addChildren elems uriMap =
       let (Elem builder', _) = runXml uriMap elems
       in fromString "\n>" `mappend` builder'

instance AddChildren (Xml Attr, Xml Elem) where
    addChildren (attrs, elems) uriMap =
        let (Attr builder, uriMap') = runXml uriMap attrs
            (Elem builder', _) = runXml uriMap' elems
        in builder `mappend` fromString "\n>" `mappend` builder'

instance TextContent t => AddChildren t where
    addChildren t _ = fromChar '>' <> textBuilder t

instance AddChildren () where
    addChildren _ _ = fromChar '>'

-- | Construct a simple-named element with the given children.
xelem :: (Name n, AddChildren c) => n -> c -> Xml Elem
xelem = xelemQ DefaultNamespace

-- | Construct a simple-named element without any children.
xelemEmpty :: Name n => n -> Xml Elem
xelemEmpty name = xelemQ DefaultNamespace name (mempty :: Xml Elem)

-- | Construct an element with the given children.
xelemQ :: (Name n, AddChildren c) => Namespace -> n -> c -> Xml Elem
xelemQ ns' name children = Xml $
    do oldUriMap <- ask
       let (mDecl, prefix,!uriMap) = oldUriMap `seq` extendNsEnv False oldUriMap ns'
       let elemNameBuilder =
               if null prefix
                  then nameBuilder name
                  else fromString prefix `mappend` fromString ":" `mappend` nameBuilder name
       let nsDeclBuilder =
               case mDecl of
                 Nothing -> mempty
                 Just (p, u) ->
                     let prefixBuilder =
                             if null p then mempty else fromChar ':' `mappend` fromString p
                     in fromString " xmlns" `mappend` prefixBuilder `mappend` fromString "=\""
                        `mappend` fromString u `mappend` fromString "\""
       let b1 = fromString "<"
       let b2 = b1 `mappend` elemNameBuilder `mappend` nsDeclBuilder
       let b3 = b2 `mappend` addChildren children uriMap
       let builderOut = Elem (b3 `mappend` fromString "</" `mappend` elemNameBuilder `mappend` fromString "\n>")
       return (builderOut, oldUriMap)

-- | Construct an element without any children.
xelemQEmpty :: Name n => Namespace -> n -> Xml Elem
xelemQEmpty ns name = xelemQ ns name (mempty :: Xml Elem)

-- |  Merges a list of elements into a single piece of XML at the element level.
xelems :: [Xml Elem] -> Xml Elem
xelems = M.mconcat

-- | No elements at all.
noElems :: Xml Elem
noElems = xempty

-- | The expression @xelemWithText n t@ constructs an XML element with name @n@ and text content @t@.
xelemWithText :: (TextContent t) => String -> t -> Xml Elem
xelemWithText n t = xelem n (xtext t)

instance Monoid (Xml Elem) where
    mempty = noElems
    mappend x1 x2 = Xml $
        do env <- ask
           let (Elem b1, env') = runXml env x1
               (Elem b2, env'') = runXml env' x2
           return (Elem $ b1 `mappend` b2, env'')

--
-- Other XML constructs
--

-- | Constructs a text node by escaping the given argument.
xtext :: TextContent t => t -> Xml Elem
xtext content = Xml $
    do env <- ask
       return (Elem $ textBuilder content, env)

-- | Constructs a text node /without/ escaping the given argument.
xtextRaw :: RawTextContent t => t -> Xml Elem
xtextRaw content = Xml $
    do env <- ask
       return (Elem $ rawTextBuilder content, env)

-- | Constructs a reference to the named entity.
-- /Note:/ no escaping is performed on the name of the entity
xentityRef :: String -> Xml Elem
xentityRef name = Xml $
    do env <- ask
       return (Elem $ fromChar '&' <> fromString name <> fromChar ';', env)

-- | Class providing methods for adding processing instructions and comments.
class Renderable t => Misc t where
    -- | Constructs a processing instruction with the given target and content.
    -- /Note:/ Rendering does not perform escaping on the target and the content.
    xprocessingInstruction :: String -> String -> Xml t
    xprocessingInstruction target content = Xml $
        do env <- ask
           return (mkRenderable $
                   fromString "<?" <>
                   fromString target <>
                   fromChar ' ' <>
                   fromString content <>
                   fromString "?>",
                   env)
    -- | Constructs an XML comment.
    -- /Note:/ No escaping is performed on the text of the comment.
    xcomment :: String -> Xml t
    xcomment content = Xml $
        do env <- ask
           return (mkRenderable $
                   fromString "<!--" <>
                   fromString content <>
                   fromString "-->",
                   env)

instance Misc Elem
instance Misc Doc

--
-- Operators
--

-- Note: (<>) is defined in Data.Monoid starting with base 4.5.0.0
#ifndef BASE_AT_LEAST_4_5_0_0
infixl 6 <>
-- | Shortcut for the 'mappend' functions of monoids. Used to concatenate elements, attributes
--   and text nodes.
(<>) :: Monoid t => t -> t -> t
(<>) = mappend
#endif

infixl 5 <#>
-- | Shortcut for constructing pairs. Used in combination with 'xelem' for separating child-attributes
--   from child-elements.
(<#>) :: a -> b -> (a, b)
(<#>) x y = (x, y)

--
-- Rendering
--

-- | Instances of the @XmlOutput@ class may serve as target of serializing an XML document.
class XmlOutput t where
    -- | Creates the target type from a 'Builder'.
    fromBuilder :: Builder -> t

instance XmlOutput Builder where
    fromBuilder b = b

instance XmlOutput BS.ByteString where
    fromBuilder = toByteString

instance XmlOutput BSL.ByteString where
    fromBuilder = toLazyByteString

-- | Any type subject to rendering must implement this type class.
class Renderable t where
    builder :: t -> Builder
    mkRenderable :: Builder -> t

instance Renderable Elem where
    builder (Elem b) = b
    mkRenderable = Elem

instance Renderable Attr where
    builder (Attr b) = b
    mkRenderable = Attr

instance Renderable Doc where
    builder (Doc b) = b
    mkRenderable = Doc

-- | Renders a given piece of XML.
xrender :: (Renderable r, XmlOutput t) => Xml r -> t
xrender r = fromBuilder $ builder r'
    where
      r' = fst $ runXml emptyNsEnv r

--
-- Utilities
--

extendNsEnv :: Bool -> NsEnv -> Namespace -> (Maybe (Prefix, Uri), Prefix, NsEnv)
extendNsEnv isAttr env ns =
    case ns of
      NoNamespace
          | isAttr -> (Nothing, "", env)
          | otherwise ->
              case Map.lookup "" (ne_namespaceMap env) of
                Nothing ->  -- empty prefix not in use
                  (Nothing, "", env { ne_noNamespaceInUse = True })
                Just uri -> -- empty prefix mapped to uri
                  (Just ("", ""), "", env { ne_namespaceMap = Map.delete "" (ne_namespaceMap env)
                                          , ne_noNamespaceInUse = True })
      DefaultNamespace ->
          (Nothing, "", env)
      QualifiedNamespace p' u ->
          let p = if null p' && (isAttr || ne_noNamespaceInUse env) then "_" else p'
              (mDecl, prefix, newMap) = genValidPrefix (ne_namespaceMap env) p u
          in (mDecl, prefix, env { ne_namespaceMap = newMap })
    where
      genValidPrefix map prefix uri =
        case Map.lookup prefix map of
          Nothing -> (Just (prefix, uri), prefix, Map.insert prefix uri map)
          Just foundUri ->
              if foundUri == uri
                 then (Nothing, prefix, map)
                 else genValidPrefix map ('_':prefix) uri

{-# SPECIALIZE INLINE genericEscape ::
     ((Char -> String -> String) -> String -> String -> String)
  -> (String -> String -> String)
  -> (Char -> String -> String)
  -> String
  -> String #-}
{-# SPECIALIZE INLINE genericEscape ::
     ((Char -> T.Text -> T.Text) -> T.Text -> T.Text -> T.Text)
  -> (T.Text -> T.Text -> T.Text)
  -> (Char -> T.Text -> T.Text)
  -> T.Text
  -> T.Text #-}
{-# SPECIALIZE INLINE genericEscape ::
     ((Char -> TL.Text -> TL.Text) -> TL.Text -> TL.Text -> TL.Text)
  -> (TL.Text -> TL.Text -> TL.Text)
  -> (Char -> TL.Text -> TL.Text)
  -> TL.Text
  -> TL.Text #-}
genericEscape :: (S.IsString s)
              => ((Char -> s -> s) -> s -> s -> s)
              -> (s -> s -> s)
              -> (Char -> s -> s)
              -> s
              -> s
genericEscape foldr showString' showChar x = foldr escChar (S.fromString "") x
    where
      -- copied from xml-light
      escChar c = case c of
        '<'   -> showString "&lt;"
        '>'   -> showString "&gt;"
        '&'   -> showString "&amp;"
        '"'   -> showString "&quot;"
        -- we use &#39 instead of &apos; because IE apparently has difficulties
        -- rendering &apos; in xhtml.
        -- Reported by Rohan Drape <rohan.drape@gmail.com>.
        '\''  -> showString "&#39;"
        -- XXX: Is this really wortherd?
        -- We could deal with these issues when we convert characters to bytes.
        _ | (oc <= 0x7f && isPrint c) || c == '\n' || c == '\r' -> showChar c
          | otherwise -> showString "&#" . showString (show oc) . showChar ';'
            where oc = ord c
      showString = showString' . S.fromString

--
-- XHTML
--

-- | Document type for XHTML 1.0 strict.
xhtmlDoctypeStrict :: String
xhtmlDoctypeStrict =
    "<!DOCTYPE html\n" ++
    "    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n" ++
    "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

-- | Document info for XHTML 1.0 strict.
xhtmlStrictDocInfo :: DocInfo
xhtmlStrictDocInfo = defaultDocInfo { docInfo_docType = Just xhtmlDoctypeStrict }

-- | Document type for XHTML 1.0 transitional.
xhtmlDoctypeTransitional :: String
xhtmlDoctypeTransitional =
    "<!DOCTYPE html\n" ++
    "    PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n" ++
    "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"

-- | Document info for XHTML 1.0 transitional.
xhtmlTransitionalDocInfo :: DocInfo
xhtmlTransitionalDocInfo = defaultDocInfo { docInfo_docType = Just xhtmlDoctypeTransitional }

-- | Document type for XHTML 1.0 frameset.
xhtmlDoctypeFrameset :: String
xhtmlDoctypeFrameset =
    "<!DOCTYPE html\n" ++
    "    PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"\n" ++
    "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"

-- | Document info for XHTML 1.0 frameset.
xhtmlFramesetDocInfo :: DocInfo
xhtmlFramesetDocInfo = defaultDocInfo { docInfo_docType = Just xhtmlDoctypeFrameset }

-- | Constructs the root element of an XHTML document.
xhtmlRootElem :: String -> Xml Elem -> Xml Elem
xhtmlRootElem lang children =
    xelemQ (namespace "" "http://www.w3.org/1999/xhtml") "html"
           (xattr "xml:lang" lang <>
            xattr "lang" lang <#>
            children)
