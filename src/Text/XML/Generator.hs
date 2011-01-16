{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, TypeFamilies, MultiParamTypeClasses #-}
module Text.XML.Generator (

    Xml, Doc, DocInfo, Elem, Attr, Namespace, Prefix, Uri

  , XmlOutput, Renderable

  , ns, doc, defaultDocInfo
  , xattr, xattrRaw, xattrQ, xattrQRaw, xattrs
  , xelem, xelemEmpty, xelems, xelemQ
  , xtext, xtextRaw, xentityRef, xprocessingInstruction, xcomment, xempty
  , xrender

  , (<>), (<#>)

  , xhtmlFramesetDocInfo, xhtmlStrictDocInfo, xhtmlTransitionalDocInfo
  , xhtmlRootElem

) where

{-
TODO:

- benchmarks
- documentation

-}

import Prelude hiding (elem)
import Control.Monad.Reader (Reader(..), ask, asks, runReader)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid hiding (mconcat)

import Blaze.ByteString.Builder hiding (empty, append)
import qualified Blaze.ByteString.Builder as Blaze
import Blaze.ByteString.Builder.Char.Utf8

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Char (isPrint, ord)
import qualified Data.String as S

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
--
-- Basic definitions
--

newtype Elem = Elem { unElem :: Builder }
newtype Attr = Attr { unAttr :: Builder }
newtype Doc = Doc { unDoc :: Builder }

type Prefix = String
type Uri = String

data Namespace
    = DefaultNamespace
    | QualifiedNamespace Prefix Uri
    deriving (Show, Eq)

ns :: Prefix -> Uri -> Namespace
ns = QualifiedNamespace

type NsEnv = Map.Map Prefix Uri

emptyNsEnv :: NsEnv
emptyNsEnv = Map.empty

newtype Xml t = Xml { unXml :: t -> Reader NsEnv (t, NsEnv) }

runXml :: NsEnv -> Xml t -> t -> (t, NsEnv)
runXml nsEnv (Xml f) t = runReader (f t) nsEnv

xempty :: Xml t
xempty = Xml $ \t->
    do env <- ask
       return (t, env)

--
-- Document
--

data DocInfo
    = DocInfo
      { docInfo_standalone :: Bool
      , docInfo_docType    :: Maybe String
      , docInfo_preMisc    :: Xml Doc
      , docInfo_postMisc   :: Xml Doc }

defaultDocInfo :: DocInfo
defaultDocInfo = DocInfo { docInfo_standalone = True
                         , docInfo_docType    = Nothing -- no escaping performed
                         , docInfo_preMisc    = xempty
                         , docInfo_postMisc   = xempty }

doc :: DocInfo -> Xml Elem -> Xml Doc
doc di rootElem = Xml $ \(Doc buffer) ->
    do let prologBuf = fromString "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"" <>
                       fromString (if standalone then "yes" else "no") <>
                       fromString "\"?>\n" <>
                       case mDocType of
                         Nothing -> mempty
                         Just s -> fromString s <> fromString "\n"
       env <- ask
       let Doc preBuf = fst $ runXml env preMisc (Doc prologBuf)
           Elem elemBuf = fst $ runXml env rootElem (Elem preBuf)
           postBuf = fst $ runXml env postMisc (Doc elemBuf)
       return $ (postBuf, env)
    where
       standalone = docInfo_standalone di
       mDocType = docInfo_docType di
       preMisc = docInfo_preMisc di
       postMisc = docInfo_postMisc di

--
-- Text content
--

class RawTextContent t where
    rawTextBuilder :: t -> Builder

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

-- Note: attributes are quoted with "

class MkAttr n t where
    type MkAttrRes n t
    xattr :: TextContent t => n -> MkAttrRes n t
    xattrRaw :: RawTextContent t => n -> MkAttrRes n t

instance MkAttr String t where
    type MkAttrRes String t = t -> Xml Attr
    xattr = xattrQ DefaultNamespace
    xattrRaw = xattrQRaw DefaultNamespace

instance MkAttr Namespace t where
    type MkAttrRes Namespace t = String -> t -> Xml Attr
    xattr = xattrQ
    xattrRaw = xattrQRaw

-- value is escaped
xattrQ :: TextContent t => Namespace -> String -> t -> Xml Attr
xattrQ ns key value = xattrQRaw' ns key (textBuilder value)

-- value is NOT escaped
xattrQRaw :: RawTextContent t => Namespace -> String -> t -> Xml Attr
xattrQRaw ns key value = xattrQRaw' ns key (rawTextBuilder value)

xattrQRaw' :: Namespace -> String -> Builder -> Xml Attr
xattrQRaw' ns' key valueBuilder = Xml $ \(Attr buffer) ->
    do uriMap' <- ask
       let (ns, uriMap, newNs) = genValidNsForDesiredPrefix uriMap' ns'
           builder = case ns of
                       DefaultNamespace -> spaceBuilder `mappend` keyBuilder `mappend` startBuilder
                                           `mappend` valueBuilder `mappend` endBuilder
                       QualifiedNamespace p u ->
                           let uriBuilder = fromString u
                               prefixBuilder = fromString p
                           in if newNs
                                 then spaceBuilder `mappend` nsDeclStartBuilder `mappend` colonBuilder
                                      `mappend` prefixBuilder `mappend` startBuilder `mappend` uriBuilder
                                      `mappend` endBuilder `mappend` spaceBuilder `mappend` prefixBuilder
                                      `mappend` colonBuilder `mappend` keyBuilder `mappend` startBuilder
                                      `mappend` valueBuilder `mappend` endBuilder
                                  else spaceBuilder `mappend` prefixBuilder `mappend` colonBuilder
                                       `mappend` keyBuilder `mappend` startBuilder
                                       `mappend` valueBuilder `mappend` endBuilder
       return $ (Attr (buffer `mappend` builder), uriMap)
    where
      spaceBuilder = fromString " "
      keyBuilder = fromString key
      startBuilder = fromString "=\""
      endBuilder = fromString "\""
      nsDeclStartBuilder = fromString "xmlns"
      colonBuilder = fromString ":"

xattrs :: [Xml Attr] -> Xml Attr
xattrs = foldl mappend noAttrs

noAttrs :: Xml Attr
noAttrs = xempty

instance Monoid (Xml Attr) where
    mempty = noAttrs
    mappend x1 x2 = Xml $ \t ->
        do nsEnv <- ask
           let (t2, nsEnv') = runXml nsEnv x1 t
           return $ runXml nsEnv' x2 t2

--
-- Elements
--

class AddChildren c where
    addChildren :: c -> Builder -> NsEnv -> Builder

instance AddChildren (Xml Attr) where
    addChildren attrs builder uriMap =
       let (Attr builder', _) = runXml uriMap attrs (Attr builder)
       in builder' <> fromString "\n>"

instance AddChildren (Xml Elem) where
    addChildren elems builder uriMap =
       let (Elem builder', _) = runXml uriMap elems (Elem $ builder <> (fromString "\n>"))
       in builder'

instance AddChildren (Xml Attr, Xml Elem) where
    addChildren (attrs, elems) builder uriMap =
        let (Attr builder', uriMap') = runXml uriMap attrs (Attr builder)
            (Elem builder'', _) = runXml uriMap' elems (Elem $ builder' <> (fromString "\n>"))
        in builder''

class AddChildren c => MkElem n c where
    type MkElemRes n c
    xelem :: n -> MkElemRes n c

instance AddChildren c => MkElem String c where
    type MkElemRes String c = c -> Xml Elem
    xelem = xelemQ DefaultNamespace

instance AddChildren c => MkElem Namespace c where
    type MkElemRes Namespace c = String -> c -> Xml Elem
    xelem = xelemQ

class MkEmptyElem n where
    type MkEmptyElemRes n
    xelemEmpty :: n -> MkEmptyElemRes n

instance MkEmptyElem String where
    type MkEmptyElemRes String = Xml Elem
    xelemEmpty name = xelemQ DefaultNamespace name (mempty :: Xml Elem)

instance MkEmptyElem Namespace where
    type MkEmptyElemRes Namespace = String -> Xml Elem
    xelemEmpty ns name = xelemQ ns name (mempty :: Xml Elem)

xelemQ :: AddChildren c => Namespace -> String -> c -> Xml Elem
xelemQ ns' name children = Xml $ \(Elem buffer) ->
    do oldUriMap <- ask
       let (ns, uriMap, newNs) = genValidNsForDesiredPrefix oldUriMap ns'
       let elemNameBuilder =
               case ns of
                 DefaultNamespace -> fromString name
                 (QualifiedNamespace p u) -> fromString p `mappend` fromString ":" `mappend` fromString name
       let nsDeclBuilder = case ns of
             DefaultNamespace -> mempty
             (QualifiedNamespace p u) ->
                 let nsDeclaration' = fromString " xmlns:" `mappend` fromString p `mappend` fromString "=\""
                                      `mappend` fromString u `mappend` fromString "\""
                 in if newNs then nsDeclaration' else mempty
       let b1 = mappend buffer $ fromString "<"
       let b2 = b1 `mappend` elemNameBuilder `mappend` nsDeclBuilder
       let b3 = addChildren children b2 uriMap
       let builderOut = Elem (b3 `mappend` fromString "</" `mappend` elemNameBuilder `mappend` fromString "\n>")
       return (builderOut, oldUriMap)

xelems :: [Xml Elem] -> Xml Elem
xelems = foldl mappend noElems

noElems :: Xml Elem
noElems = xempty

-- xelemWithText :: MkElem n (Xml Elem) => n -> String -> Xml Elem
xelemWithText n t = xelem n (xtext t)

instance Monoid (Xml Elem) where
    mempty = noElems
    mappend x1 x2 = Xml $ \t ->
        do nsEnv <- ask
           let t2 = fst $ runXml nsEnv x1 t
               t3 = fst $ runXml nsEnv x2 t2
           return (t3, nsEnv)

--
-- Other XML constructs
--

-- content is escaped
xtext :: TextContent t => t -> Xml Elem
xtext content = append $ textBuilder content

-- content is NOT escaped
xtextRaw :: RawTextContent t => t -> Xml Elem
xtextRaw content = append $ rawTextBuilder content

-- no escaping performed
xentityRef :: String -> Xml Elem
xentityRef name = append $ fromChar '&' <> fromString name <> fromChar ';'

class Renderable t => Misc t where
    -- no escaping performed
    xprocessingInstruction :: String -> String -> Xml t
    xprocessingInstruction target content =
        append $ fromString "<?" <>
                 fromString target <>
                 fromChar ' ' <>
                 fromString content <>
                 fromString "?>"
    -- no escaping performed
    xcomment :: String -> Xml t
    xcomment content =
        append $ fromString "<!--" <>
                 fromString content <>
                 fromString "-->"

instance Misc Elem
instance Misc Doc

--
-- Operators

infixl 6 <>
(<>) :: Monoid t => t -> t -> t
(<>) = mappend

infixl 5 <#>
(<#>) :: a -> b -> (a, b)
(<#>) x y = (x, y)

--
-- Rendering
--

class XmlOutput t where
    fromBuilder :: Builder -> t

instance XmlOutput BS.ByteString where
    fromBuilder = toByteString

instance XmlOutput BSL.ByteString where
    fromBuilder = toLazyByteString

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

xrender :: (Renderable r, XmlOutput t) => Xml r -> t
xrender r = fromBuilder $ builder r'
    where
      r' = fst $ runXml emptyNsEnv r (mkRenderable mempty)

--
-- Utilities
--

genValidNsForDesiredPrefix :: NsEnv -> Namespace -> (Namespace, NsEnv, Bool)
genValidNsForDesiredPrefix env ns =
    case ns of
      DefaultNamespace -> (ns, env, False)
      QualifiedNamespace p u ->
          let validPrefix = genValidPrefix env p u
          in (QualifiedNamespace validPrefix u
             ,Map.insert validPrefix u env
             ,not $ Map.member validPrefix env)
    where
      genValidPrefix :: NsEnv -> Prefix -> Uri -> Prefix
      genValidPrefix env prefix uri =
        case Map.lookup prefix env of
          Nothing -> prefix
          Just foundUri ->
              if foundUri == uri
                 then prefix
                 else genValidPrefix env ('_':prefix) uri

append :: Renderable t => Builder -> Xml t
append builder' = Xml $ \t ->
    do nsEnv <- ask
       return $ (mkRenderable (builder t <> builder'), nsEnv)

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
xhtmlDoctypeStrict =
    "<!DOCTYPE html\n" ++
    "    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n" ++
    "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

xhtmlStrictDocInfo = defaultDocInfo { docInfo_docType = Just xhtmlDoctypeStrict }

xhtmlDoctypeTransitional =
    "<!DOCTYPE html\n" ++
    "    PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n" ++
    "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"

xhtmlTransitionalDocInfo = defaultDocInfo { docInfo_docType = Just xhtmlDoctypeTransitional }

xhtmlDoctypeFrameset =
    "<!DOCTYPE html\n" ++
    "    PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"\n" ++
    "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"

xhtmlFramesetDocInfo = defaultDocInfo { docInfo_docType = Just xhtmlDoctypeFrameset }

xhtmlRootElem :: String -> Xml Elem -> Xml Elem
xhtmlRootElem lang children =
    xelem "html" (xattr "xmlns" "http://www.w3.org/1999/xhtml" <>
                  xattr "xml:lang" lang <>
                  xattr "lang" lang <#>
                  children)
