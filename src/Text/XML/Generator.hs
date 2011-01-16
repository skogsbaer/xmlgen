{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies, MultiParamTypeClasses #-}
module Text.XML.Generator (

    Xml, Doc, DocInfo, Elem, Attr, Namespace, Prefix, Uri

  , XmlOutput, Renderable

  , ns, doc, defaultDocInfo
  , xattr, xattrQ, xattrs
  , xelem, xelemEmpty, xelems, xelemQ
  , xtext, xentityRef, xprocessingInstruction, xcomment, xempty
  , xrender

  , (<>), (<#>)

  , xhtmlFramesetDocInfo, xhtmlStrictDocInfo, xhtmlTransitionalDocInfo
  , xhtmlRootElem

) where

{-
TODO:

- raw content
- Data.Text.Text, Data.Text.Lazy.Text as type for content
- tests

-}

import Prelude hiding (elem)
import Control.Monad.Reader (Reader(..), ask, asks, runReader)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid

import Blaze.ByteString.Builder hiding (empty, append)
import qualified Blaze.ByteString.Builder as Blaze
import Blaze.ByteString.Builder.Char.Utf8

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Char (isPrint, ord)

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
-- Attributes
--

class MkAttr n where
    type MkAttrRes n
    xattr :: n -> MkAttrRes n

instance MkAttr String where
    type MkAttrRes String = String -> Xml Attr
    xattr = xattrQ DefaultNamespace

instance MkAttr Namespace where
    type MkAttrRes Namespace = String -> String -> Xml Attr
    xattr = xattrQ

-- value is escaped
xattrQ :: Namespace -> String -> String -> Xml Attr
xattrQ ns' key value = Xml $ \(Attr buffer) ->
    do uriMap' <- ask
       let (ns, uriMap, newNs) = genValidNsForDesiredPrefix uriMap' ns'
           builder = case ns of
                       DefaultNamespace -> mconcat [spaceBuilder, keyBuilder, startBuilder
                                                   ,valueBuilder, endBuilder]
                       QualifiedNamespace p u ->
                           let uriBuilder = fromString u
                               prefixBuilder = fromString p
                           in if newNs
                                 then mconcat [spaceBuilder, nsDeclStartBuilder, colonBuilder
                                              ,prefixBuilder, startBuilder, uriBuilder
                                              ,endBuilder, spaceBuilder, prefixBuilder
                                              ,colonBuilder, keyBuilder, startBuilder
                                              ,valueBuilder, endBuilder]
                                  else mconcat [spaceBuilder, prefixBuilder, colonBuilder
                                               ,keyBuilder, startBuilder
                                               ,valueBuilder, endBuilder]
       return $ (Attr (mconcat [buffer, builder]), uriMap)
    where
      spaceBuilder = fromString " "
      keyBuilder = fromString key
      startBuilder = fromString "=\""
      valueBuilder = fromString (escape value)
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
    xelemEmpty ::n -> MkEmptyElemRes n

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
                 (QualifiedNamespace p u) -> mconcat [fromString p, fromString ":", fromString name]
       let nsDeclBuilder = case ns of
             DefaultNamespace -> mempty
             (QualifiedNamespace p u) ->
                 let nsDeclaration' = mconcat [fromString " xmlns:", fromString p, fromString "=\""
                                              ,fromString u, fromString "\""]
                 in if newNs then nsDeclaration' else mempty
       let b1 = mappend buffer $ fromString "<"
       let b2 = mconcat [b1, elemNameBuilder, nsDeclBuilder]
       let b3 = addChildren children b2 uriMap
       let builderOut = Elem (mconcat [b3, fromString "</", elemNameBuilder, fromString "\n>"])
       return (builderOut, oldUriMap)

xelems :: [Xml Elem] -> Xml Elem
xelems = foldl mappend noElems

noElems :: Xml Elem
noElems = xempty

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
xtext :: String -> Xml Elem
xtext content = append $ fromString (escape content)

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

escape :: String -> String
escape s = escStr s ""
    where
      -- stolen from xml-light
      escStr             :: String -> ShowS
      escStr cs rs        = foldr escChar rs cs
      escChar            :: Char -> ShowS
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
          | otherwise -> showString "&#" . shows oc . showChar ';'
            where oc = ord c

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
