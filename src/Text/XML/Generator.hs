module Text.XML.Generator where

import qualified Data.ByteString.Lazy as BSL
import Data.Monoid

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8

newtype Out t = Out { outBuf :: Builder }
data ATTR = ATTR
data ELEM = ELEM
type Attr = Out ATTR
type Elem = Out ELEM


type Prefix = String
type Uri = String
data Namespace
    = DefaultNamespace
    | QualifiedNamespace Prefix Uri

data OutEnv
    = OutEnv
      { outEnv_counter :: Int
      , outEnv_namespaceMap :: Map Uri Int }

type Trans t = t -> Reader OutEnv t

{-
class XmlGen t where
    xattr :: String -> String -> t ATTR -> t ATTR
    xelem :: String -> (t ATTR -> t ATTR) -> (t ELEM-> t ELEM) -> t ELEM -> t ELEM

instance XmlGen Out where
  -}

{-
- Escaping
- rawText
- comments
- processing instructions
- Xml header
- syntactic sugar for elements with no attributes/children
- namespaces
-}

xattr :: String -> String -> Trans Attr
xattr key value (Out buffer)
  = Out (mconcat [buffer, spaceBuilder, keyBuilder, startBuilder, valueBuilder, endBuilder])
  where
  spaceBuilder = fromString " "
  keyBuilder = fromString key
  valueBuilder = fromString value
  startBuilder = fromString "=\""
  endBuilder = fromString "\""

xelem :: String -> Trans Attr -> Trans Elem -> Trans Elem
xelem name attrs elems (Out outBuffer)
  = endOut
  where
  startOut = Out (mconcat [outBuffer, fromString "<", fromString name])
  (Out attrsBuffer) = attrs startOut
  (Out elemsBuffer) = elems (Out $ mappend attrsBuffer (fromString "\n>"))
  endOut = Out (mconcat [elemsBuffer, fromString "</", fromString name, fromString "\n>"])

xattrQ :: Namespace -> String -> String -> Trans Attr
xattrQ = undefined

xelemQ :: Namespace -> String -> Trans Attr -> Trans Elem -> TransElem
xelemQ = undefined

xtext :: String -> Trans Elem
xtext content (Out buffer)
 = Out (buffer `mappend` fromString content)


(<#>) :: Trans (Out t) -> Trans (Out t) -> Trans (Out t)
(<#>) comb1 comb2 = comb2 . comb1


xrender :: Trans Elem -> Builder
xrender elem = outBuf (elem (Out $ mempty))

xempty :: Out t -> Out t
xempty = id

test :: IO ()
test = BSL.putStr $ toLazyByteString $ xrender xsample

nsFoo = ("http://www.factisresearch.com/ns/foo", "foo")

xelemFoo = xelem nsFoo

xsample :: Elem -> Elem
xsample =
  xelem "foo" (xattr "key" "value")
              (xelem "bar" xempty (xtext "BAR")
               <#>
               xelem "spam" xempty (xelem "egg" xempty xempty <#> xtext "this is spam!"))


-- sample (Out "")
