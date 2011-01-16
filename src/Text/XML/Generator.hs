module Text.XML.Generator
    ( Prefix, Uri, Namespace(..), Xml, Attr, Elem, Out
    , xattr, xelem, xelems, xattrQ, xelemQ, xtext, xempty, xrender
    , (<@>), (<#>)
    )
where

import Control.Monad.Reader (Reader(..), ask, asks, runReader)
import qualified Data.Map as Map
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
    deriving (Show, Eq)

type OutEnv = Map.Map Prefix Uri

type Xml t = t -> Reader OutEnv (t, OutEnv)

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

xattr :: String -> String -> Xml Attr
xattr = xattrQ DefaultNamespace

xelem :: String -> Xml Attr -> Xml Elem -> Xml Elem
xelem = xelemQ DefaultNamespace

xelems :: [Xml Elem] -> Xml Elem
xelems = foldl (<#>) xempty

xattrQ :: Namespace -> String -> String -> Xml Attr
xattrQ ns' key value (Out buffer) =
    do
        uriMap' <- ask
        let (ns, uriMap, newNs) = genValidNsForDesiredPrefix uriMap' ns'
        let builder = case ns of
                        DefaultNamespace -> mconcat [ spaceBuilder, keyBuilder, startBuilder
                                                    , valueBuilder, endBuilder]
                        QualifiedNamespace p u ->
                            case newNs of
                                 False -> mconcat [ spaceBuilder, prefixBuilder, colonBuilder
                                                  , keyBuilder, startBuilder
                                                  , valueBuilder, endBuilder]
                                 True -> mconcat [ spaceBuilder, nsDeclStartBuilder, colonBuilder
                                                 , prefixBuilder, startBuilder, uriBuilder
                                                 , endBuilder, spaceBuilder, prefixBuilder
                                                 , colonBuilder, keyBuilder, startBuilder
                                                 , valueBuilder, endBuilder]
                              where
                                nsDeclStartBuilder = fromString "xmlns"
                                uriBuilder = fromString u
                                prefixBuilder = fromString p
                                colonBuilder = fromString ":"
        return $ (Out (mconcat [buffer, builder]), uriMap)
        where
          spaceBuilder = fromString " "
          keyBuilder = fromString key
          valueBuilder = fromString value
          startBuilder = fromString "=\""
          endBuilder = fromString "\""

xelemQ :: Namespace -> String -> Xml Attr -> Xml Elem -> Xml Elem
xelemQ ns' name attrs elems (Out buffer)
  = do
      oldUriMap <- ask
      let (ns, uriMap, newNs) = genValidNsForDesiredPrefix oldUriMap ns'
      let elemNameBuilder = case ns of
            DefaultNamespace -> fromString name
            (QualifiedNamespace p u) -> mconcat [fromString p, fromString ":", fromString name]
      let nsDeclBuilder = case ns of
            DefaultNamespace -> mempty
            (QualifiedNamespace p u) -> nsDeclaration
              where nsDeclaration = if newNs then nsDeclaration' else mempty
                    nsDeclaration' = mconcat [fromString " xmlns:", fromString p, fromString "=\""
                        , fromString u, fromString "\""]
      let startBuffer = mappend buffer $ fromString "<"
      let startBuilder = mconcat [startBuffer, elemNameBuilder, nsDeclBuilder]

      let (Out attrsBuffer, uriMapAttrs) = flip runReader uriMap $ attrs (Out startBuilder)
      let (Out elemsBuffer) = fst $ flip runReader uriMapAttrs $ elems (Out $ mappend attrsBuffer (fromString "\n>"))
      let endOut = Out (mconcat [elemsBuffer, fromString "</", elemNameBuilder, fromString "\n>"])
      return (endOut, uriMapAttrs)

isDefaultNamespace :: Namespace -> Bool
isDefaultNamespace (DefaultNamespace) = True
isDefaultNamespace _ = False

getPrefix :: Namespace -> Prefix
getPrefix (QualifiedNamespace p _) = p
getPrefix _ = error "error!"

getUri :: Namespace -> Uri
getUri (QualifiedNamespace _ u) = u
getUri _ = error "error!"

xtext :: String -> Xml Elem
xtext content (Out buffer)
 = return $ (Out (buffer `mappend` fromString content), error "uriMap of element evaluated")

(<#>) :: Xml Elem -> Xml Elem -> Xml Elem
(<#>) comb1 comb2 t
  = do
      state <- ask
      let t2 = fst $ runReader (comb1 t) state
          t3 = fst $ runReader (comb2 t2) state
      return (t3, error "uriMap of element evaluated")

(<@>) :: Xml Attr -> Xml Attr -> Xml Attr
(<@>) comb1 comb2 t
  = do
      state <- ask
      let (t2, state') = runReader (comb1 t) state
          (t3, state'') = runReader (comb2 t2) state'
      return (t3, state'')

genValidNsForDesiredPrefix :: OutEnv -> Namespace -> (Namespace, OutEnv, Bool)
genValidNsForDesiredPrefix env ns =
    case ns of
         DefaultNamespace -> (ns, env, False)
         QualifiedNamespace p u -> ( QualifiedNamespace validPrefix u
                                   , Map.insert validPrefix u env
                                   , not $ Map.member validPrefix env
                                   )
           where validPrefix = genValidPrefix env p u

genValidPrefix :: OutEnv -> Prefix -> Uri -> Prefix
genValidPrefix env prefix uri =
    case Map.member prefix env of
         False -> prefix
         True -> let foundUri = Map.findWithDefault nextPrefix prefix env
                     nextPrefix = followingPrefix prefix
                 in if foundUri == uri then prefix else genValidPrefix env nextPrefix uri

followingPrefix :: Prefix -> Prefix
followingPrefix p = '_':p

xrender :: Xml Elem -> BSL.ByteString
xrender elem = toLazyByteString buffer
  where
  (Out buffer) = fst $ runReader (elem (Out $ mempty)) emptyState

emptyState :: OutEnv
emptyState = Map.empty

xempty :: Xml t
xempty t = do
    env <- ask
    return (t, env)

test :: IO ()
test = BSL.putStr $ xrender xsample

_NS_PR1_NS1_ = QualifiedNamespace "foo" "urn:foo"
_NS_PR4_NS1_ = QualifiedNamespace "___foo" "urn:foo"

_NS_PR2_NS2_ = QualifiedNamespace "_foo" "urn:_foo"

_NS_PR3_NS3_ = QualifiedNamespace "__foo" "urn:__foo"

_NS_PR1_NS3_ = QualifiedNamespace "foo" "urn:bar"

testNS :: Namespace
testNS = QualifiedNamespace "foo" "http://www.example.com"

xsample :: Xml Elem
xsample =
  xelemQ _NS_PR3_NS3_ "foo" ((xattrQ _NS_PR2_NS2_ "key" "value") <@> (xattrQ _NS_PR2_NS2_ "key2" "value"))
              (xelemQ _NS_PR1_NS1_ "bar" (xattrQ _NS_PR2_NS2_ "key" "value") (xtext "BAR")
               <#>
               xelemQ _NS_PR1_NS1_ "bar" xempty
                   (xelemQ _NS_PR1_NS3_ "spam" xempty (xelem "egg" xempty xempty <#> xtext "this is spam!")))


-- sample (Out "")
