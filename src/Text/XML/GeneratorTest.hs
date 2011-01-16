module Text.XML.Generator.Test where

import qualified Data.ByteString.Lazy as BSL
import Text.XML.Generator

test :: Renderable r => Xml r -> IO ()
test = BSL.putStr . xrender

_NS_PR1_NS1_ = ns "foo" "urn:foo"
_NS_PR4_NS1_ = ns "___foo" "urn:foo"
_NS_PR2_NS2_ = ns "_foo" "urn:_foo"
_NS_PR3_NS3_ = ns "__foo" "urn:__foo"
_NS_PR1_NS3_ = ns "foo" "urn:bar"

testNS :: Namespace
testNS = ns "foo" "http://www.example.com"

xsample :: Xml Elem
xsample =
  xelem _NS_PR3_NS3_ "foo"
       (xattr _NS_PR2_NS2_ "key" "value" <>
        xattr _NS_PR2_NS2_ "key2" "value",
        xelem _NS_PR1_NS1_ "bar" (xattr _NS_PR2_NS2_ "key" "value" <#> xtext "BAR") <>
        xelem _NS_PR1_NS1_ "bar"
            (xelem _NS_PR1_NS3_ "spam" (xelemEmpty "egg" <> xtext "this is spam!")))

xsample2 :: Xml Elem
xsample2 = xelem "foo" $
                xattr "key" "value" <>
                xattr "key2" "value2" <#>
                xelemEmpty "bar" <>
                xelem "spam" (xattr "key" "value") <>
                xelem "egg" (xtext "ham") <>
                xelemEmpty testNS "bar" <>
                xelem testNS "spam" (xattr testNS "key" "value") <>
                xelem testNS "egg" (xelemEmpty "ham")

xsample3 :: Xml Doc
xsample3 =
    doc defaultDocInfo $ xelem "foo" $ xattr "key" "val\"'&<>ue" <#> xtext "<&;'"
