{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

import Control.Exception (catch, SomeException)

import System.Process
import System.Posix.Temp
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Environment

import Data.Char (ord, chr)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import Text.XML.HXT.Core hiding (xshow)
import Text.XML.HXT.DOM.ShowXml (xshow)
import Data.Tree.NTree.TypeDefs

import Data.String
import qualified Data.Text as T

import Test.Framework

import Text.XML.Generator

test :: Renderable r => FilePath -> Xml r -> IO ()
test f x = BSL.writeFile f (xrender x)

_NS_PR1_NS1_ = namespace "foo" "urn:foo"
_NS_PR4_NS1_ = namespace "___foo" "urn:foo"
_NS_PR2_NS2_ = namespace "_foo" "urn:_foo"
_NS_PR3_NS3_ = namespace "__foo" "urn:__foo"
_NS_PR1_NS3_ = namespace "foo" "urn:bar"

testNS :: Namespace
testNS = namespace "foo" "http://www.example.com"

xsample1 :: Xml Elem
xsample1 =
  xelemQ _NS_PR3_NS3_ "foo"
        (xattrQ _NS_PR2_NS2_ "key" "value" <>
         xattrQ _NS_PR2_NS2_ "key2" "value",
         xelemQ _NS_PR1_NS1_ "bar" (xattrQ _NS_PR2_NS2_ "key" "value" <#> xtext "BAR") <>
         xelemQ _NS_PR1_NS1_ "bar"
             (xelemQ _NS_PR1_NS3_ "spam" (xelemEmpty "egg" <> xtext "this is spam!")))

test_1 =
    do out <- runXmllint xsample1
       exp <- readExpected "1.xml"
       assertEqual exp out

xsample2 :: Xml Elem
xsample2 = xelem "foo" $
                xattr "key" "value" <>
                xattr "key2" "value2" <#>
                xelemEmpty "bar" <>
                xelem "spam" (xattr "key" "value") <>
                xelem "egg" (xtext "ham") <>
                xelemQEmpty testNS "bar" <>
                xelemQ testNS "spam" (xattrQ testNS "key" "value") <>
                xelemQ testNS "egg" (xelemEmpty "ham")

test_2 =
    do out <- runXmllint xsample2
       exp <- readExpected "2.xml"
       assertEqual exp out

xsample3 :: Xml Doc
xsample3 =
    doc defaultDocInfo $ xelem "foo" $ xattr "key" "val\"'&<>ue" <#> xtext "<&;'"

test_3 =
    do out <- runXmllint xsample3
       exp <- readExpected "3.xml"
       assertEqual exp out

xsample4 :: Xml Elem
xsample4 =
    xelemQ ns "x" (attrs <#>
                   xelemQ noNamespace "y" (attrs <#> xelemQ ns "z" attrs))
    where
      attrs = xattrQ ns "a" "in URI" <>
              xattrQ noNamespace "b" "in no ns" <>
              xattrQ defaultNamespace "c" "in default ns"
      ns = namespace "" "http://URI"

test_4 =
    do out <- runXmllint xsample4
       exp <- readExpected "4.xml"
       assertEqual exp out

xsample5 :: Xml Doc
xsample5 =
    doc defaultDocInfo $
      xelem "people" $
        xelems $ map (\(name, age) -> xelem "person" (xattr "age" age <#> xtext name)) people
    where
      people = [("Stefan", "32"), ("Judith", "4")]

test_5 =
    do out <- runXmllint xsample5
       exp <- readExpected "5.xml"
       assertEqual exp out

xhtmlSample :: Xml Elem
xhtmlSample =
    xhtmlRootElem "de" (xelem "head" (xelem "title" "Test") <> xelem "body" (xattr "foo" "1"))

test_xhtml =
    do out <- runXmllint xhtmlSample
       exp <- readExpected "xhtml.xml"
       assertEqual exp out

readExpected name =
    readFile ("test" </> name)
    `catch` (\(e::SomeException) -> do hPutStrLn stderr (show e)
                                       return "")

runXmllint :: Renderable r => Xml r -> IO String
runXmllint x =
    do (name, handle) <- mkstemp "/tmp/xmlgen-test-XXXXXX"
       let rx = xrender x
       BSL.hPut handle rx
       hClose handle
       readProcess "xmllint" ["--format", name] ""

prop_textOk (ValidXmlString s) =
    let docStr = xelem "root" (xattr "attr" s, xtext s)
        docText = xelem "root" (xattr "attr" t, xtext t)
        treeListStr = unsafePerformIO $ runX (readString [withWarnings no, withErrors no] (BSLC.unpack $ xrender docStr))
        treeListText = unsafePerformIO $ runX (readString [withWarnings no, withErrors no] (BSLC.unpack $ xrender docText))
    in treeListStr == treeListText
    where
      t = s

prop_quotingOk (ValidXmlString s) =
    let doc = xelem "root" (xattr "attr" s, xtext s)
        treeList = unsafePerformIO $ runX (readString [withWarnings no, withErrors no] (BSLC.unpack $ xrender doc))
        root = head treeList
    in case childrenOfNTree root of
         [NTree root children] ->
             let attrValue = case root of
                               XTag _ [NTree _ attrs] -> xshow attrs
                               XTag _ [NTree _ [NTree (XText attrValue) _]] -> attrValue
                               XTag _ [NTree _ []] -> ""
                 textValue = case children of
                               elems -> xshow elems
                               [NTree (XText textValue) _] -> textValue
                               [] -> ""
             in normWsAttr s == T.pack attrValue && normWsElem s == T.pack textValue
         l -> error (show root ++ "\n" ++ show l)
    where
      normWsAttr = T.replace "\r" " " . T.replace "\n" " " . T.replace "\n\r" " "
      normWsElem = T.replace "\r" "\n" . T.replace "\n\r" "\b"
      childrenOfNTree (NTree _ l) = l

newtype ValidXmlString = ValidXmlString T.Text
    deriving (Eq, Show)

instance Arbitrary ValidXmlString where
    arbitrary = sized $ \n ->
                do k <- choose (0, n)
                   s <- sequence [validXmlChar | _ <- [1..k] ]
                   return $ ValidXmlString (T.pack s)
        where
          validXmlChar =
              let l = map chr ([0x9, 0xA, 0xD] ++ [0x20..0xD7FF] ++
                               [0xE000..0xFFFD] ++ [0x10000..0x10FFFF])
              in elements l

main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
