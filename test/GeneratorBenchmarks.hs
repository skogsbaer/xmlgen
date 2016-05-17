{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import System.Environment
import Data.Monoid
import qualified Data.Text as T

import Text.XML.Generator

benchElems :: Int -> IO ()
benchElems numberOfElems = BSL.writeFile "/tmp/test.xml" (xrender doc)
      where doc = xelem "root" $ xelems $ map (\s -> xelem "foo" (xattr "key" s, xtext s)) (map (\i -> T.pack (show i)) [1..numberOfElems])

benchAttrs :: Int -> IO ()
benchAttrs numberOfElems = BSL.writeFile "/tmp/test.xml" (xrender doc)
      where doc = xelem "root" $ xattrs $ map (\s -> xattr ("key-" `mappend` s) s) (map (\i -> T.pack (show i)) [1..numberOfElems])

main =
    do args <- getArgs
       case args of
         "--elems":s:[] -> benchElems (read s)
         "--attrs":s:[] -> benchAttrs (read s)
         _ -> defaultMain (concatMap (\i -> [bench (show i ++ " elems") (whnfIO (benchElems i)),
                                             bench (show i ++ " attrs") (whnfIO (benchAttrs i))]) [1000, 10000, 100000, 1000000])
