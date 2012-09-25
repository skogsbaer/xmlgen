import Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import System.Environment
import qualified Data.Text as T

import Text.XML.Generator

benchElems :: Int -> IO ()
benchElems numberOfElems = BSL.writeFile "/tmp/test.xml" (xrender doc)
      where doc = xelem "root" $ xelems $ map (\s -> xelem "foo" (xattr "key" s, xtext s)) (map (\i -> T.pack (show i)) [1..numberOfElems])

benchAttrs :: Int -> IO ()
benchAttrs numberOfElems = BSL.writeFile "/tmp/test.xml" (xrender doc)
      where doc = xelem "root" $ xattrs $ map (\s -> xattr ("key-" ++ s) (T.pack s)) (map (\i -> show i) [1..numberOfElems])

main =
    do args <- getArgs
       case args of
         "--elems":s:[] -> benchElems (read s)
         "--attrs":s:[] -> benchAttrs (read s)
         _ -> defaultMain (concatMap (\i -> [bench (show i ++ " elems") (benchElems i),
                                             bench (show i ++ " attrs") (benchAttrs i)]) [1000, 10000, 100000, 1000000])
