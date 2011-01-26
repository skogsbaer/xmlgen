import Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import System.Environment
import qualified Data.Text as T

import Text.XML.Generator

gen1 :: Int -> IO ()
gen1 numberOfElems = BSL.writeFile "/tmp/test.xml" (xrender doc)
      where doc = xelem "root" $ xelems $ map (\s -> xelem "foo" (xattr "key" s, xtext s)) (map (\i -> T.pack (show i) :: T.Text) [1..numberOfElems])

gen2 :: Int -> IO ()
gen2 numberOfElems = BSL.writeFile "/tmp/test.xml" (xrender doc)
      where doc = xelem "root" $ foldr (<>) xempty $ map (\s -> xelem "foo" (xattr "key" s, xtext s)) (map (\i -> T.pack (show i) :: T.Text) [1..numberOfElems])

gen3 :: Int -> IO ()
gen3 numberOfElems = BSL.writeFile "/tmp/test.xml" (xrender doc)
      where doc = xelem "root" $ xattrs $ map (\s -> xattr "key" s) (map (\i -> T.pack (show i) :: T.Text) [1..numberOfElems])

main =
    do args <- getArgs
       case args of
         "--standalone-1":s:[] -> gen1 (read s)
         "--standalone-2":s:[] -> gen2 (read s)
         "--standalone-3":s:[] -> gen3 (read s)
         _ -> defaultMain (concatMap (\i -> [bench ("gen1 " ++ show i) (gen1 i),
                                             bench ("gen2 " ++ show i) (gen2 i),
                                             bench ("gen3 " ++ show i) (gen3 i)]) [1000, 10000, 100000, 1000000])
