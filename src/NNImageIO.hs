module NNImageIO (
   getBitmaps, checkBMP, getRawPixels
)
where

import Codec.BMP
import System.Directory
import qualified Data.ByteString as B

checkBMP :: Either Error BMP -> BMP
checkBMP (Right b) = b
checkBMP _ = undefined --errors do not include exceptions apparently.

getClassification :: (Num a) => FilePath -> IO (BMP, [a])
getClassification path = do
   let bmp :: IO (Either Error BMP)
       bmp = readBMP path
   b <- bmp
   let b' = checkBMP b
       c :: Num a => (BMP, [a])
       c | path !! (length path - 8) == 'x' = (b', [1, 0])
         | otherwise = (b', [0, 1])
   return c  

getClass :: (Num a, Num b) => FilePath -> IO ([a], [b])
getClass path = do
   pxs <- B.readFile path
   let p :: (Num a) => [a]
       p = fmap fromIntegral $ B.unpack pxs
       c :: (Num a, Num b) => ([a], [b])
       c | path !! (length path - 8) == 'x' = (p, [1, 0])
         | otherwise = (p, [0, 1])
   return c

getBitmaps :: Num a => FilePath -> IO [([a], [a])]
getBitmaps path = do
   fileNames <- getDirectoryContents path
   let paths :: [FilePath]
       paths = fmap (\x -> path ++ x) fileNames
       paths' :: [FilePath]
       paths' = filter (\x -> x /= path ++ "." && x /= path ++ "..") paths
   bmps <- sequence $ fmap getClassification paths'
   let bss  = fmap (\(x, y) -> (B.unpack $ unpackBMPToRGBA32 x, y)) bmps 
       bss' = fmap (\(x, y) -> (fmap fromIntegral x, y)) bss
   return bss'

getRawPixels :: (Num a, Num b) => FilePath -> IO [([a], [b])]
getRawPixels path = do
   fileNames <- getDirectoryContents path
   let paths :: [FilePath]
       paths = fmap (\x -> path ++ x) fileNames
       paths' :: [FilePath]
       paths' = filter (\x -> x /= path ++ "." && x /= path ++ "..") paths
   pxs <- sequence $ fmap getClass paths'
   return pxs
   

