-- A Module concerned with the reading of data from files.
-- NOTE: There is no graceful error handling, and the program will terminate
-- upon a failed read! Please ensure your files have the correct data!

module NNFileIO (
   readIntData, readResult, readProblems
) where

import Data.List.Split (splitOn)

-- readIntData converts a csv of Integer values to an IO [[Int]]
-- readIntData makes no guarantees about the correctness of the file
-- path, or of the data being read.
readIntData :: String -> IO [[Int]]
readIntData path = do
   f <- readFile path
   let file = lines f
       dataList = map (\x -> "[" ++ x ++ "]") file
       trainingData = map (\x -> read x :: [Int]) dataList
   return trainingData

-- readResult reads the information in the weights file specified
-- by the user when they run BANNHL. The data is required for
-- BANNHC to output sensible values (to re-scale normalised data).
readResult :: String -> IO ([[Double]], (Double, Double))
readResult path = do
   f <- readFile path
   let f' = filter (not . (`elem` "\n")) f
       rs = read f' :: ([[Double]], (Double, Double))
   return rs

-- readProblems reads the list of input data to run the
-- forward pass algorithm against.
readProblems :: String -> IO [[Double]]
readProblems path = do
   f <- readFile path
   let f' = lines f
       xs = map (\x -> read x :: Double) f'
       ps = fmap (:[]) xs
   return ps


