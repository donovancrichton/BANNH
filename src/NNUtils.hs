-- A Module containing useful utility functions involving lists for use with 
-- BANNHL and BANNHC.

module NNUtils (
   trim, chunk
) where

import NNTypes (Matrix)

--trims the training data of the final row containing the classification.
trim :: Matrix a -> Matrix a
trim m = [reverse $ tail $ reverse vs | vs <- m ]

-- split a list into a list of n-length lists, with the final list length
-- being the remainder of the division of xs / n
chunk :: (Floating a) => Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

      



