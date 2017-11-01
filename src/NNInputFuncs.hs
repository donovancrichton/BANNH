-- This Module contains all possible input functions that can be used from
-- the NNConfig module.

module NNInputFuncs (
  sp
) where

import NNTypes (InputFunction)
import NNUtils (chunk)

import Data.List (foldl') -- for sp

-----------------------INPUT FUNCTIONS---------------------------

-- sp defines the vector/matrix product to get the linear combination of
-- inputs and weights. sp Also adds the bias node to each linear
-- combination.
sp :: (Floating a) => InputFunction a
sp xs ws = map (\ws -> foldl' (+) 0 $! zipWith (*) ws xs') ws' 
   where xs' = xs ++ [1] -- add the bias node
         ws' = chunk (length xs + 1) ws


