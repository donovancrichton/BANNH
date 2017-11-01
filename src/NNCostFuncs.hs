-- This Module contains all possible cost funcs that can be used in
-- BANNHL, and are configued in the network by modifying the NNConfig 
-- module Haskell code.

module NNCostFuncs (
   logistic, sError
) where

import NNTypes (Scalar)

---------------------------COST FUNCTION------------------------

-- The logistic cost function ensures convexity if sigmoid is used
-- as the activation function. 
logistic :: (Floating a, Eq a, Ord a) => Scalar a -> Scalar a -> Scalar a
logistic x y | y == 0 = (-1) * log (1 - x)
             | otherwise = (-1) * log x

-- The squared error cost function, with a (1/2) on the front to make
-- it easily differentiable.
sError :: (Floating a) => Scalar a -> Scalar a -> Scalar a
sError target actual = 1 / 2 * ( (target - actual) * (target - actual) )

