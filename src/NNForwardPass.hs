-- A Module that contains all functions necessary for the forward pass.
-- Note that this relies on a valid configuration from NNConfig!

module NNForwardPass (
   applyWeights, fp
) where

import NNTypes (Vector)
import NNConfig (applyFuncs)

-------------------------FORWARD PASS-----------------------

-- applyWeights returns a list of functions: [Input -> Output] by
-- currying the weights through applyFuncs.
applyWeights :: (Floating a) => [Vector a] -> [Vector a -> Vector a]
applyWeights wss = zipWith ($) applyFuncs wss

-- fp returns the forward pass through the network by 
-- currying the weights through applyWeights, given some input and some
-- weight vector.
fp :: (Floating a) => Vector a -> [Vector a] -> [Vector a]
fp xs wss = scanl (\x f -> f x) xs (applyWeights wss)

