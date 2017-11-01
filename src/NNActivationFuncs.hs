-- This Module contains all possible activation functions that can be used
-- with the neural network
module NNActivationFuncs (
   sigmoid
) where

import NNTypes (ActivationFunction)

-----------------------ACTIVATION FUNCTIONS----------------------

-- sigmoid defines the activation function for the layer
sigmoid :: (Floating a) => ActivationFunction a
sigmoid x = 1 / (1 + exp(negate x))
