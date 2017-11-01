-- A Module that allows for some manual configuration of the network outside of
-- the established learning parameters and layer sizes in BANNHL.
-- Specifically it allows the construction of layers and the network by fixing
-- some parameters, however it must ensure that all activation and input
-- functions are correctly imported.

module NNConfig (
   applyFuncs, sizes, cost
)
where

import NNTypes
import NNLayers (fc, prepLayer, costLayer)
import NNActivationFuncs (sigmoid)
import NNInputFuncs (sp)
import NNCostFuncs (sError, logistic)

-- sizes is the function controlling the structure of the network, by
-- determining the number of neurons in each layer. The first layer
-- takes an input of N + 1 to an output of M, where the + 1 represents the
-- allowance for a bias node, subsequent layers take inputs of the previous
-- layers M + 1 to allow for a bias node, to the desired number of outputs.
sizes :: [Size Int]
sizes = [(2, 10), (11, 1)]

-- layers generates a number of fully connected layers, the size is
-- determined by the weights of the network. For classification replicate
-- layers to the size of the network, for regression, make it one smaller
layers :: [Layer a]
layers = replicate 2 fc

-- applyFuncs returns a list of functions: [Weights -> Inputs -> Outputs]
-- by currying the specified activation function and input function
-- through prepLayer. ++ prepLayer sp id allows a sigmoid network
-- to perform regression if appended to the network as the final layer.
applyFuncs :: (Floating a) => [Vector a -> Vector a -> Vector a]
applyFuncs = prepLayer sp sigmoid layers ++ prepLayer sp id layers

-- cost implements the cost function with regard to the training outputs
-- as a seperate and final layer within the neural network. This gets appended
-- during the backPropogation step by BANNHL. Modify the cost function
-- in the list below to change the network cost function.
cost :: (Ord a, Floating a) => Vector a -> [Vector a -> Vector a]
cost as = [costLayer as sError]


