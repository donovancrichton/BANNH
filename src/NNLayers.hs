-- A module containing the functions for creating specific types of layers
-- in the neural network, and preparing the layers for use.

module NNLayers (
   fc, prepLayer, costLayer
) where

import NNTypes (Layer, Vector, InputFunction, ActivationFunction, Scalar)

-------------------LAYER IMPLEMENTATIONS------------------------

-- fc defines the implementation for a fully connected layer.
-- That is a layer where each value of the input layer is connected
-- to each node in the output layer. This function will apply the 
-- input function to both the inputs and the weights, and apply
-- the activation function to the result.
fc :: Layer a
fc fi fa ws xs = [ fa s | s <- fi xs ws ]

--------------------LAYER PREPERATION---------------------

-- prepLayer partially applies (or curries) the input function and the 
-- activation function through a list of layers compatible with those
-- functions. prepLayer returns a list of functions: weights ->
-- input -> output
prepLayer :: (Floating a) => InputFunction a -> ActivationFunction a ->
   [Layer a] -> [Vector a -> Vector a -> Vector a]
prepLayer fi af ls = map (\f -> f fi af) ls

-- costLayer takes the labelled output, a cost function, and some input
-- and returns the cost of the input with respect to the output.
-- This is used to curry a cost function with the labelled data to produce
-- a final layer for the network.
costLayer :: (Floating a) =>  
   Vector a ->
   (Scalar a -> Scalar a -> Scalar a) ->
   Vector a -> Vector a
costLayer as costFunc input = [sum costs]
   where xys   = zip input as
         costs = map (uncurry costFunc) xys

