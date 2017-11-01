-- A Module containing the type definitions for use with an artificial neural
-- network, also contains any utility functions on said types.

module NNTypes ( 
   Scalar, Vector, Matrix, InputFunction, ActivationFunction, Layer, Size
) where

type Scalar a = a 
type Vector a = [Scalar a]
type Matrix a = [Vector a]
type Size a = (a, a)
type InputFunction a = Vector a -> Vector a -> Vector a
type ActivationFunction a = Scalar a -> Scalar a
type Layer a = InputFunction a -> ActivationFunction a ->
     Vector a -> Vector a -> Vector a

