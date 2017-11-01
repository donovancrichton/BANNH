
-- Neural Network Imports
import NNFileIO (readIntData)
import NNTypes
import NNConfig (cost, sizes, applyFuncs)
import NNForwardPass (fp)

-- Math Imports
import qualified Numeric.AD.Rank1.Kahn as AD

-- Utility Imports
import System.Environment (getArgs)
import System.Random (randomIO, randoms, mkStdGen, Random)

----------------------------UTILITY FUNCTIONS-------------------------------

-- randomList generates a list of random values between 0 and 1 given some
-- integer seed.
randomList :: (Floating a, Random a) => Int -> Vector a
randomList seed = map (\x -> x - 0.5) rs
   where rs = randoms (mkStdGen seed) :: (Floating a, Random a) => Vector a

-- setWeights returns a list of random weight vectors based on the size
-- list passed to it, and some integer seed.
setWeights :: (Floating a, Random a) => [Size Int] -> Int -> [Vector a]
setWeights ss seed = [ take (x * y) $! randomList seed | (x, y) <- ss ]

norm :: [[Double]] -> [[Double]]
norm xxs = fmap (\xs -> fmap (\x -> x / mx) xs) xxs
   where xs = concat xxs
         mx = maximum $! xs

--------------------APPLICATION FUNCTIONS--------------------

-- bp applies the backProp function given some actuals, some inputs
-- and some weights. It curries the forward pass results through
-- applyFuncs.
bp :: (Floating a, Ord a) => Vector a -> Vector a -> [Vector a] -> 
   [Vector a -> Vector a]
bp as xs wss = backProp as wss (take (length wss) (fp xs wss)) applyFuncs

-- deriveWeights takes a list of functions: [ [Vector a] -> [Vector a] ]
-- and curries each function through AD.jacobian, leaving a function:
-- Input -> Derivative of Output
deriveWeights :: (Floating a) => 
   [ [AD.Kahn a] -> [AD.Kahn a] ] -> [ [a] -> [[a]] ]
deriveWeights = fmap AD.jacobian

-------------------BACKWARD PASS-----------------

-- backProp returns a list of functions of weight -> error, so that the
-- derivative can be taken of each function to find the change in weight with
-- respect to the error. 
backProp :: (Floating a, Ord a) => 
   Vector a ->
   [Vector a] -> 
   [Vector a] -> 
   [Vector a -> Vector a -> Vector a] -> 
   [Vector a -> Vector a]
backProp as (w : ws) (x : xs) (f : fs) = r : backProp as ws xs fs 
    where h   = (`f` x)
          ts  = zipWith ($) fs ws ++ cost as
          hts = h : ts
          r   = foldl (.) id (reverse hts)
backProp _ _ _ _       = []

-- updateWeights updates the weights according to the derivative of the weight
-- with respect to the error, and some learning rate alpha.
updateWeights :: (Floating a) => a -> [Vector a] -> [Vector a] -> [Vector a]
updateWeights alpha ws1 ws2 = rs
   where rs  = map (map (\(x, y) -> x - alpha * y)) rs'
         rs' = zipWith zip ws1 ws2

-- sgd execuses stochastic gradient descent through a list of training values,
-- a list of inputs, and a list of weights, given some learning rate alpha.
sgd :: (Floating a, Ord a) => a -> [Vector a] -> [Vector a] -> [Vector a] -> 
  [Vector a]
sgd alpha (y : ys) (x : xs) wss = sgd alpha ys xs wss'
  where 
        c = deriveWeights (bp (fmap AD.auto y) (fmap AD.auto x) 
          (fmap (fmap AD.auto) wss))
        d = zipWith (\f x -> f x) c wss
        e = map (\x -> head x) d
        wss' = updateWeights alpha wss e
sgd alpha _ _ wss = wss

-- learn execuses stochastic gradient descent for a given n of epochs.
learn alpha as td n wss  
   | n > 0  = let s = sgd alpha as td wss
              in learn alpha as td (n - 1) s 
   | n == 0 = wss

-- main executes learn with the specified parameters.
main :: IO ()
main = do
  args <- getArgs
  seed <- randomIO
  let dataPath = args !! 0
      savePath = args !! 1
      epochs = read $ args !! 2 :: Int
      alpha = read $ args !! 3 :: Double
  tData <- readIntData dataPath
  let 
      td'' = map init tData
      td' = map (\xs -> map fromIntegral xs) td''
      tdMax = head $ maximum td'
      td = norm $ td'
      actuals'' = map tail tData
      actuals' = map (\xs -> map fromIntegral xs) actuals''
      actMax = head $ maximum actuals'
      actuals = norm $ actuals'
      x = setWeights sizes seed
      v = learn alpha actuals td epochs x
  print $ fp (last td) v
  writeFile savePath $! show (v, (tdMax, actMax))
