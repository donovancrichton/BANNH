{-# OPTIONS_GHC -funbox-strict-fields #-}

--import Data.Vector (foldl', Vector)
import Data.Matrix
import System.Random
import Data.List (foldl')
import NNImageIO


-- setWeights returns a matrix of randomly initialised Double values
-- between 0 and 1, this matrix is row rows high, and columns columns wide. 
-- The seed affects the values returned (the same seed results in the same
-- values).
setWeights :: Int -> Int -> Int -> Matrix Double
setWeights rows columns seed = 
   let ws = take (rows * columns) $ randomList seed
   in fromList rows columns ws

-- randomList is a helper function to build a list of random doubles
-- according to the Int seed passed. The list of doubles is infinite
-- taking from this will always result in the same values.
randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed) :: [Double]


-- fc returns the output of a fully connected layer as a Matrix. Sepcifically
-- it takes the weights, and the inputs performing a linear-combination on both
-- matrices and applying the activation function to the result.
fc :: (Double -> Double) -> Matrix Double -> Matrix Double -> Matrix Double
fc af xs ws =
   let lc = ws * xs
   in fmap (\x -> af x) $! lc

-- normWeights normalises the weights matrices between -0.5 and 0.5
-- inorder to facilitate efficient training.
normWeights :: Matrix Double -> Matrix Double
normWeights ws = fmap (\x -> x - 0.5) ws

-- normInput normalises the inputs of the images between -0.5 and 0.5
-- once again to facilities efficient training.
normInput :: Matrix Double -> Matrix Double
normInput xs = fmap (\x -> (x - 128) / 128) xs


processInput :: (Real a, Real b) => [([a], [b])] -> Matrix Double
processInput xys =
   let xs = fmap (\(x, y) -> (fmap (\x' -> f x') x)) xys
       f = fromRational . toRational
   in transpose $ normInput $ fromLists [(head xs)]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (negate x))

sigmoid' :: Double -> Double
sigmoid' x = exp (negate x) / ((exp (negate x) + 1) * (exp (negate x) + 1))

xEnt :: Double -> Double -> Double
xEnt a r | a /= 0.0 = a * log r
         | otherwise = 0.0

xEntropy :: [Double] -> [Double] -> Double
xEntropy as rs = 
   let ars = zip as rs
       es  = fmap (\(x, y) -> xEnt x y) ars
   in negate $ foldl' (+) 0 es

xEnt' :: Double -> Double -> Double
xEnt' a x | a == 0 = 0
          | otherwise = negate (1 / x)

xEntropy' :: [Double] -> Double -> [Double]
xEntropy' as x = fmap (\y -> xEnt' y x) as

softMax :: [Double] -> [Double]
softMax x = 
   let xExp = fmap (\x -> exp x) x
       sumx = foldl' (+) 0 xExp
   in fmap (\x -> x / sumx) xExp

-- jsm computes the softmax partial derivative term
-- in the softmax jacobian   
jsm :: Int -> Int -> [Double] -> Double
jsm i j xs = (xs !! i) * (1 - (xs !! j))

softMax' :: [Double] -> [[Double]]
softMax' xs = 
   let ls :: [Int]
       ls = [0 .. (length xs - 1)]
   in fmap (\x -> fmap (\y -> jsm x y xs) ls) ls



main = do
   bs <- getRawPixels "/home/donovan/Honours/Data/OX-Data/DATAs/"
   let as = head $ fmap (\(x, y) -> y) bs
       x1 = processInput bs
       w1 = normWeights $ setWeights 1024 1024 1
       w2 = normWeights $ setWeights 2 1024 1
       a2 = fc sigmoid x1 w1 
       a3 = fc sigmoid a2 w2
       a3' = softMax (concat $! toLists a3)
       rs' = (as, a3')
       --rs = fmap (\(a, r) -> xEntropy a r) rs'
       rs = xEntropy (fst rs') (snd rs') --this is the end of the forwardPass
       --ce :: [Matrix Double]
       ce = fromList 2 1 $! xEntropy' as rs
       --sm :: [Matrix Double]
       sm = (fromLists . softMax') $! a3'
       d1 = sm * ce
       d2 = fmap sigmoid' d1 
       d3 = d2 * (transpose a2) -- W2 update
       d4 = (transpose w2) * d2 
       d5 = fmap sigmoid' d4
       d6 = d5 * (transpose x1) --W1 update
   print $ d6
