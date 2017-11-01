-- Utility Imports
import System.Environment (getArgs)

-- Network Imports
import NNConfig
import NNForwardPass (fp)
import NNFileIO (readResult, readProblems)
import NNTypes
import NNUtils (chunk)



-----------------------MAIN----------------

main :: IO ()
main = do
   args <- getArgs
   let wsPath = args !! 0
       problemPath = args !! 1
   ws' <- readResult wsPath
   ps <- readProblems problemPath
   let ws   = fst ws'
       xMax = fst (snd ws')
       aMax = snd (snd ws')
       xs   = fmap (fmap (/ xMax)) ps
       xs'  = fmap (\x -> last $ fp x ws) xs
       xs'' = concat $ fmap (fmap (* aMax)) xs'
       ps'  = concat ps
       rs   = zip ps' xs''
   mapM_ print rs
   

