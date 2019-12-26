{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
import Data.Array.ST
import Control.Monad.ST
import Data.List
import IntCode
import Pipe

instance Monad m => MonadInput (PipeT a m) a where
  input = pipeIn
  output = pipeOut

mkProgram :: [Int] -> PipeT Int (ST s) (STArray s Int Int)
mkProgram = initialise

ampControl :: [Int] -> Int -> Int -> Int
ampControl xs phase input = 
  let (_, [output]) = runST (runPipeT (mkProgram xs >>= execute) [phase, input])
  in output

pipeline :: [Int] -> [Int] -> Int
pipeline xs = foldl (flip (ampControl xs)) 0

findMax :: IO Int
findMax = 
  do xs <- Main.input
     return (maximum (map (pipeline xs) (permutations [0, 1, 2, 3, 4])))

split :: (a -> Bool) -> [a] -> [[a]]
split f = foldr g [[]]
  where
    g x (ys:yss) 
      | f x  = []:ys:yss
      | otherwise = (x:ys):yss

input :: IO [Int]
input = map read . split (== ',') <$> readFile "day7-input.txt"
