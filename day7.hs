{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
import Data.Array.ST
import Control.Monad.ST
import Control.Monad.Cont
import Data.List
import IntCode
import Pipe

instance Monad m => MonadInput (PipeT a m) a where
  input = pipeIn
  output = pipeOut

mkProgram :: [Int] -> PipeT Int (ST s) (STArray s Int Int)
mkProgram = initialise

ampControl :: [Int] -> [Int] -> [Int]
ampControl xs is = snd (runST (runPipeT (mkProgram xs >>= execute) is))

feed :: [Int] -> Int -> Int -> Int
feed xs phase input = 
  let [output] = ampControl xs [phase, input]
  in output

pipeline :: [Int] -> [Int] -> Int
pipeline xs = foldl (flip (feed xs)) 0

findMax :: IO Int
findMax = 
  do xs <- Main.input
     return (maximum (map (pipeline xs) (permutations [0..4])))

-- The 5 amplifiers need to be connected in a circle and
-- given 5-9 as their phases, the first is fed 0 and they
-- will cycle round until the final one outputs one more
-- value, so we are forcing the `last` of the final os
loop :: [Int] -> [Int] -> [Int] -> [Int]
loop xs = foldr (\p next is1 -> ampControl xs (p : next is1)) (0:)

tie :: [Int] -> [Int] -> Int
tie xs ps = let os = loop xs ps (init os) in last os

findMaxLoop :: IO Int
findMaxLoop =
  do xs <- Main.input
     return (maximum (map (tie xs) (permutations [5..9])))

split :: (a -> Bool) -> [a] -> [[a]]
split f = foldr g [[]]
  where
    g x (ys:yss) 
      | f x  = []:ys:yss
      | otherwise = (x:ys):yss

input :: IO [Int]
input = map read . split (== ',') <$> readFile "day7-input.txt"
