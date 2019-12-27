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

ampControl :: [Int] -> [Int] -> [Int]
ampControl xs is = snd (runST (runPipeT (mkProgram xs >>= execute) is))

loop :: [Int] -> [Int] -> [Int] -> [Int]
loop xs = foldr (\p next is1 -> ampControl xs (p : next is1)) (0:)

pipeline :: [Int] -> [Int] -> Int
pipeline xs ps = 
  let os = loop xs ps (init os) 
  in last os

findMaxConfig :: [Int] -> IO Int
findMaxConfig ps =
  do xs <- Main.input
     return (maximum (map (pipeline xs) (permutations ps)))

task1 :: IO Int
task1 = findMaxConfig [0..4]

task2 :: IO Int
task2 = findMaxConfig [5..9]

input :: IO [Int]
input = readProgram "day7-input.txt"
