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
--ampControl xs is = snd (runST (runPipeT (mkProgram xs >>= execute) is))
ampControl _ = snd . runPipe twoIncLoop

-- Let's try running it with this program instead (i.e. just Pipe)
-- and see what happens?
twoIncLoop :: Pipe Int ()
twoIncLoop = 
  do pipeIn
     x <- pipeIn
     pipeOut (x + 1)
     x <- pipeIn
     pipeOut (x + 1)

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

split :: (a -> Bool) -> [a] -> [[a]]
split f = foldr g [[]]
  where
    g x (ys:yss) 
      | f x  = []:ys:yss
      | otherwise = (x:ys):yss

input :: IO [Int]
input = map read . split (== ',') <$> readFile "day7-input.txt"
