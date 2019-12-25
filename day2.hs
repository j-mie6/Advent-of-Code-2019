{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Array.IO
import Control.Monad
import IntCode

instance (Read a, Show a) => MonadInput IO a where
  input  = readLn
  output = print

runWithValues :: Int -> Int -> IO Int
runWithValues x y =
  do xs <- Main.input
     arr <- initialise @IOArray xs
     writeArray arr 1 x
     writeArray arr 2 y
     execute arr

task1 :: IO Int
task1 = runWithValues 12 2

task2 :: IO (Int, Int)
task2 = fmap head . sequence $
  do x <- [0..99]
     y <- [0..99]
     return $ do z <- runWithValues x y
                 guard (z == 19690720)
                 return (x, y)

split :: (a -> Bool) -> [a] -> [[a]]
split f = foldr g [[]]
  where
    g x (ys:yss) 
      | f x  = []:ys:yss
      | otherwise = (x:ys):yss

input :: IO [Int]
input = map read . split (== ',') <$> readFile "day2-input.txt"
