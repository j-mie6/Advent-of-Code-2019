{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Array.IO
import Control.Monad.List
import Control.Monad.Trans
import IntCode

instance (Read a, Show a) => MonadInput IO a where
  input  = readLn
  output = print

test :: IO Int
test = Main.input >>= (initialise @IOArray >=> execute)

split :: (a -> Bool) -> [a] -> [[a]]
split f = foldr g [[]]
  where
    g x (ys:yss) 
      | f x  = []:ys:yss
      | otherwise = (x:ys):yss

input :: IO [Int]
input = map read . split (== ',') <$> readFile "day5-input.txt"
