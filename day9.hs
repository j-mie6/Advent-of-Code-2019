{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Array.IO
import Control.Monad
import IntCode

instance (Read a, Show a) => MonadInput IO a where
  input  = readLn
  output = print

boost :: IO Integer
boost = Main.input >>= (initialise @IOArray >=> execute)

split :: (a -> Bool) -> [a] -> [[a]]
split f = foldr g [[]]
  where
    g x (ys:yss) 
      | f x  = []:ys:yss
      | otherwise = (x:ys):yss

input :: IO [Integer]
input = map read . split (== ',') <$> readFile "day9-input.txt"