{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Array.IO
import Control.Monad
import IntCode

instance (Read a, Show a) => MonadInput IO a where
  input  = readLn
  output = print

test :: IO Int
test = Main.input >>= (initialise @IOArray >=> execute)

input :: IO [Int]
input = readProgram "day5-input.txt"
