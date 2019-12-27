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

input :: IO [Integer]
input = readProgram "day9-input.txt"