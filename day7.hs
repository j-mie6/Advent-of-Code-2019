{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Data.List
import IntCode

data PipeT x m a = PipeT ([x] -> m (a, [x], [x] -> [x])) deriving Functor
instance Monad m => Applicative (PipeT x m) where
  pure = return
  (<*>) = ap
instance Monad m => Monad (PipeT x m) where
  return x = PipeT (\is -> return (x, is, id))
  PipeT f >>= g = PipeT (\is -> 
    do (x, is', os) <- f is
       let PipeT g' = g x
       (y, is'', os') <- g' is'
       return (y, is'', os . os'))
instance MonadTrans (PipeT x) where lift m = PipeT (\is -> (, is, id) <$> m)
runPipeT :: Monad m => PipeT x m a -> [x] -> m (a, [x])
runPipeT (PipeT f) is = do (x, _, os) <- f is; return (x, os [])

pipeOut :: Monad m => x -> PipeT x m ()
pipeOut x = PipeT (\is -> return ((), is, (x:)))

pipeIn :: Monad m => PipeT x m x
pipeIn = PipeT (\(x:is) -> return (x, is, id))

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
