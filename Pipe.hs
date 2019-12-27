{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Pipe where
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans

type Pipe x = PipeT x Identity
newtype PipeT x m a = PipeT (forall r. [x] -> (a -> [x] -> ([x] -> [x]) -> m r) -> m r) deriving Functor

instance Monad m => Applicative (PipeT x m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (PipeT x m) where
  return x = PipeT (\is k -> k x is id)
  PipeT f >>= g = PipeT (\is k ->
    f is (\x is' os ->
      let PipeT h = g x
      in h is' (\y is'' os' -> k y is'' (os . os'))))

instance MonadTrans (PipeT x) where
  lift m = PipeT (\is k -> m >>= \x -> k x is id)

runPipe :: Pipe x a -> [x] -> (a, [x])
runPipe p is = runIdentity (runPipeT p is)

runPipeT :: Monad m => PipeT x m a -> [x] -> m (a, [x])
runPipeT (PipeT f) is = f is (\x _ os -> return (x, os []))

pipeOut :: Monad m => x -> PipeT x m ()
pipeOut x = PipeT (\is k -> k () is (x:))

pipeIn :: Monad m => PipeT x m x
pipeIn = PipeT (\(~(x:is)) k -> k x is id)