{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Pipe where
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans

type Pipe x = PipeT x Identity
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

instance MonadTrans (PipeT x) where 
  lift m = PipeT (\is -> (, is, id) <$> m)

runPipe :: Pipe x a -> [x] -> (a, [x])
runPipe p is = runIdentity (runPipeT p is)

runPipeT :: Monad m => PipeT x m a -> [x] -> m (a, [x])
runPipeT (PipeT f) is = do (x, _, os) <- f is; return (x, os [])

pipeOut :: Monad m => x -> PipeT x m ()
pipeOut x = PipeT (\is -> return ((), is, (x:)))

pipeIn :: Monad m => PipeT x m x
pipeIn = PipeT (\(x:is) -> return (x, is, id))