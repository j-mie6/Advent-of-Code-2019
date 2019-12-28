{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Comm where
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader

data CommState r x m = CommState 
  { _pipe :: x -> CommT r x m ()
  , _ack :: CommT r x m x
  }
newtype CommT r x m a = CommT (ContT r (StateT (CommState r x m) (ReaderT (CommT r x m x) m)) a) 
  deriving Functor
  deriving Applicative
  deriving Monad
  deriving MonadCont
  deriving (MonadState (CommState r x m))
  deriving (MonadReader (CommT r x m x))

ignore :: Monad m => x -> CommT r x m ()
ignore _ = void (join (gets _ack))

send :: Monad m => x -> CommT r x m ()
send x = callCC (\ _ack -> 
  do k <- gets _pipe
     put (CommState ignore (_ack ()))
     k x)
     
recv :: Monad m => CommT r x m x
recv = callCC (\ _pipe ->
  do k <- gets _ack
     abort <- ask
     put (CommState _pipe abort)
     k)

-- The first to receive gets executed first
-- It should be given firstSend as it's continuation
connect :: Monad m => CommT r x m x -> CommT r x m a -> CommT r x m a
connect firstSend firstReceive =
  do put (CommState ignore firstSend)
     firstReceive

runCommT :: Monad m => CommT r x m r -> m r
runCommT (CommT m) = 
  runReaderT (
    do abort <- ask
       evalStateT (
         runContT m return) 
           (CommState ignore abort))
    (error "Uh oh")
