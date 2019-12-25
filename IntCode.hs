{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module IntCode where
import Data.Array
import Data.Array.Base
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

type MonadIntCode m t x = ( MonadTrans t
                          , MonadReader (() -> t m ()) (t m)
                          , MonadState (IntState x) (t m)
                          , MonadCont (t m)
                          , MonadInput m x)
type Intcode m t arr x = (Ix x, Num x, MArray arr x m, MonadIntCode m t x)

pattern Add  <- 1
pattern Mult <- 2
pattern In   <- 3
pattern Out  <- 4
pattern Off  <- 9
pattern Halt <- 99

pattern Pos <- 0
pattern Imm <- 1
pattern Rel <- 2

data IntState x = IntState { pc :: x }
newtype IntCode r x m a = IntCode {runIntCode :: StateT (IntState x) (ReaderT (() -> IntCode r x m ()) (ContT r m)) a}
  deriving Functor
  deriving Applicative
  deriving Monad
  deriving (MonadReader (() -> IntCode r x m ()))
  deriving (MonadState (IntState x))
  deriving MonadCont
instance MonadTrans (IntCode r x) where lift m = IntCode (lift (lift (lift m)))

class Monad m => MonadInput m a where
  input  :: m a
  output :: a -> m ()

instance (MonadTrans t, Monad (t m), MonadInput m a)
      => MonadInput (t m) a where
  input = lift input
  output = lift . output

instance (Monad m, MonadTrans t, Monad (t m), MArray arr e m)
      => MArray arr e (t m) where
  getBounds arr = lift (getBounds arr)
  getNumElements arr = lift (getNumElements arr)
  newArray bnds x = lift (newArray bnds x)
  unsafeRead arr idx = lift (unsafeRead arr idx)
  unsafeWrite arr idx x = lift (unsafeWrite arr idx x)
  
liftOp :: Intcode m t arr x => (x -> x -> x) -> arr x x -> x -> x -> x -> t m ()
liftOp op arr dst src1 src2 =
  do x <- readArray arr src1
     y <- readArray arr src2
     writeArray arr dst (op x y)

noargs m _ _ _ _ = m

-- This is magical, when this is executed then
-- it jumps to the continuation point stored by callCC
-- this has the effect of skipping the rest of the code
exit :: MonadReader (() -> m ()) m => m ()
exit = join (asks ($ ()))

getPc :: MonadState (IntState x) m => m x
getPc = gets pc

putPc :: MonadState (IntState x) m => x -> m ()
putPc pc = modify (\s -> s {pc = pc})

decode :: Intcode m t arr x => x -> (arr x x -> x -> x -> x -> t m ())
decode Add  = liftOp (+)
decode Mult = liftOp (*)
decode Halt = noargs exit
decode _    = noargs (fail "Unknown opcode")

-- This will need remodelling for when the opcode has a varying length
fetch :: Intcode m t arr x => arr x x -> t m (x, x, x, x)
fetch arr =
    do i <- getPc
       opcode <- readArray arr i
       src1   <- readArray arr (i + 1)
       src2   <- readArray arr (i + 2)
       dest   <- readArray arr (i + 3)
       putPc (i + 4)
       return (opcode, src1, src2, dest)

initialise :: (MArray arr x m, Ix x, Num x, Monad m) => [x] -> m (arr x x)
initialise xs = newListArray (0, fromIntegral m) xs
  where n = length xs
        m = n + (4 - n `mod` 4)

execute :: (Ix x, Num x, MonadInput m x, MArray arr x m) => arr x x -> m x--(arr Int Int)
execute arr = flip runContT return $
  -- callCC registers the readArray arr 0 as the place where code should return to
  -- when onExit is called, beautiful beautiful technique
  do callCC (\onExit -> runReaderT (evalStateT (runIntCode exec) (IntState 0)) (IntCode . lift . lift . onExit))
     readArray arr 0
     --return arr
  where
    exec = do (opcode, src1, src2, dest) <- fetch arr
              decode opcode arr dest src1 src2
              exec

{-execIntCode :: [Int] -> IO Int
execIntCode = initialise @IO @IOArray >=> execute

test :: [Int] -> IO ()
test xs = execIntCode xs >>= freeze @Int @IOArray @Int @IO @Array >>= (\arr -> 
  let ys = elems arr
  in print (take (length xs) ys))-}
