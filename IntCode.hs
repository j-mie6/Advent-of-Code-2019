{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module IntCode where
import Data.Array.Base
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans

type Val = Integer
type Idx = Val
type Op = Val
type MonadIntCode m t = ( MonadTrans t
                        , MonadReader (() -> t m ()) (t m)
                        , MonadCont (t m)
                        , MonadInput m Int) 
type Intcode m t arr = (MArray arr Idx m, MonadIntCode m t)
newtype IntCode r m a = IntCode {runIntCode :: ReaderT (() -> IntCode r m ()) (ContT r m) a}
  deriving Functor
  deriving Applicative
  deriving Monad
  deriving (MonadReader (() -> IntCode r m ()))
  deriving MonadCont
instance MonadTrans (IntCode r) where lift m = IntCode (lift (lift m))

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
  
liftOp :: Intcode m t arr => (Val -> Val -> Val) -> arr Idx Val -> Idx -> Idx -> Idx -> t m ()
liftOp op arr dst src1 src2 =
  do x <- readArray arr src1
     y <- readArray arr src2
     writeArray arr dst (op x y)

noargs m _ _ _ _ = m
-- This is magical, when this is executed then
-- it jumps to the continuation point stored by callCC
-- this has the effect of skipping the rest of the code

exit :: MonadIntCode m t => t m ()
exit = join (asks ($ ()))

pattern Add  <- 1
pattern Mult <- 2
pattern In   <- 3
pattern Out  <- 4
pattern Off  <- 9
pattern Halt <- 99

pattern Pos <- 0
pattern Imm <- 1
pattern Rel <- 2

decode :: Intcode m t arr => Op -> (arr Idx Val -> Idx -> Idx -> Idx -> t m ())
decode Add  = liftOp (+)
decode Mult = liftOp (*)
decode Halt = noargs exit
decode _  = noargs (fail "Unknown opcode")

-- This will need remodelling for when the opcode has a varying length
fetch :: Intcode m t arr => arr Idx Val -> Idx -> t m (Idx, Op, Val, Val, Val)
fetch arr i =
    do opcode <- readArray arr i
       src1   <- readArray arr (i + 1)
       src2   <- readArray arr (i + 2)
       dest   <- readArray arr (i + 3)
       return (i + 4, opcode, src1, src2, dest)

initialise :: (Monad m, MArray arr Val m) => [Val] -> m (arr Idx Val)
initialise xs = newListArray (0, fromIntegral m) xs
  where n = length xs
        m = n + (4 - n `mod` 4)

execute :: (MonadInput m Int, MArray arr Val m) => arr Idx Val -> m Val--(arr Int Int)
execute arr = flip runContT return $
  -- callCC registers the readArray arr 0 as the place where code should return to
  -- when onExit is called, beautiful beautiful technique
  do callCC (\onExit -> runReaderT (runIntCode (exec 0)) (IntCode . lift . onExit))
     lift $ readArray arr 0
     --return arr
  where
    exec pc = do (pc', opcode, src1, src2, dest) <- fetch arr pc
                 decode opcode arr dest src1 src2
                 exec pc'

{-execIntCode :: [Int] -> IO Int
execIntCode = initialise @IO @IOArray >=> execute

test :: [Int] -> IO ()
test xs = execIntCode xs >>= freeze @Int @IOArray @Int @IO @Array >>= (\arr -> 
  let ys = elems arr
  in print (take (length xs) ys))-}
