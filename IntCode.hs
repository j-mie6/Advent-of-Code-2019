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

type MonadIntCode m t arr x = ( Ix x
                              , Num x
                              , MArray arr x m
                              , MonadTrans t
                              , MonadReader (() -> t m ()) (t m)
                              , MonadState (IntState arr x) (t m)
                              , MonadCont (t m)
                              , MonadInput m x)

pattern Add  <- 1
pattern Mult <- 2
pattern In   <- 3
pattern Out  <- 4
pattern Off  <- 9
pattern Halt <- 99

pattern Pos <- 0
pattern Imm <- 1
pattern Rel <- 2

data IntState arr x = IntState { pc :: x, mem :: arr x x }
newtype IntCode r arr x m a = IntCode {runIntCode :: StateT (IntState arr x) (ReaderT (() -> IntCode r arr x m ()) (ContT r m)) a}
  deriving Functor
  deriving Applicative
  deriving Monad
  deriving (MonadReader (() -> IntCode r arr x m ()))
  deriving (MonadState (IntState arr x))
  deriving MonadCont
instance MonadTrans (IntCode r arr x) where lift m = IntCode (lift (lift (lift m)))

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

-- This is magical, when this is executed then
-- it jumps to the continuation point stored by callCC
-- this has the effect of skipping the rest of the code
exit :: MonadReader (() -> m ()) m => m ()
exit = join (asks ($ ()))

incPc :: Num x => MonadState (IntState arr x) m => x -> m ()
incPc i = modify (\s -> s {pc = pc s + i})

data Fetched x = Fetched 
  { op :: x 
  , ops :: Ops x }
data Ops x = BinOp x x x
           | UnaryOp x
           | NoOps

size :: Num x => Ops x -> x
size (BinOp _ _ _) = 3
size (UnaryOp _) = 1
size NoOps = 0

fetch :: MonadIntCode m t arr x => t m (Fetched x)
fetch =
    do i <- gets pc
       arr <- gets mem
       opcode <- readArray arr i
       ops <- case opcode of
         Add  -> readBinary arr i
         Mult -> readBinary arr i
         In   -> readUnary arr i
         Out  -> readUnary arr i
         Off  -> readUnary arr i
         Halt -> return NoOps
       incPc (size ops + 1)
       return (Fetched opcode ops)
    where
      readUnary arr i   = UnaryOp <$> readArray arr (i + 1)
      readBinary arr i  = liftM3 BinOp (readArray arr (i + 1)) 
                                       (readArray arr (i + 2)) 
                                       (readArray arr (i + 3))

liftBin :: MonadIntCode m t arr x => (x -> x -> x) -> Ops x -> t m ()
liftBin op (BinOp src1 src2 dst) =
  do arr <- gets mem
     x <- readArray arr src1
     y <- readArray arr src2
     writeArray arr dst (op x y)

decode :: MonadIntCode m t arr x => Fetched x -> t m ()
decode (Fetched Add ops)         = liftBin (+) ops
decode (Fetched Mult ops)        = liftBin (*) ops
decode (Fetched In (UnaryOp x))  = undefined
decode (Fetched Out (UnaryOp x)) = undefined
decode (Fetched Off (UnaryOp x)) = undefined
decode (Fetched Halt NoOps)      = exit
decode _                         = fail "Unknown opcode"

initialise :: (MArray arr x m, Ix x, Num x, Monad m) => [x] -> m (arr x x)
initialise xs = newListArray (0, fromIntegral (length xs)) xs

execute :: (Ix x, Num x, MonadInput m x, MArray arr x m) => arr x x -> m x--(arr Int Int)
execute arr = flip runContT return $
  -- callCC registers the readArray arr 0 as the place where code should return to
  -- when onExit is called, beautiful beautiful technique
  do callCC (\onExit -> runReaderT (evalStateT (runIntCode exec) (IntState 0 arr)) (IntCode . lift . lift . onExit))
     readArray arr 0
     --return arr
  where
    exec = do fetched <- fetch
              decode fetched
              exec

{-execIntCode :: [Int] -> IO Int
execIntCode = initialise @IO @IOArray >=> execute

test :: [Int] -> IO ()
test xs = execIntCode xs >>= freeze @Int @IOArray @Int @IO @Array >>= (\arr -> 
  let ys = elems arr
  in print (take (length xs) ys))-}
