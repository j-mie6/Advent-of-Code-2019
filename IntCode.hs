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
                              , MonadReader (arr x x -> t m ()) (t m)
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
newtype IntCode r arr x m a = IntCode {runIntCode :: StateT (IntState arr x) (ReaderT (arr x x -> IntCode r arr x m ()) (ContT r m)) a}
  deriving Functor
  deriving Applicative
  deriving Monad
  deriving (MonadReader (arr x x -> IntCode r arr x m ()))
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
exit :: (MonadState (IntState arr x) m , MonadReader (arr x x -> m ()) m) => m ()
exit = do arr <- gets mem
          join (asks ($ arr))

incPc :: (Num x, MonadState (IntState arr x) m) => x -> m ()
incPc i = modify (\s -> s {pc = pc s + i})

readMem :: MonadIntCode m t arr x => x -> t m x
readMem idx = do arr <- gets mem
                 readArray arr idx

writeMem :: MonadIntCode m t arr x => x -> x -> t m ()
writeMem idx x = do arr <- gets mem
                    writeArray arr idx x

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
       opcode <- readMem i
       ops <- case opcode of
         Add  -> readBinary i
         Mult -> readBinary i
         In   -> readUnary i
         Out  -> readUnary i
         Off  -> readUnary i
         Halt -> return NoOps
         _    -> fail "Unknown opcode"
       incPc (size ops + 1)
       return (Fetched opcode ops)
    where
      readUnary i   = UnaryOp <$> readMem (i + 1)
      readBinary i  = liftM3 BinOp (readMem (i + 1)) 
                                   (readMem (i + 2)) 
                                   (readMem (i + 3))

liftBin :: MonadIntCode m t arr x => (x -> x -> x) -> Ops x -> t m ()
liftBin op (BinOp src1 src2 dst) = 
  liftM2 op (readMem src1) (readMem src2) >>= writeMem dst

decode :: MonadIntCode m t arr x => Fetched x -> t m ()
decode (Fetched Add ops)           = liftBin (+) ops
decode (Fetched Mult ops)          = liftBin (*) ops
decode (Fetched In (UnaryOp dst))  = do x <- input
                                        writeMem dst x
decode (Fetched Out (UnaryOp src)) = do x <- readMem src
                                        output x
--decode (Fetched Off (UnaryOp x))   = undefined
decode (Fetched Halt NoOps)        = exit
decode _                           = fail "Unknown opcode"

initialise :: (MArray arr x m, Ix x, Num x, Monad m) => [x] -> m (arr x x)
initialise xs = newListArray (0, fromIntegral (length xs)) xs

execute :: (Ix x, Num x, MonadInput m x, MArray arr x m) => arr x x -> m x--(arr Int Int)
execute arr = flip runContT return $
  -- callCC registers the readArray arr 0 as the place where code should return to
  -- when onExit is called, beautiful beautiful technique
  do arr' <- callCC (\onExit -> 
       flip runReaderT (IntCode . lift . lift . onExit) $
         flip evalStateT (IntState 0 arr) $
           runIntCode exec)
     readArray arr' 0
     --return arr'
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
