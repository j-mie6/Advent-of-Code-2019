{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module IntCode where
import Data.Ix
import Data.Array.Base
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

type MonadIntCode m t arr x = ( Ix x
                              , Integral x
                              , MArray arr x m
                              , MonadTrans t
                              , MonadReader (arr x x -> t m ()) (t m)
                              , MonadState (IntState arr x) (t m)
                              , MonadCont (t m)
                              , MonadInput m x)

data IntState arr x = IntState { pc :: x, mem :: arr x x, rel :: x }
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
readMem idx = do ensureMemSize idx
                 arr <- gets mem
                 readArray arr idx

writeMem :: MonadIntCode m t arr x => x -> x -> t m ()
writeMem idx x = do ensureMemSize idx
                    arr <- gets mem
                    writeArray arr idx x

ensureMemSize :: MonadIntCode m t arr x => x -> t m ()
ensureMemSize idx = 
  do arr <- gets mem
     upper <- snd <$> getBounds arr
     if idx > upper 
       then do arr' <- newArray (0, idx) 0
               let copy i
                     | i <= upper = 
                        do readArray arr i >>= writeArray arr' i
                           copy (i + 1)
                     | otherwise = return ()
               copy 0
               return arr'
               modify (\s -> s {mem = arr'})
       else return ()

data Fetched x = Fetched 
  { op :: Op 
  , ops :: Ops x }
data Ops x = BinOp (Operand x) (Operand x) (Operand x)
           | UnaryOp (Operand x)
           | NoOps deriving Show
data Operand x = Operand { mode :: Mode, val :: x } deriving Show
data Op = Add | Mult | Halt | In | Out | Off deriving Show
data Mode = Pos | Imm | Rel deriving Show

readByMode :: MonadIntCode m t arr x => Operand x -> t m x
readByMode (Operand Pos x) = readMem x
readByMode (Operand Imm x) = return x
readByMode (Operand Rel x) = gets ((+ x) . rel) >>= readMem

writeByMode :: MonadIntCode m t arr x => Operand x -> x -> t m ()
writeByMode (Operand Pos x) y = writeMem x y
writeByMode (Operand Imm _) _ = error "Cannot write to immediate value"
writeByMode (Operand Rel x) y = gets ((+ x) . rel) >>= flip writeMem y

size :: Num x => Ops x -> x
size (BinOp _ _ _) = 3
size (UnaryOp _) = 1
size NoOps = 0

modeOf :: (Eq x, Num x) => x -> Mode
modeOf 0 = Pos
modeOf 1 = Imm
modeOf 2 = Rel
modeOf _ = error "Invalid address mode"

opOf :: (Eq x, Num x) => x -> Op
opOf 1  = Add
opOf 2  = Mult
opOf 3  = In
opOf 4  = Out
opOf 9  = Off
opOf 99 = Halt
opOf _  = error "Invalid opcode"

extract :: Integral x => x -> (Op, [Mode])
extract x = (opOf (x `mod` 100), modes (x `div` 100))
  where
    modes 0 = repeat Pos
    modes x = modeOf (x `mod` 10) : modes (x `div` 10)

fetch :: MonadIntCode m t arr x => t m (Fetched x)
fetch =
    do i <- gets pc
       (opcode, modes) <- extract <$> readMem i
       ops <- case opcode of
         Add  -> readBinary i modes
         Mult -> readBinary i modes
         In   -> readUnary i modes
         Out  -> readUnary i modes
         Off  -> readUnary i modes
         Halt -> return NoOps
       incPc (size ops + 1)
       return (Fetched opcode ops)
    where
      readUnary i  (m1:_) = UnaryOp . Operand m1 <$> readMem (i + 1)
      readBinary i (m1:m2:m3:_) = 
        liftM3 BinOp (Operand m1 <$> readMem (i + 1)) 
                     (Operand m2 <$> readMem (i + 2)) 
                     (Operand m3 <$> readMem (i + 3))

liftBin :: MonadIntCode m t arr x => (x -> x -> x) -> Ops x -> t m ()
liftBin op (BinOp src1 src2 dst) = 
    liftM2 op (readByMode src1) 
              (readByMode src2) 
  >>= writeByMode dst

decode :: MonadIntCode m t arr x => Fetched x -> t m ()
decode (Fetched Add ops)           = liftBin (+) ops
decode (Fetched Mult ops)          = liftBin (*) ops
decode (Fetched In (UnaryOp dst))  = do x <- input
                                        writeByMode dst x
decode (Fetched Out (UnaryOp src)) = do x <- readByMode src
                                        output x
decode (Fetched Off (UnaryOp src)) = do x <- readByMode src
                                        modify (\s -> s {rel = rel s + x})
decode (Fetched Halt NoOps)        = exit

initialise :: (MArray arr x m, Ix x, Num x, Monad m) => [x] -> m (arr x x)
initialise xs = newListArray (0, fromIntegral (length xs - 1)) xs

execute :: (Ix x, Integral x, MonadInput m x, MArray arr x m) => arr x x -> m x--(arr Int Int)
execute arr = flip runContT return $
  -- callCC registers the readArray arr' 0 as the place where code should return to
  -- when onExit is called, beautiful beautiful technique
  do arr' <- callCC (\onExit -> 
       flip runReaderT (IntCode . lift . lift . onExit) $
         flip evalStateT (IntState 0 arr 0) $
           runIntCode exec)
     readArray arr' 0
     --return arr'
  where
    exec = do fetch >>= decode; exec

{-execIntCode :: [Int] -> IO Int
execIntCode = initialise @IO @IOArray >=> execute

test :: [Int] -> IO ()
test xs = execIntCode xs >>= freeze @Int @IOArray @Int @IO @Array >>= (\arr -> 
  let ys = elems arr
  in print (take (length xs) ys))-}
