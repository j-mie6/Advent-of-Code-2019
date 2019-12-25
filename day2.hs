{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
import Data.Array.IO
import Data.Array.IArray
import Data.Array.MArray
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.List
import Control.Monad.Trans

type MonadIntCode m t = ( Monad m
                        , MonadTrans t
                        , Monad (t m)
                        , MonadReader (() -> t m ()) (t m)
                        , MonadCont (t m)) 
type Intcode m t arr = (MArray arr Int m, MonadIntCode m t)
newtype IntCode r m a = IntCode {runIntCode :: ReaderT (() -> IntCode r m ()) (ContT r m) a}
  deriving Functor
  deriving Applicative
  deriving Monad
  deriving (MonadReader (() -> IntCode r m ()))
  deriving MonadCont
instance MonadTrans (IntCode r) where lift m = IntCode (lift (lift m))

{-instance Monad m 
      => MonadTrans t 
      => Monad (t m)
      => MArray arr e m
      => MArray arr e (t m) where
  getBounds arr = lift (getBounds arr)
  getNumElements arr = lift (getNumElements arr)
  newArray bnds x = lift (newArray bnds x)
  unsafeRead arr idx = lift (unsafeRead arr idx)
  unsafeWrite arr idx x = lift (unsafeWrite arr idx x)-}
  
liftOp :: Intcode m t arr => (Int -> Int -> Int) -> arr Int Int -> Int -> Int -> Int -> t m ()
liftOp op arr dst src1 src2 =
  do x <- lift $ readArray arr src1
     y <- lift $ readArray arr src2
     lift $ writeArray arr dst (op x y)

noargs m _ _ _ _ = m
-- This is magical, when this is executed then
-- it jumps to the continuation point stored by callCC
-- this has the effect of skipping the rest of the code

exit :: MonadIntCode m t => t m ()
exit = join (asks ($ ()))

decode :: Intcode m t arr => Int -> (arr Int Int -> Int -> Int -> Int -> t m ())
decode 1  = liftOp (+)
decode 2  = liftOp (*)
decode 99 = noargs exit
decode _  = noargs (fail "Unknown opcode")

-- This will need remodelling for when the opcode has a varying length
fetch :: Intcode m t arr => arr Int Int -> Int -> t m (Int, Int, Int, Int, Int)
fetch arr i =
    do opcode <- lift $ readArray arr i
       src1   <- lift $ readArray arr (i + 1)
       src2   <- lift $ readArray arr (i + 2)
       dest   <- lift $ readArray arr (i + 3)
       return (i + 4, opcode, src1, src2, dest)

initialise :: (Monad m, MArray arr Int m) => [Int] -> m (arr Int Int)
initialise xs = newListArray (0, m) xs
  where n = length xs
        m = n + (4 - n `mod` 4)

execute :: (Monad m, MArray arr Int m) => arr Int Int -> m Int--(arr Int Int)
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

runWithValues :: Int -> Int -> IO Int
runWithValues x y =
  do xs <- input
     arr <- initialise @IO @IOArray xs
     writeArray arr 1 x
     writeArray arr 2 y
     execute arr

task1 :: IO Int
task1 = runWithValues 12 2

-- The ListT monad, though usually dodgy, works well here
-- It does not, however, short circuit
-- We non-deterministically choose two numbers between 0 to 99
-- We run this program with those values
task2 :: IO [(Int, Int)]
task2 = runListT $
  do let val = 19690720
     x <- ListT (return [0..99])
     y <- ListT (return [0..99])
     z <- lift $ runWithValues x y
     guard (z == val)
     return (x, y)

split :: (a -> Bool) -> [a] -> [[a]]
split f = foldr g [[]]
  where
    g x (ys:yss) 
      | f x  = []:ys:yss
      | otherwise = (x:ys):yss

input :: IO [Int]
input = map read . split (== ',') <$> readFile "day2-input.txt"

{-execIntCode :: [Int] -> IO Int
execIntCode = initialise @IO @IOArray >=> execute

test :: [Int] -> IO ()
test xs = execIntCode xs >>= freeze @Int @IOArray @Int @IO @Array >>= (\arr -> 
  let ys = elems arr
  in print (take (length xs) ys))-}
