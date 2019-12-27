{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
import Data.Array.ST
import Control.Monad.ST
import Control.Monad.State
import IntCode hiding (size)
import Pipe
import Data.Map
import Prelude hiding (lookup)

instance Monad m => MonadInput (PipeT a m) a where
  input = pipeIn
  output = pipeOut

mkProgram :: [Int] -> PipeT Int (ST s) (STArray s Int Int)
mkProgram = initialise

hullPainter :: [Int] -> PipeT Int (ST s) Int
hullPainter = mkProgram >=> execute

type Robot a = PipeT Int (State (Pos, Dir, Map Pos Colour)) a
type Pos = (Int, Int)
data Dir = North | East | South | West deriving (Enum, Show, Eq)
data Colour = Black | White deriving (Enum, Show, Eq)

connect :: (forall s. PipeT Int (ST s) Int) -> Robot () -> Map Pos Colour
connect control robot =
  let (_, controlOut) = runST (runPipeT control robotOut)
      (m, robotOut)   = runRobot robot (controlOut ++ [2])
  in m

{-foo :: PipeT Int (ST s) Int
foo = 
  do x <- pipeIn
     if x == 0 then pipeOut 1
     else pipeOut 0
     pipeOut 1
     return 0-}

hullPaintingRobot :: Robot ()
hullPaintingRobot = 
  do c <- lookRobot
     pipeOut (fromEnum c)
     c' <- pipeIn
     if c' == 2 then return ()
     else do paintRobot (toEnum c')
             d <- pipeIn
             turnRobot (d == 1)
             moveRobot
             hullPaintingRobot

task1 :: IO Int
task1 =
  do xs <- Main.input
     let m = connect (hullPainter xs) hullPaintingRobot
     print m
     return (size m)

-- Helpers
rot :: Int -> Dir -> Dir
rot d = toEnum . (`mod` 4) . (+ d). fromEnum

rotRight :: Dir -> Dir
rotRight = rot 1

rotLeft :: Dir -> Dir
rotLeft = rot (-1)

move :: Pos -> Int -> Dir -> Pos
move (x, y) n North = (x, y + n)
move (x, y) n East  = (x + n, y)
move (x, y) n South = (x, y - n)
move (x, y) n West  = (x - n, y)

pos :: Robot Pos
pos = lift (gets (\(x, _, _) -> x))

painted :: Robot (Map Pos Colour)
painted = lift (gets (\(_, _, x) -> x))

runRobot :: Robot () -> [Int] -> (Map Pos Colour, [Int])
runRobot r is = 
  let (((), os), (_, _, m)) = runState (runPipeT r is) ((0, 0), North, empty) 
  in (m, os)

turnRobot :: Bool -> Robot ()
turnRobot right = lift (modify (\(p, d, m) -> (p, (if right then rotRight else rotLeft) d, m)))

moveRobot :: Robot ()
moveRobot = lift (modify (\(p, d, m) -> (move p 1 d, d, m)))

paintRobot :: Colour -> Robot ()
paintRobot col = lift (modify (\(p, d, m) -> (p, d, insert p col m)))

lookRobot :: Robot Colour
lookRobot =
  do p <- pos
     m <- painted
     return (maybe Black id (lookup p m))

input :: IO [Int]
input = readProgram "day11-input.txt"
