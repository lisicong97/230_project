{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude
import qualified Model.Maze as Maze
import qualified Model.Zombie as Zombie
import System.Random
import Data.Time.Clock


-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
-- data Tick = Tick
data Tick = Tick UTCTime

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  {     
    seed      :: StdGen
  , maze      :: [[Char]]
  , playerLoc :: Maze.MazeCoord -- ^ current player location
  , treasureLocs :: [Maze.MazeCoord]
  , score :: Int
  , zombieLocs :: [Maze.MazeCoord]
  , zombieDirects :: [Int] -- ^ 0=up, 1=down, 2=left, 3=right

  , time :: UTCTime

  , gameState :: Int -- ^ 0=game continue, 1=game over
  } 

init :: Int -> StdGen -> UTCTime -> PlayState
init _ seed1 t = PS 
  { 
    seed       = seed5
  , maze      = Maze.maze0
  , playerLoc = Maze.startLoction
  , treasureLocs = [loc1, loc2]
  , score = 0
  , zombieLocs = zombies
  , zombieDirects = dirs

  , time = t
  
  , gameState = 0
  }
  where 
        (seed5, dirs) = Zombie.initDirects seed4 zombies 
        (seed4, zombies) = Zombie.initZombies seed3
        loc1 = allEmptyCells !! i1
        loc2 = allEmptyCells !! i2
        (i1, seed2) = randomR (0, length allEmptyCells - 1) seed1
        (i2, seed3) = randomR (0, length allEmptyCells - 1) seed2
        allEmptyCells = Maze.emptyCell Maze.maze0  Maze.startLoction


isCurr :: PlayState -> Int -> Int -> Bool
isCurr _ _ _ = True

