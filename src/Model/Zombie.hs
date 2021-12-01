{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ParallelListComp #-}
module Model.Zombie
  (
    initZombies,
    getZombieLocs,
  )
  where

import System.Random
import qualified Model.Maze as Maze



zombieNum :: Int
zombieNum = 4



getRandomZombie :: [[Int]] -> IO [Int]
getRandomZombie cells = do
  i <- randomRIO (0, length cells - 1)
  return (cells !! i)

initZombies :: StdGen -> (StdGen, [Maze.MazeCoord])
initZombies seed = getZombieLocs seed []

getZombieLocs :: StdGen -> [Maze.MazeCoord] -> (StdGen, [Maze.MazeCoord])
getZombieLocs seed coords = if length coords == zombieNum then
                                (seed, coords)
                            else
                                getZombieLocs seed' (loc:coords)
                            where
                                loc = allEmptyCells !! i
                                (i, seed') = randomR (0, length allEmptyCells - 1) seed
                                allEmptyCells = Maze.emptyCell Maze.maze0  Maze.startLoction