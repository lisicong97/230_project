{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ParallelListComp #-}
module Model.Zombie
  (
    initZombies,
    getZombieLocs,
    initDirects,
    updateDirects,
    zombiesMove,
  )
  where

import System.Random
import qualified Model.Maze as Maze



zombieNum :: Int
zombieNum = 8



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


zombiesMove :: [Int] -> [Maze.MazeCoord] -> [Maze.MazeCoord] -> [Maze.MazeCoord]
zombiesMove [] _ coords' = coords'
zombiesMove _ [] coords' = coords'
zombiesMove (d : ds) (c :cs) coords' = zombiesMove ds cs (coords' ++ [c'])
                                        where
                                            c' = zombieMove d c Maze.maze0

zombieMove :: Int -> Maze.MazeCoord -> [[Char ]] -> Maze.MazeCoord
zombieMove dir coord maze =  if dir == 0 then
                                Maze.zombieUp coord maze
                            else if dir == 1 then
                                Maze.zombieDown coord maze
                            else if dir == 2 then
                                Maze.zombieLeft coord maze
                            else if dir == 3 then
                                Maze.zombieRight coord maze
                            else
                                Maze.zombieUp coord maze


initDirects :: StdGen -> [Maze.MazeCoord] -> (StdGen, [Int])
initDirects seed coords = updateDirects seed coords [0, 0, 0, 0] []

updateDirects :: StdGen -> [Maze.MazeCoord] -> [Int] -> [Int] -> (StdGen, [Int])
updateDirects seed [] _ dircts' = (seed, dircts')
updateDirects seed _ [] dircts' = (seed, dircts')
updateDirects seed (c : cs) (d : ds) dirs = updateDirects seed' cs ds (dirs++[d'])
                                            where
                                                (seed', d') = changeDir seed c d




changeDir :: StdGen -> Maze.MazeCoord -> Int -> (StdGen, Int)
changeDir seed coord 0 = if ((Maze.getLocX coord) <= 1) && (Maze.maze0 !! (Maze.getLocX coord - 1) !! Maze.getLocY coord == '#') then changeDir seed' coord dir else (seed, 0)
                            where
                                (dir,seed') = randomR (0, 3) seed
changeDir seed coord 1 = if ((Maze.getLocX coord) >= Maze.mazeDim-1) && (Maze.maze0 !! (Maze.getLocX coord + 1) !! Maze.getLocY coord == '#') then changeDir seed' coord dir else (seed, 1)
                            where
                                (dir,seed') = randomR (0, 3) seed
changeDir seed coord 2 = if ((Maze.getLocY coord) <= 1)  && (Maze.maze0 !! Maze.getLocX coord !! (Maze.getLocY coord - 1) == '#') then changeDir seed' coord dir else (seed, 2)
                            where
                                (dir,seed') = randomR (0, 3) seed
changeDir seed coord 3 = if ((Maze.getLocY coord) >= Maze.mazeDim-1) && (Maze.maze0 !! Maze.getLocX coord !! (Maze.getLocY coord + 1) == '#') then changeDir seed' coord dir else (seed, 3)
                            where
                                (dir,seed') = randomR (0, 3) seed
changeDir seed coord _ = changeDir seed coord 0