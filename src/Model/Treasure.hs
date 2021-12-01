import System.Random

import Model.Maze


emptyCell :: [[Char]] -> [[Int]]
emptyCell maze  = [ [r, c] | c <- [0..(mazeDim-1)], ((maze !! r) !! c) == " " | r <- [0..(mazeDim-1)] ]


getRandomTreasure :: [[Int]] -> IO [Int]
getRandomTreasure cells = do
  i <- randomRIO (0, length cells - 1)
  return (cells !! i)