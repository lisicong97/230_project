{-# LANGUAGE DeriveFunctor #-}
module Model.Maze
  (
    MazeCoord,
    startLoction,
    maze0,
    drawMazeWidget
  )
  where

import Brick

data MazeCoord = MkMazeCoord
  { row :: Int  -- 1 <= pRow <= dim 
  , col :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

startLoction :: MazeCoord
startLoction = MkMazeCoord 1 1

drawMazeWidget :: [[Char]] -> MazeCoord -> Widget n
drawMazeWidget maze (MkMazeCoord x y) =
    vBox [ hBox [ if (r == x) && (c == y) then str "*" else str [(maze !! r) !! c] | c <- [0..(mazeDim-1)]] |  r <- [0..(mazeDim-1)] ]

mazeDim :: Int
mazeDim = 27



maze0 :: [[Char]]
maze0 = [ "###########################"
        , "#........... #............#"
        , "#.####.#####.#.#####.####.#"
        , "#*#  # #   #.#.#   # #  #*#"
        , "#.####.#####.#.#####.####.#"
        , "#.........................#"
        , "#.####.#.#########.#.####.#"
        , "#......#.....#.....#......#"
        , "######.##### # #####.######"
        , "     #.#           #.#     "
        , "######.# ###---### #.######"
        , "      .  #       #  .      "
        , "######.# ######### #.######"
        , "     #.#           #.#     "
        , "######.# ######### #.######"
        , "#............#............#"
        , "#.####.#####.#.#####.####.#"
        , "#*...#...............#...*#"
        , "####.#.#.#########.#.#.####"
        , "#......#.....#.....#......#"
        , "#.##########.#.##########.#"
        , "#.........................#"
        , "###########################"
        ]