{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ParallelListComp #-}
module Model.Maze
  (
    MazeCoord,
    startLoction,
    maze0,
    drawMazeWidget,
    up,
    down,
    left,
    right,
    mazeDim,
    Treasure,
    emptyCell,
  )
  where

import Brick
import System.Random

data MazeCoord = MkMazeCoord
  { row :: Int  -- 1 <= pRow <= dim 
  , col :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)


up :: MazeCoord  -> [[Char ]] -> MazeCoord 
up p maze = if maze !! (row p - 1) !! col p == '#' then
  p
else
  p{ row = max 1 (row p - 1) } 

down ::MazeCoord  -> [[Char ]] -> MazeCoord
down p maze = if maze !! (row p + 1) !! col p == '#' then
  p
else 
  p{ row = min mazeDim (row p + 1) }


left ::MazeCoord  -> [[Char ]] -> MazeCoord
left p maze = if maze !! row p !! (col p - 1) == '#' then
  p
else
  p{ col = max 1 (col p - 1) }

right ::MazeCoord  -> [[Char ]] -> MazeCoord
right p maze = if maze !! row p !! (col p + 1) == '#' then
  p
else
  p{ col = min mazeDim (col p + 1)}

startLoction :: MazeCoord
startLoction = MkMazeCoord 1 1

drawMazeWidget :: [[Char]] -> MazeCoord -> StdGen -> Widget n
drawMazeWidget maze (MkMazeCoord x y) seed =
    vBox [ hBox [ if (r == x) && (c == y)
      then str "*"
      -- else str [(maze !! r) !! c] 
      else (
        if (r == tx) && (c == ty)
          then str "O"
          else str [(maze !! r) !! c]
      )
      | c <- [0..(mazeDim-1)]] |  r <- [0..(mazeDim-1)] ]
      where MkMazeCoord tx ty = allEmptyCells !! i
            (i, _) = randomR (0, length allEmptyCells - 1) seed
            allEmptyCells = emptyCell maze  (MkMazeCoord x y)



mazeDim :: Int
mazeDim = 27

type Treasure = [[Char]]     -- ^ maze
              -> MazeCoord   -- ^ player location
              -> IO MazeCoord  -- ^ treasure location]

emptyCell :: [[Char]] -> MazeCoord -> [MazeCoord]
emptyCell maze (MkMazeCoord x y) = [ MkMazeCoord r c | c <- [0..(mazeDim-1)], r <- [0..(mazeDim-1)] , ((maze !! r) !! c) == ' ' ]



maze0 :: [[Char]]
maze0 = [ "###########################"
        , "#            #            #"
        , "# #### ##### # ##### #### #"
        , "#    # #     # #        # #"
        , "#  ### ### # # ## ####### #"
        , "#                         #"
        , "## ### # ######### # #### #"
        , "#      #           #      #"
        , "###### ##### # ##### ######"
        , "#                         #"
        , "###### # ###   ### # ######"
        , "#        #                #"
        , "###### # ######### ########"
        , "#      #           #      #"
        , "###### # ######### # ######"
        , "#            #            #"
        , "# #### ##### # ##### #### #"
        , "#    #               #    #"
        , "#### # # ######### # # ####"
        , "#      #     #     #      #"
        , "# ########## # ########## #"
        , "#    #               #    #"
        , "#### # ########### # # ####"
        , "#      #     #     #      #"
        , "# ########## # ###### #####"
        , "#                         #"
        , "###########################"
        ]