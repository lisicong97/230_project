{-# LANGUAGE DeriveFunctor #-}
module Model.Maze
  (
    MazeCoord,
    startLoction,
    maze0,
    drawMazeWidget,
    up,
    down,
    left, 
    right
  )
  where

import Brick

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

drawMazeWidget :: [[Char]] -> MazeCoord -> Widget n
drawMazeWidget maze (MkMazeCoord x y) =
    vBox [ hBox [ if (r == x) && (c == y) then str "*" else str [(maze !! r) !! c] | c <- [0..(mazeDim-1)]] |  r <- [0..(mazeDim-1)] ]

mazeDim :: Int
mazeDim = 27



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