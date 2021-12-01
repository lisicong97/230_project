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
    emptyCell,
    genLoc,
    getLocX,
    getLocY
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

drawMazeWidget :: [[Char]] -> MazeCoord -> [MazeCoord] -> [MazeCoord] -> Widget n
drawMazeWidget maze (MkMazeCoord x y) treasureLocs zombieLocs =
    vBox [ hBox [ if (r == x) && (c == y)
      then str "*"
      else (
        if judgeExistThing zombieLocs r c
          then str "&"
          else (
            if (r == tx1) && (c == ty1) || (r == tx2) && (c == ty2)
            then str "O"
            else str [(maze !! r) !! c]
            )
      )
      | c <- [0..(mazeDim-1)]] |  r <- [0..(mazeDim-1)] ]
      where MkMazeCoord tx1 ty1 = treasureLocs !! 0
            MkMazeCoord tx2 ty2 = treasureLocs !! 1

judgeExistThing :: [MazeCoord] -> Int -> Int -> Bool
judgeExistThing [] _ _ = False
judgeExistThing ((MkMazeCoord x y) : coords) r c = if (r==x) && (c==y) then
                                                        True
                                                    else
                                                        judgeExistThing coords r c

mazeDim :: Int
mazeDim = 27

emptyCell :: [[Char]] -> MazeCoord -> [MazeCoord]
emptyCell maze (MkMazeCoord x y) = [ MkMazeCoord r c | c <- [0..(mazeDim-1)], r <- [0..(mazeDim-1)] , ((maze !! r) !! c) == ' ' ]

genLoc :: Int -> Int -> MazeCoord
genLoc x y = MkMazeCoord x y

getLocX :: MazeCoord -> Int
getLocX (MkMazeCoord x _) = x

getLocY :: MazeCoord -> Int
getLocY (MkMazeCoord _ y) = y

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