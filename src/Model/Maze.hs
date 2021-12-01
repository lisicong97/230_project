

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

--------------------------------------------
-- up :: PlayState -> [String] -> PlayState
-- up ps maze tls = if maze !! (row (playerLoc ps) - 1) !! (col (playerLoc ps) - 1) == '#' then
--   ps
-- else
--   if (row (playerLoc ps) - 1) - 1 == tx1 && (col (playerLoc ps) - 1) == ty1 then
--     ps{score = score ps + 1
--       , playerLoc = MkMazeCoord (max 1 (row (playerLoc ps) - 1)) (col (playerLoc ps))
--       , treasureLocs = [loc1, treasureLocs !! 1]
--       , seed = seed2
--     }
--   else
--     if (row (playerLoc ps) - 1) - 1 == tx2 && (col (playerLoc ps) - 1) == ty2 then
--       ps{score = score ps + 1
--       , playerLoc = MkMazeCoord (max 1 (row (playerLoc ps) - 1)) (col (playerLoc ps))
--       , treasureLocs = [head treasureLocs, loc2]
--       , seed = seed3
--       }
--     else
--       ps{ playerLoc = MkMazeCoord (max 1 (row (playerLoc ps) - 1)) (col (playerLoc ps)) }
--     where
--       loc1 = allEmptyCells !! i1
--       (i1, seed2) = randomR (0, length allEmptyCells - 1) (seed ps)
--       allEmptyCells = Maze.emptyCell Maze.maze0  Maze.startLoction

--       loc2 = allEmptyCells !! i2
--       (i2, seed3) = randomR (0, length allEmptyCells - 1) (seed ps)
--       -- allEmptyCells = Maze.emptyCell Maze.maze0  Maze.startLoction

--       MkMazeCoord tx1 ty1 = tls !! 0
--       MkMazeCoord tx2 ty2 = tls !! 1






down ::MazeCoord  -> [String] -> MazeCoord
down p maze = if maze !! (row p + 1) !! col p == '#' then
  p
else
  p{ row = min mazeDim (row p + 1) }


left ::MazeCoord  -> [String] -> MazeCoord
left p maze = if maze !! row p !! (col p - 1) == '#' then
  p
else
  p{ col = max 1 (col p - 1) }

right ::MazeCoord  -> [String] -> MazeCoord
right p maze = if maze !! row p !! (col p + 1) == '#' then
  p
else
  p{ col = min mazeDim (col p + 1)}

startLoction :: MazeCoord
startLoction = MkMazeCoord 1 1

drawMazeWidget :: [String] -> MazeCoord -> [MazeCoord] -> Widget n
drawMazeWidget maze (MkMazeCoord x y) treasureLocs =
    vBox [ hBox [ if r == x && c == y
      then str "*"
      else (
        if r == tx1 && c == ty1 || r == tx2 && c == ty2
          then str "O"
          else str [maze !! r !! c]
      )
      | c <- [0..(mazeDim-1)]] |  r <- [0..(mazeDim-1)] ]
      where MkMazeCoord tx1 ty1 = head treasureLocs
            MkMazeCoord tx2 ty2 = treasureLocs !! 1



mazeDim :: Int
mazeDim = 27

emptyCell :: [String] -> MazeCoord -> [MazeCoord]
emptyCell maze (MkMazeCoord x y) = [ MkMazeCoord r c | c <- [0..(mazeDim-1)], r <- [0..(mazeDim-1)] , (maze !! r) !! c == ' ' ]

genLoc :: Int -> Int -> MazeCoord
genLoc = MkMazeCoord

getLocX :: MazeCoord -> Int
getLocX (MkMazeCoord x _) = x

getLocY :: MazeCoord -> Int
getLocY (MkMazeCoord _ y) = y

maze0 :: [String]
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