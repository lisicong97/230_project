module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Maze
import Model.Zombie
import Data.Time.Clock
import System.Random (randomR)
-------------------------------------------------------------------------------

-- control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
-- control s ev = case ev of 
--   AppEvent (Tick currentTime)     -> Brick.continue (autoMove currentTime s)
--   T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move Model.Maze.up s)
--   T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move Model.Maze.down s)
--   T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move Model.Maze.left s)
--   T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move Model.Maze.right s)
--   T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
--   _                               -> Brick.continue s -- Brick.halt s

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = if (gameState s) == 0 then
  case ev of 
    AppEvent (Tick currentTime)     -> Brick.continue (autoMove currentTime s)
    T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move Model.Maze.up s)
    T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move Model.Maze.down s)
    T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move Model.Maze.left s)
    T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move Model.Maze.right s)
    T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
    _                               -> Brick.continue s -- Brick.halt s
  else
    case ev of
      T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
      _                               -> Brick.continue s -- Brick.halt s


-- autoMove :: UTCTime -> PlayState -> PlayState 
-- autoMove t s = if floor ( nominalDiffTimeToSeconds $ diffUTCTime t (time s)) >= 1
--                   then s { time = t, seed = seed1, zombieLocs = newZombieLocs, zombieDirects = newZombieDirects}
--                   else s
--                   where 
--                     (seed1, newZombieDirects) = updateDirects (seed s) (zombieLocs s) (zombieDirects s) []
--                     newZombieLocs = zombiesMove newZombieDirects (zombieLocs s) []

autoMove :: UTCTime -> PlayState -> PlayState 
autoMove t s = if floor ( nominalDiffTimeToSeconds $ diffUTCTime t (time s)) >= 1
                  then 
                    if compareMeet (playerLoc s) newZombieLocs
                      then s { time = t, seed = seed1, zombieLocs = newZombieLocs, zombieDirects = newZombieDirects, gameState = 1}
                    else
                      s { time = t, seed = seed1, zombieLocs = newZombieLocs, zombieDirects = newZombieDirects}
                  else s
                  where 
                    (seed1, newZombieDirects) = updateDirects (seed s) (zombieLocs s) (zombieDirects s) []
                    newZombieLocs = zombiesMove newZombieDirects (zombieLocs s) []

-------------------------------------------------------------------------------
move :: (MazeCoord -> [[Char]] -> MazeCoord ) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s
  | getLocX (playerLoc ps) == tx1 && getLocY (playerLoc ps) == ty1 =
  ps {
    score = score ps + 1
  , treasureLocs = [loc1, (treasureLocs ps) !! 1]
  , seed = seed2
  }
  | compareMeet (playerLoc ps) (zombieLocs ps)=
  ps {
      gameState = 1
  }
  | getLocX (playerLoc ps) == tx2 && getLocY (playerLoc ps) == ty2 =
  ps {
    score = score ps + 1
  , treasureLocs = [(treasureLocs ps) !! 0, loc2]
  , seed = seed3
  }
  | otherwise =
  ps
  where
      ps = s{playerLoc = f (playerLoc s) maze0}
      allEmptyCells = emptyCell maze0 startLoction
      loc1 = allEmptyCells !! i1
      (i1, seed2) = randomR (0, length allEmptyCells - 1) (seed ps)
      loc2 = allEmptyCells !! i2
      (i2, seed3) = randomR (0, length allEmptyCells - 1) (seed ps)
      tx1 = getLocX (head (treasureLocs ps))
      ty1 = getLocY (head (treasureLocs ps))
      tx2 = getLocX ((treasureLocs ps) !! 1)
      ty2 = getLocY ((treasureLocs ps) !! 1)
      


compareMeet :: MazeCoord -> [MazeCoord] -> Bool
compareMeet _ [] = False
compareMeet p (x:xs) = (getLocX p == getLocX x && getLocY p == getLocY x)
                      || compareMeet p xs


-- -------------------------------------------------------------------------------
-- zombieSingleMove :: Int -> (MazeCoord  -> [[Char ]] -> MazeCoord ) -> PlayState -> PlayState
-- -------------------------------------------------------------------------------
-- zombieSingleMove id f s = 
--   s { zombieLocs = modifySingleLoc f (zombieLocs s) [] id}


-- modifySingleLoc :: (MazeCoord  -> [[Char ]] -> MazeCoord ) -> [MazeCoord] -> [MazeCoord] -> Int -> [MazeCoord]
-- modifySingleLoc _ [] cur _ = cur
-- modifySingleLoc f (c : cs) cur id = if length cur == id 
--                                       then (cur' ++ cs)
--                                       where
--                                         cur' = cur ++ c'
--                                         c' = f c maze0
--                                     else
--                                       modifySingleLoc f cs (cur++c) id



