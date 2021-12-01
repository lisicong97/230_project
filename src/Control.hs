module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))

import Model.Player
import Model.Maze
<<<<<<< HEAD
import Model.Zombie
=======
import Data.Time.Clock
import Data.List (tails)
import System.Random (randomR)
>>>>>>> 570e7e169139b6533ddd08c52ade29d7454a0ab6

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent (Tick currentTime)     -> Brick.continue (zombieMove currentTime s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move Model.Maze.up s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move Model.Maze.down s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move Model.Maze.left s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move Model.Maze.right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

zombieMove :: UTCTime -> PlayState -> PlayState 
zombieMove t s = if (floor $ nominalDiffTimeToSeconds $ diffUTCTime t (time s)) >= 1
                  then move Model.Maze.down s { time = t}
                  else s

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
      
      

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = put (psBoard s) xo <$> getPos xo s
  | otherwise      = return Retry

getPos :: XO -> PlayState -> IO Pos
getPos xo s = getStrategy xo s (psPos s) (psBoard s) xo

getStrategy :: XO -> PlayState -> Strategy
getStrategy X s = plStrat (psX s)
getStrategy O s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res })


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



