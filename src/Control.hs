module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Model.Player
import Model.Maze
import Data.Time.Clock

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
zombieMove t s = if floor ( nominalDiffTimeToSeconds $ diffUTCTime t (time s)) >= 1
                  then move Model.Maze.down s { time = t}
                  else s

-------------------------------------------------------------------------------
move :: (MazeCoord  -> [[Char ]] -> MazeCoord ) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s= 
  s { playerLoc = f (playerLoc s) maze0}

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


