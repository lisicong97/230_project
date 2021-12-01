module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
import Model.Maze
import Model.Zombie

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (play X s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move Model.Maze.up s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move Model.Maze.down s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move Model.Maze.left s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move Model.Maze.right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

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



