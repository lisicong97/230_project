{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import qualified Model.Maze as Maze
import System.Random (StdGen)
import Model.Maze (MazeCoord)
import System.Random


-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psX      :: Player.Player   -- ^ player X info
  , psO      :: Player.Player   -- ^ player O info
  , psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.XO        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result      

  , seed      :: StdGen
  , maze      :: [[Char]]
  , playerLoc :: Maze.MazeCoord -- ^ current player location
  , treasureLocs :: [Maze.MazeCoord]
  } 

init :: Int -> StdGen -> PlayState
init n seed = PS 
  { psX      = Player.human
  , psO      = Player.rando
  , psScore  = Score.init n
  , psBoard  = Board.init
  , psTurn   = Board.X
  , psPos    = head Board.positions 
  , psResult = Board.Cont ()

  , seed       = seed
  , maze      = Maze.maze0
  , playerLoc = Maze.startLoction
  , treasureLocs = [loc1, loc2]
  }
  where 
        loc1 = allEmptyCells !! i1
        loc2 = allEmptyCells !! i2
        (i1, newSeed) = randomR (0, length allEmptyCells - 1) seed
        (i2, _) = randomR (0, length allEmptyCells - 1) newSeed
        allEmptyCells = Maze.emptyCell Maze.maze0  Maze.startLoction


isCurr :: PlayState -> Int -> Int -> Bool
-- isCurr s r c = Board.pRow p == r && Board.pCol p == c
--   where 
--     p = psPos s 
isCurr _ _ _ = True

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'
                                  , psTurn  = Board.flipXO (psTurn s) })
next s res             = nextBoard s res 

nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
nextBoard s res = case res' of
                    Board.Win _ -> Left res' 
                    Board.Draw  -> Left res'
                    _           -> Right s' 
  where 
    sc'  = Score.add (psScore s) (Board.boardWinner res) 
    res' = Score.winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = mempty                -- clear the board
             , psTurn  = Score.startPlayer sc' -- toggle start player
             } 

