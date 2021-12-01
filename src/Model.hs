{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import qualified Model.Maze as Maze
import qualified Model.Zombie as Zombie
import System.Random (StdGen)
import Model.Maze (MazeCoord)
import System.Random
import Data.Time.Clock


-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
-- data Tick = Tick
data Tick = Tick UTCTime

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
  , score :: Int
  , zombieLocs :: [Maze.MazeCoord]
  , zombieDirects :: [Int] -- ^ 0=up, 1=down, 2=left, 3=right

  , time :: UTCTime
  } 

init :: Int -> StdGen -> UTCTime -> PlayState
init n seed1 t = PS 
  { psX      = Player.human
  , psO      = Player.rando
  , psScore  = Score.init n
  , psBoard  = Board.init
  , psTurn   = Board.X
  , psPos    = head Board.positions 
  , psResult = Board.Cont ()

  , seed       = seed5
  , maze      = Maze.maze0
  , playerLoc = Maze.startLoction
  , treasureLocs = [loc1, loc2]
  , score = 0
  , zombieLocs = zombies
  , zombieDirects = dirs

  , time = t
  }
  where 
        (seed5, dirs) = Zombie.initDirects seed4 zombies 
        (seed4, zombies) = Zombie.initZombies seed3
        loc1 = allEmptyCells !! i1
        loc2 = allEmptyCells !! i2
        (i1, seed2) = randomR (0, length allEmptyCells - 1) seed1
        (i2, seed3) = randomR (0, length allEmptyCells - 1) seed2
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

