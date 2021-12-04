module Main where

import Brick
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Model
import View 
import Control 
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Random
import Data.Time.Clock

-------------------------------------------------------------------------------
main :: IO ()
main = do
  rounds <- fromMaybe defaultRounds <$> getRounds
  chan   <- newBChan 10
  forkIO  $ forever $ do
    t <- getCurrentTime
    writeBChan chan (Tick t)
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  seed <- getStdGen
  st <- getCurrentTime
  res <- customMain initialVty buildVty (Just chan) app (Model.init rounds seed st)
  print (psResult res, psScore res) 

app :: App PlayState Tick String
app = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

getRounds :: IO (Maybe Int)
getRounds = do
  args <- getArgs
  case args of
    (str:_) -> return (readMaybe str)
    _       -> return Nothing

defaultRounds :: Int
defaultRounds = 3

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [
     (playerAttr, V.red `on` V.red)
    ,(zombieAttr, V.blue `on` V.blue)
    ,(treasureAttr, V.green  `on` V.green)
  ]

data Ele = P | Z | T
  deriving (Eq, Show, Enum)

playerAttr, zombieAttr, treasureAttr :: AttrName
playerAttr = attrName "P"
zombieAttr = attrName "Z"
treasureAttr = attrName "T"

tToAttr :: Ele -> AttrName
tToAttr P = playerAttr
tToAttr Z = zombieAttr
tToAttr T = treasureAttr