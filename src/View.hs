module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board
import Model.Maze
import Model.Zombie
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s =
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      -- vTile [ mkRow s row | row <- [1..dim] ]
      if (gameState s) == 0 then
        drawMazeWidget maze0 (playerLoc s) (treasureLocs s) (zombieLocs s)
      else
        drawGameOverWidget (score s)
    

drawGameOverWidget :: Int -> Widget n
drawGameOverWidget score = 
  vBox [ hBox [ str [(gameOverFigure !! r) !! c]
      | c <- [0..(31)]] |  r <- [0..(20)] ]

gameOverFigure :: [String]
gameOverFigure = 
        [ "** You Are Killed By A Zombie **"
        , "********************************"
        , "                                "
        , " #####     #    #     # ####### "
        , "#     #   # #   ##   ## #       "
        , "#        #   #  # # # # #       "
        , "#  #### #     # #  #  # #####   "
        , "#     # ####### #     # #       "
        , "#     # #     # #     # #       "
        , " #####  #     # #     # ####### "
        , "                                "
        , "####### #     # ####### ######  "
        , "#     # #     # #       #     # "
        , "#     # #     # #       #     # "
        , "#     # #     # #####   ######  "
        , "#     #  #   #  #       #   #   "
        , "#     #   # #   #       #    #  "
        , "#######    #    ####### #     # "
        , "                                "
        , "********************************"
        , "** Press ESC To Quit The Game **"
        ]




header :: PlayState -> String
header s = printf "RLA YOUR SCORE = %d" (score s)
  where
    p    = psPos s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c
  | isCurr s r c = withCursor raw
  | otherwise    = raw
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = 
  modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkXO xoMb)
  where
    xoMb      = psBoard s ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkXO :: Maybe XO -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just O) = blockO

blockB, blockX, blockO :: Widget n
blockB = vBox (replicate 5 (str "     "))
blockX = vBox [ str "X   X"
              , str " X X "
              , str "  X  "
              , str " X X "
              , str "X   X"]
blockO = vBox [ str "OOOOO"
              , str "O   O"
              , str "O   O"
              , str "O   O"
              , str "OOOOO"]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget



