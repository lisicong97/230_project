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
  vBox [ hBox [ if (gameOverFigure !! r) !! c == '#'
                then withAttr (attrName "wall") (str " ")
                else if (gameOverFigure !! r) !! c == ' '
                  then withAttr (attrName "path") (str " ")
                  else withAttr (attrName "path") (str [(gameOverFigure !! r) !! c])
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


withCursor :: Widget n -> Widget n
withCursor = 
  modifyDefAttr (`withStyle` reverseVideo)

    --   | otherwise = Nothing


vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget



