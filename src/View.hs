module View (view) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Maze ( maze0, drawMazeWidget )

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
drawGameOverWidget _ = 
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



