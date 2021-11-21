{-# LANGUAGE DeriveFunctor #-}
module Model.Maze 
  (
    mazeWidge
  )
  where

import Brick


mazeWidge :: Widget n
mazeWidge = vBox [foldl (\w c -> hBox [w, str [c]]) emptyWidget c | c <- maze0 ]


maze0 :: [[Char]]
maze0 = [ "###########################"
        , "#........... #............#"
        , "#.####.#####.#.#####.####.#"
        , "#*#  # #   #.#.#   # #  #*#"
        , "#.####.#####.#.#####.####.#"
        , "#.........................#"
        , "#.####.#.#########.#.####.#"
        , "#......#.....#.....#......#"
        , "######.##### # #####.######"
        , "     #.#           #.#     "
        , "######.# ###---### #.######"
        , "      .  #       #  .      "
        , "######.# ######### #.######"
        , "     #.#           #.#     "
        , "######.# ######### #.######"
        , "#............#............#"
        , "#.####.#####.#.#####.####.#"
        , "#*...#...............#...*#"
        , "####.#.#.#########.#.#.####"
        , "#......#.....#.....#......#"
        , "#.##########.#.##########.#"
        , "#.........................#"
        , "###########################"
        ]