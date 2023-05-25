module Main where

import Data.Maybe
import Game

main :: IO ()
main = play LeftPlayer exampleGame

play :: Player -> Game -> IO ()
play p g = do
  putStrLn ""
  print g
  putStrLn ""
  let opts = options p g
  if null opts
    then do
      putStrLn $ "Player " ++ show (opponent p) ++ " wins!"
    else do
      putStrLn $ "Player " ++ show p ++ ", choose your move:\n(or type 0 to calculate and choose a winning move (this may take forever), or type -1 to quit)"
      mapM_ putStrLn $ zipWith (\i m -> show i ++ ": " ++ show m) [1 ..] opts
      i <- readLn
      if i == -1
        then return ()
        else
          if i == 0
            then do
              let winning = winningMove p g
              if isNothing winning
                then do
                  putStrLn "\nALL MOVES ARE LOSING!"
                  play p g
                else do
                  putStrLn "\nWinning move found:"
                  play (opponent p) (fromJust winning)
            else
              if i < 1 || i > length opts
                then do
                  putStrLn "\nINVALID MOVE!"
                  play p g
                else do
                  let selected = opts !! (i - 1)
                  play (opponent p) selected

exampleGame = toadsAndFrogsFromString ">_>_<_<" + gamify (Cutcake 3 6) + gamify (FibonacciNim 38) + linearColFromString "-xl----lRl---l"

numbersExample = g 2 + g (-4)

cutcakeExample = gamify (Cutcake 4 4)

fractionExample = linearColFromString "l-" + linearColFromString "l-" + g (-1)

nimExample = gamify (ClassicNim 5)

nimAdditionExample = gamify (ClassicNim 1) + gamify (ClassicNim 2) + gamify (ClassicNim 3)

fibonacciNimAnalysis = map (simplify . gamify . FibonacciNim) [1 .. 100]

upExample = toadsAndFrogsFromString ">_><<"

hotGameExample = toadsAndFrogsFromString "_>>_<<_"
