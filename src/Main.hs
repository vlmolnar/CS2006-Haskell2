module Main where

import Graphics.Gloss
import System.Environment

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

getRule :: [Char] -> Rule
getRule "Regular" = Regular
getRule "Three" = Three
getRule "Four" = Four

main :: IO ()
main = do
          args <- getArgs
          if null args
              then runGame initWorld
              else do size_string <- return (args !! 0)
                      let size = read size_string :: Int
                      ta <- return (args !! 1)
                      let target = read ta :: Int
                      col <- return (args !! 2)
                      rule <- return (args !! 3)
                      case col of
                               "Black" -> runGame (Play (Board size target (getRule rule) []) White (AI Black 2) PvE )
                               otherwise->runGame (Play (Board size target (getRule rule) []) Black (AI White 2) PvE )

runGame :: World -> IO ()
runGame world = play (InWindow "Gomoku" (640, 480) (10, 10))  (light black) 10
            world -- in Board.hs, provides world
            drawWorld -- in Draw.hs, turns world into Picture
            handleInput -- in Input.hs, given an event, changes world
            updateWorld -- in AI.hs, IO
