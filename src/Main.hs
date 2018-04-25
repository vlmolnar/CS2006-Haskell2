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
getRule "Three" = Three
getRule "Four" = Four
getRule _ = Regular

getColour :: [Char] -> Col
getColour "Black" = Black
getColour _ = White

main :: IO ()
main = do
          args <- getArgs
          if null args
              then runGame initWorld
              else do size_string <- return (args !! 0)
                      let s = read size_string :: Int
                      target <- return (args !! 1)
                      let t = read target :: Int
                      rule <- return (args !! 2)
                      let r = (getRule rule)
                      colour <- return (args !! 3)
                      let c = (getColour colour)
                      level_string <- return (args !! 4)
                      let l = read level_string :: Int
                      runGame (Play (Board s t r [])  (other c) (AI c l) PvE)

runGame :: World -> IO ()
runGame world = play (InWindow "Gomoku" (640, 480) (10, 10))  (light black) 10
            world -- in Board.hs, provides world
            drawWorld -- in Draw.hs, turns world into Picture
            handleInput -- in Input.hs, given an event, changes world
            updateWorld -- in AI.hs, IO
