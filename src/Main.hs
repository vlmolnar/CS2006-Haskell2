module Main where

import Graphics.Gloss

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

testFunc :: IO()
testFunc = display (InWindow "Testing" (500,500) (100,100))
                blue
                (Pictures [Color red (Circle 100)])

main :: IO ()
main = play (InWindow "Gomoku" (640, 480) (10, 10)) black 10
            initWorld -- in Board.hs, provides world
            drawWorld -- in Draw.hs, turns world into Picture
            handleInput -- in Input.hs, given an event, changes world
            updateWorld -- in AI.hs, IO
