module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Window resolution: (640, 480)
drawing pictures =
  [ Color black (circleSolid 10)                  -- Black piece
  , Color white (circleSolid 10)                  -- White piece
  , Color white (Line [(-250, -10), (50, -10)])   -- Horizontal line
  , Color white (Line [(-250, -10), (-250, -210)])-- Vertical line
  ]
  -- "translate" function repositions coordinats, takes an x and y argument

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = Color blue $ Circle 100   -- $ same as parantheses on anything that comes after it

-- Function to test if Gloss is working properly
testFunc :: IO()
testFunc = display (InWindow "Testing" (500,500) (100,100))
                blue
                (Pictures [Color red (Circle 100)])

-- Reference: http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
