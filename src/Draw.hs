module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Window resolution: (640, 480)
squareWidth :: Float
squareWidth = 70

pieceWidth :: Float
pieceWidth = 20

--Returns single black piece
blackPiece :: Float -> Float -> Picture
blackPiece x y = translate x y (Color black (circleSolid pieceWidth))

--Returns single white piece
whitePiece :: Float -> Float -> Picture
whitePiece x y = pictures [ translate x y (Color black (circleSolid width))
                          , translate x y (Color white (circleSolid smallWidth))
                          ]
                          where
                            width = pieceWidth
                            smallWidth = pieceWidth * 0.9

-- Returns single square in grid
gridSquare :: Float -> Float -> Picture
gridSquare x y = pictures [ translate x y (Color black (rectangleSolid width width))
                          , translate x y (Color yellow (rectangleSolid smallWidth smallWidth))
                          ]
                          where
                            width = squareWidth
                            smallWidth = squareWidth * 0.9

-- Returns single horizontal line in grid, alternate strategy to squares, takes in start x and y coords and grid length
gridHorLine :: Float -> Float -> Int -> Picture
gridHorLine x y size = Color white (Line[(x, y), (x + (fromIntegral size) * width, y)])
                       where
                        width = squareWidth

-- Returns single vertical line in grid, alternate strategy to squares, takes in start x and y coords and grid length
gridVerLine :: Float -> Float -> Int -> Picture
gridVerLine x y size = Color white (Line[(x, y), (x, y - (fromIntegral size) * width)])
                       where
                        width = squareWidth

--Takes the board size as argument to create a grid of the given size
makeGrid :: Int -> Picture
makeGrid size = pictures ([(gridVerLine (-240 + (fromIntegral i) * squareWidth) 200 (size - 1)) | i <- [0..size-1]]
                          ++ [(gridHorLine (-240) (200 - (fromIntegral i) * squareWidth) (size - 1)) | i <- [0..size-1]])

-- makePieces :: Board -> Picture

-- alternate to makeGrid, uses gridSquares, unfinished
-- makeBoard :: Board -> Picture
-- makeBoard board = do let squares = []
--                      let x =
--                      do squares ++
--                            where
--                              size = size board  -- Board size should be 6



-- List that keeps track of all current pictures on display
drawing :: Picture
drawing = pictures
  [--  blackPiece 240 175
  -- , whitePiece 0 0
  -- , gridSquare 240 175
  -- , gridSquare 50 (-140)
  -- , gridHorLine (-240) 200 6
  -- , gridVerLine (-240) 200 6
  makeGrid 6
  ]
  -- [ Color yellow (rectangleSolid 60 60) -- Single grid on board
  -- , Color black (circleSolid 20)      -- Black piece
  -- , Color white (circleSolid 20)      -- White piece
  -- ]

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = pictures
  [
  makeGrid (size (board w))
  ]
-- drawWorld w = Color blue $ Circle 100   -- $ same as parantheses on anything that comes after it

-- Function to test if Gloss is working properly
testFunc :: IO()
testFunc = display (InWindow "Testing" (500,500) (100,100))
                blue
                (Pictures [Color red (Circle 100)])

-- Reference: http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
