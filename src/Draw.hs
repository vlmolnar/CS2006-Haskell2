module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Window resolution: (640, 480)
-- Constants
squareWidth :: Float
squareWidth = 70

pieceWidth :: Float
pieceWidth = 20

xBase :: Float
xBase = (-240)

yBase :: Float
yBase = 200

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
makeGrid size = pictures ([(gridVerLine (xBase + (fromIntegral i) * squareWidth) yBase (size - 1)) | i <- [0..size-1]]
                          ++ [(gridHorLine xBase (yBase - (fromIntegral i) * squareWidth) (size - 1)) | i <- [0..size-1]])

makePieces :: [(Position, Col)] -> Picture
makePieces[] = pictures []
makePieces xs = pictures [if c == Black
                            then blackPiece (xBase + (fromIntegral a) * squareWidth) (yBase - (fromIntegral b) * squareWidth)
                          else whitePiece (xBase + (fromIntegral a) * squareWidth) (yBase - (fromIntegral b) * squareWidth)
                          | x <- xs, let a = fst (fst x), let b = snd (fst x), let c = snd x]

makeVictory :: Col --Colour of winner
              -> Picture
makeVictory Black = Color white (Translate (-300) 0 (Text "Black wins!"))
makeVictory White = Color white (Translate (-300) 0 (Text "White wins!"))


-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = do let victor = winner w
                 case victor of Nothing -> pictures
                                            [ makeGrid (b_size (board w))
                                            , makePieces (pieces (board w))
                                            ]
                                Just col -> makeVictory col

-- Reference: http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
