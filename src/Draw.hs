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
xBase = (-220)

yBase :: Float
yBase = 200

buttonWidth :: Float
buttonWidth = squareWidth / 1.5

playWidth :: Float
playWidth = squareWidth * 4



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

makeUndoButton :: Picture
makeUndoButton = pictures [ Color white (translate (xBase * 1.2 - 20) (yBase - buttonWidth / 2) (rectangleSolid buttonWidth buttonWidth))
                          , Color (light black) (translate (xBase * 1.2 - 20) (yBase - buttonWidth / 2) (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                          , Color white (translate (xBase * 1.2 - buttonWidth / 2 - 10) (yBase - buttonWidth / 2 - 5) (scale 0.1 0.1 (Text "Undo")))
                          ]

makeSaveButton :: Picture
makeSaveButton = pictures [ Color white (translate (xBase * 1.2 - 20) (yBase - squareWidth - buttonWidth/2) (rectangleSolid buttonWidth buttonWidth))
                          , Color (light black) (translate (xBase * 1.2 - 20) (yBase - squareWidth - buttonWidth/2) (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                          , Color white (translate (xBase * 1.2 - buttonWidth / 2 - 10) (yBase - squareWidth - buttonWidth/2 - 5) (scale 0.1 0.1 (Text "Save")))
                          ]

makePlayButton :: Picture
makePlayButton = pictures [ --New Game
                            Color white (translate 0 (-100) (rectangleSolid playWidth squareWidth))
                          , Color (light black) (translate 0 (-100) (rectangleSolid (playWidth - 4) (squareWidth - 4)))
                          , Color (white) (translate (- (playWidth/2 - 40)) (-115) (scale 0.3 0.3 (Text "New Game")))
                          -- Load Game
                          , Color white (translate 0 (-180) (rectangleSolid playWidth squareWidth))
                          , Color (light black) (translate 0 (-180) (rectangleSolid (playWidth - 4) (squareWidth - 4)))
                          , Color (white) (translate (- (playWidth/2 - 40)) (-195) (scale 0.3 0.3 (Text "Load Game")))
                          ]

makeVictory :: Maybe Col --Colour of winner
              -> Picture
makeVictory (Just Black) = Color white (scale 0.8 0.8 (translate (-350) 0 (Text "Black wins!")))
makeVictory (Just White) = Color white (scale 0.8 0.8 (translate (-350) 0 (Text "White wins!")))
makeVictory (Nothing) = Color white (scale 0.8 0.8 (translate (-350) 0 (Text "It's a tie!")))

makeGameMode :: GameMode -> Picture
makeGameMode PvP = drawGameMode "Player vs Player"
makeGameMode PvE = drawGameMode "Player vs AI"
makeGameMode EvE = drawGameMode "AI vs AI"

drawGameMode :: String -> Picture
drawGameMode msg = pictures [ Color white (translate (-230) 150 (rectangleSolid (buttonWidth * 3) buttonWidth))
                             , Color (light black) (translate (-230) 150 (rectangleSolid (buttonWidth * 3 - 4) (buttonWidth - 4)))
                             , Color white (translate (-285) 145 (scale 0.1 0.1 (Text msg)))
                             ]

makeAICol :: Col -> Picture
makeAICol Black = pictures [ Color white (translate 0 150 (rectangleSolid buttonWidth buttonWidth))
                           , Color black (translate 0 150 (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                           , Color white (translate (-6) 145 (scale 0.1 0.1 (Text "AI")))
                           ]
makeAICol White = pictures [ Color white (translate 0 150 (rectangleSolid buttonWidth buttonWidth))
                           , Color (light black) (translate (-6) 145 (scale 0.1 0.1 (Text "AI")))
                           ]

--
makeAIOptions :: GameMode -> Col -> Picture
makeAIOptions mode c = if mode == PvP then Text ""  --If no AI is used, the button is not displayed
                       else case c of Black -> makeAICol Black
                                      White -> makeAICol White
                                      Empty -> Text ""


makeBoardButtons :: Int -> Picture
makeBoardButtons size = pictures [--Number display
                                   Color white (translate (-230) 5 (rectangleSolid (buttonWidth * 3) buttonWidth))
                                 , Color (light black) (translate (-230) 5 (rectangleSolid (buttonWidth * 3 - 4) (buttonWidth - 4)))
                                 , Color white (translate (-285) 3 (scale 0.1 0.1 (Text (show size))))
                                 --Up button
                                 , Color white (translate (-135) 30 (rectangleSolid (buttonWidth) buttonWidth))
                                 , Color (light black) (translate (-135) 30 (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                                 , Color white (translate (-147) 15 (scale 0.3 0.3 (Text "^")))
                                 -- Down button
                                 , Color white (translate (-135) (30 - buttonWidth) (rectangleSolid (buttonWidth) buttonWidth))
                                 , Color (light black) (translate (-135) (30 - buttonWidth) (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                                 , Color white (translate (-145) (17 - buttonWidth) (scale 0.3 0.3 (Text "v")))
                                 ]

makeTargetButtons :: Int -> Picture
makeTargetButtons target = pictures [--Number display
                                  Color white (translate (50) 5 (rectangleSolid (buttonWidth * 3) buttonWidth))
                                , Color (light black) (translate (50) 5 (rectangleSolid (buttonWidth * 3 - 4) (buttonWidth - 4)))
                                , Color white (translate (0) 3 (scale 0.1 0.1 (Text (show target))))
                                --Up button
                                , Color white (translate (145) 30 (rectangleSolid (buttonWidth) buttonWidth))
                                , Color (light black) (translate (145) 30 (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                                , Color white (translate (135) 15 (scale 0.3 0.3 (Text "^")))
                                -- Down button
                                , Color white (translate (145) (30 - buttonWidth) (rectangleSolid (buttonWidth) buttonWidth))
                                , Color (light black) (translate (145) (30 - buttonWidth) (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                                , Color white (translate (137) (17 - buttonWidth) (scale 0.3 0.3 (Text "v")))
                                ]

makeMenu :: Picture
makeMenu = Color white (translate (-120) 180 (scale 0.5 0.5 (Text "Gomoku")))


-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld (Play board turn ai mode rule) = pictures
                           [ makeGrid (b_size board)
                           , makePieces (pieces board)
                           , makeUndoButton
                           , makeSaveButton
                           ]
drawWorld (Victory winner) = makeVictory winner
drawWorld (Menu size target mode colour) = pictures
                        [ makeMenu
                        , makePlayButton
                        , makeGameMode mode
                        , makeAIOptions mode colour
                        , makeBoardButtons size
                        , makeTargetButtons target
                        ]

-- Reference: http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
