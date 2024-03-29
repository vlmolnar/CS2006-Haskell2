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

--Returns single hint piece
hintPiece :: Float -> Float -> Picture
hintPiece x y = pictures [ translate x y (Color blue (circleSolid width))
                          , translate x y (Color (light black) (circleSolid smallWidth))
                          ]
                          where
                            width = pieceWidth
                            smallWidth = pieceWidth * 0.9

-- Returns single square in grid on Play screen
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

--Takes the board size as argument to create a grid of the given size on Play screen
makeGrid :: Int -> Picture
makeGrid size = pictures ([(gridVerLine (xBase + (fromIntegral i) * squareWidth) yBase (size - 1)) | i <- [0..size-1]]
                          ++ [(gridHorLine xBase (yBase - (fromIntegral i) * squareWidth) (size - 1)) | i <- [0..size-1]])

-- Converts array positions to coordinates on the board on Play screen
makePieces :: [(Position, Col)] -> Picture
makePieces[] = pictures []
makePieces xs = pictures [if c == Black
                            then blackPiece (xBase + (fromIntegral a) * squareWidth) (yBase - (fromIntegral b) * squareWidth)
                          else whitePiece (xBase + (fromIntegral a) * squareWidth) (yBase - (fromIntegral b) * squareWidth)
                          | x <- xs, let a = fst (fst x), let b = snd (fst x), let c = snd x]

-- Displays Undo button on Play screen
makeUndoButton :: GameMode -> Picture
makeUndoButton EvE = Text ""
makeUndoButton _ = pictures [ Color white (translate (xBase * 1.2 - 20) (yBase - buttonWidth / 2) (rectangleSolid buttonWidth buttonWidth))
                            , Color (light black) (translate (xBase * 1.2 - 20) (yBase - buttonWidth / 2) (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                            , Color white (translate (xBase * 1.2 - buttonWidth / 2 - 10) (yBase - buttonWidth / 2 - 5) (scale 0.1 0.1 (Text "Undo")))
                            ]

-- Displays Save button on Play screen
makeSaveButton :: GameMode -> Picture
makeSaveButton EvE = Text ""
makeSaveButton _ = pictures [ Color white (translate (xBase * 1.2 - 20) (yBase - squareWidth - buttonWidth/2) (rectangleSolid buttonWidth buttonWidth))
                            , Color (light black) (translate (xBase * 1.2 - 20) (yBase - squareWidth - buttonWidth/2) (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                            , Color white (translate (xBase * 1.2 - buttonWidth / 2 - 10) (yBase - squareWidth - buttonWidth/2 - 5) (scale 0.1 0.1 (Text "Save")))
                            ]
-- Displayes hint button on Play screen
makeHintButton :: GameMode -> Picture
makeHintButton EvE = Text ""
makeHintButton _ = pictures [ Color white (translate (xBase * 1.2 - 20) (yBase - (squareWidth * 2) - buttonWidth/2) (rectangleSolid buttonWidth buttonWidth))
                            , Color (light black) (translate (xBase * 1.2 - 20) (yBase - (squareWidth * 2)- buttonWidth/2) (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                            , Color white (translate (xBase * 1.2 - buttonWidth / 2 - 10) (yBase - (squareWidth * 2) - buttonWidth/2 - 5) (scale 0.1 0.1 (Text "Hint")))
                            ]

-- Displays the hint move, if enabled, on Play screen
makeHint :: [Position] -> Picture
makeHint [] = Text ""
makeHint xs = hintPiece (xBase + (fromIntegral (fst (head xs))) * squareWidth) (yBase - (fromIntegral (snd (head xs))) * squareWidth)

-- blackPiece (xBase + (fromIntegral a) * squareWidth) (yBase - (fromIntegral b) * squareWidth)


-- Prints text of winner on Victory screen
makeVictory :: Maybe Col --Colour of winner
              -> Picture
makeVictory (Just Black) = Color white (scale 0.8 0.8 (translate (-350) 0 (Text "Black wins!")))
makeVictory (Just White) = Color white (scale 0.8 0.8 (translate (-350) 0 (Text "White wins!")))
makeVictory (Nothing) = Color white (scale 0.8 0.8 (translate (-350) 0 (Text "It's a tie!")))

-- Creates buttons for starting new game, with on option to start anew or to load from save file
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

-- Checks game mode and displays it on Menu screen
makeGameMode :: GameMode -> Picture
makeGameMode PvP = drawGameMode "Player vs Player"
makeGameMode PvE = drawGameMode "Player vs AI"
makeGameMode EvE = drawGameMode "AI vs AI"

-- Creates button to display game mode on Menu screen
drawGameMode :: String -> Picture
drawGameMode msg = pictures [ Color white (translate (-230) 150 (rectangleSolid (buttonWidth * 3) buttonWidth))
                             , Color (light black) (translate (-230) 150 (rectangleSolid (buttonWidth * 3 - 4) (buttonWidth - 4)))
                             , Color white (translate (-285) 145 (scale 0.1 0.1 (Text msg)))
                             ]

-- Displays buttons for setting AI colour on Menu screen, if AI exists
makeAICol :: Col -> (Float, Float) -> Picture
makeAICol Black (x, y) = pictures [ Color white (translate x y (rectangleSolid buttonWidth buttonWidth))
                           , Color black (translate x y (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                           , Color white (translate (x-6) (y-5) (scale 0.1 0.1 (Text "AI")))
                           ]
makeAICol White (x, y) = pictures [ Color white (translate x y (rectangleSolid buttonWidth buttonWidth))
                           , Color (light black) (translate (x-6) (y-5) (scale 0.1 0.1 (Text "AI")))
                           ]

--Displays button for setting AI difficulty level
makeAILevel :: Int -> (Float, Float) -> Picture
makeAILevel level (x,y) = pictures [ Color white (translate x y (rectangleSolid buttonWidth buttonWidth))
                                   , Color (light black) (translate x y (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                                   , Color white (translate (x-12) (y-5) (scale 0.1 0.1 (Text text)))
                                   ]
                          where
                            text = if level == 1 then "Easy" else "Med"


-- Checks if AI is present in game, and if so displays its colour on Menu screen
makeAIOptions :: GameMode -> [AI] -> Picture
makeAIOptions PvP _ = Text ""  --If no AI is used, the button is not displayed
makeAIOptions PvE ai = pictures [ makeAICol (ai_colour (head ai)) (0, 150)
                                , makeAILevel (ai_level (head ai)) (0,100)
                                ]
makeAIOptions EvE ai = pictures [ makeAICol (ai_colour (head ai)) (0, 150)
                                , makeAILevel (ai_level (head ai)) (0,100)
                                , makeAICol (ai_colour (last ai)) (145, 150)
                                , makeAILevel (ai_level (last ai)) (145,100)
                                ]

makeRuleButton :: Rule -> Picture
makeRuleButton rule = pictures [ Color white (translate 260 5 (rectangleSolid buttonWidth buttonWidth))
                               , Color (light black) (translate 260 5 (rectangleSolid (buttonWidth - 4) (buttonWidth - 4)))
                               , Color white (translate (245) (0) (scale 0.1 0.1 (Text text)))
                               ]
                      where
                         text = if rule == Three then "3&3" else if rule == Four then "4&4" else "Norm"

-- Displays buttons for setting game board size on Menu screen
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

-- Displays buttons for setting game target on Menu screen
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

-- Writes "Gomoku" on Menu screen
makeLogo :: Picture
makeLogo = Color white (translate (-120) 180 (scale 0.5 0.5 (Text "Gomoku")))


-- Given a world state, return a Picture which will render the world state
drawWorld :: World -> Picture
drawWorld (Play board turn ai mode) = pictures
                           [ makeGrid (b_size board)
                           , makePieces (pieces board)
                           , makeUndoButton mode
                           , makeSaveButton mode
                           , makeHintButton mode
                           , makeHint (hint board)
                           ]
drawWorld (Victory winner) = makeVictory winner
drawWorld (Menu size target ai mode rule) = pictures
                        [ makeLogo
                        , makePlayButton
                        , makeGameMode mode
                        , makeAIOptions mode ai
                        , makeBoardButtons size
                        , makeTargetButtons target
                        , makeRuleButton rule
                        ]
