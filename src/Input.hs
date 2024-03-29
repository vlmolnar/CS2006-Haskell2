module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import File
import Board
import Draw
import AI

import Debug.Trace

squareWidth :: Float
squareWidth = 70

xBase :: Float
xBase = (-220)

yBase :: Float
yBase = 200

buttonWidth :: Float
buttonWidth = squareWidth / 1.5

playWidth :: Float
playWidth = squareWidth * 4

-- Update the world state given an input event
handleInput :: Event -> World -> World
--Debugging
handleInput (EventMotion (x, y)) w
     = trace ("Mouse moved to: " ++ show (x,y)) w

--Handles UI during the game
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Play board turn ai EvE)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (Play board turn ai EvE) --Prevents user from placing pieces in an EvE game
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Play board turn ai mode)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (makeWorld (Play board turn ai mode) (x,y))
-- handleInput (EventKey (Char k) Down _ _) (Play board turn ai mode)
--     = trace ("Key " ++ show k ++ " down") (Play board turn ai mode)
-- handleInput (EventKey (Char k) Up _ _) (Play board turn ai mode)
--     = trace ("Key " ++ show k ++ " up") (Play board turn ai mode)


--Handles UI on Victory screen, proceeds to menu if the user clicks
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Victory winner)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (Menu 6 3 [(AI White 2), (AI Black 1)] PvE Regular)

--Handles UI on Menu screen
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Menu size target mode colour rule)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) playPress (Menu size target mode colour rule) (x,y)

handleInput e b = b


--Calculates board coordinates from window coordinates
getBoardCoord :: (Float, Float) -> (Int, Int)
getBoardCoord (x,y) = (round ((x - xBase) / squareWidth), (round ((yBase - y) / squareWidth)))

--Updates Play world based on user input
makeWorld :: World -> (Float, Float) -> World
-- Checks for button clicks on board, makes a move if it's legal
makeWorld (Play board turn ai mode) (x, y) = do let val = makeMove board turn (getBoardCoord (x,y)) (b_rule board)
                                                case val of Nothing  -> undoPress (Play board turn ai mode) (x, y)
                                                            (Just b) -> Play b (other turn) ai mode

--Checks for button clicks in Play
undoPress :: World -> (Float, Float) -> World
undoPress (Play board turn ai EvE) (x, y) = (Play board turn ai EvE)  -- Disables Undo and Save in EvE mode
undoPress (Play board turn ai mode) (x, y) = if x >= (xBase * 1.2 - 20 - buttonWidth/2)  --Undo button
                                                    && x <= (xBase * 1.2 - 20 + buttonWidth/2)
                                                    && y <= yBase
                                                    && y >= (yBase - buttonWidth)
                                                    then undoMove (Play board turn ai mode) --Updates world to previous state
                                             else if x >= (xBase * 1.2 - 20 - buttonWidth/2) --Save button
                                                     && x <= (xBase * 1.2 - 20 + buttonWidth/2)
                                                     && y <= yBase - squareWidth
                                                     && y >= (yBase - squareWidth - buttonWidth)
                                                     then writeSave $ worldToSave (Play board turn ai mode)
                                             else if x >= (xBase * 1.2 - 20 - buttonWidth/2) --Hint button
                                                     && x <= (xBase * 1.2 - 20 + buttonWidth/2)
                                                     && y <= yBase - (squareWidth * 2)
                                                     && y >= (yBase - (squareWidth * 2) - buttonWidth)
                                                     then writeSave $ worldToSave (Play (hintToBoard (Play board turn ai mode)) turn ai mode)
                                             else (Play board turn ai mode)

--Checks for button clicks in Menu
playPress :: World -> (Float, Float) -> World
playPress (Menu size target ai mode rule) (x,y) =
                                    if x >= -140 --New Game button
                                      && x <= 140
                                      && y <= -65
                                      && y >= -130 then (Play (Board size target rule [] []) Black ai mode) -- Starts new game with settings
                              else if x >= -140 -- Load Game button
                                      && x <= 140
                                      && y <= -150
                                      && y >= -215 then saveToWorld readSave -- Reads from file to load saved state
                              else if x >= -20 -- AI 1 colour button
                                      && x <= 20
                                      && y <= 170
                                      && y >= 130 then (Menu size target [ AI (other (ai_colour (head ai))) (ai_level (head ai))
                                                                         , AI (other (ai_colour (last ai))) (ai_level (last ai))] mode rule) --Changes AI colour
                             else if mode == EvE -- AI 2 colour button
                                     && x >= 125
                                     && x <= 165
                                     && y <= 170
                                     && y >= 130 then (Menu size target [ AI (other (ai_colour (head ai))) (ai_level (head ai))
                                                                        , AI (other (ai_colour (last ai))) (ai_level (last ai))] mode rule) --Changes AI colour
                              else if x >= -20 -- AI 1 level button
                                      && x <= 20
                                      && y <= 120
                                      && y >= 80 then (Menu size target [AI ((ai_colour (head ai))) (switchAILevel (ai_level (head ai))), (last ai)] mode rule) --Changes AI level
                              else if mode == EvE -- AI 2 level button
                                      && x >= 125
                                      && x <= 165
                                      && y <= 120
                                      && y >= 80 then (Menu size target [(head ai), AI ((ai_colour (last ai))) (switchAILevel (ai_level (last ai)))] mode rule) --Changes AI level
                              else if x >= -155 -- Board size up
                                      && x <= -115
                                      && y <= 50
                                      && y >= 10 then if size + 1 <= 10 then(Menu (size+1) target ai mode rule) -- Increments board size by 1
                                                      else (Menu size target ai mode rule)
                              else if x >= -155 -- Board size down
                                      && x <= -115
                                      && y <= 5
                                      && y >= -38 then if size - 1 >= 3 && size - 1 >= target then(Menu (size-1) target ai mode rule) -- Decrements board size by 1
                                                       else (Menu size target ai mode rule)
                              else if x >= 125 -- Target size up
                                      && x <= 165
                                      && y <= 50
                                      && y >= 10 then if target + 1 <= size && target + 1 <= 5 then(Menu size (target+1) ai mode rule) -- Increments target size by 1
                                                      else (Menu size target ai mode rule)
                              else if x >= 125 -- Target size down
                                      && x <= 165
                                      && y <= 5
                                      && y >= -38 then if target - 1 >= 3 then(Menu size (target-1) ai mode rule) -- Decrements target size by 1
                                                       else (Menu size target ai mode rule)
                              else if x >= -300 -- Change game mode
                                      && x <= -160
                                      && y <= 170
                                      && y >= 130 then (Menu size target ai (switchGameMode mode) rule) -- Changes game mode
                              else if x >= 240-- Change game mode
                                      && x <= 280
                                      && y <= 25
                                      && y >= -15 then (Menu size target ai mode (switchRule rule)) -- Changes game rules
                              else (Menu size target ai mode rule) --Click not on any buttons

-- GameMode button functionality, switches from one mode to another
switchGameMode :: GameMode -> GameMode
switchGameMode PvP = PvE
switchGameMode PvE = EvE
switchGameMode EvE = PvP

-- Switches AI difficulty levels
switchAILevel :: Int -> Int
switchAILevel 1 = 2
switchAILevel 2 = 1
switchAILevel _ = 2

-- Switches game ruleset
switchRule :: Rule -> Rule
switchRule Regular = Three
switchRule Three = Four
switchRule Four = Regular

hintToBoard :: World-> Board
hintToBoard world = do case h of [] -> Board (b_size b) (b_target b) (b_rule b) (pieces b) (getHint world)
                                 _ -> Board (b_size b) (b_target b) (b_rule b) (pieces b) []
                        where
                          b = board world
                          h = hint (board world)
