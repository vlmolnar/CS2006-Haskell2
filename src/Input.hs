module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
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
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Play board turn ai mode rule)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (makeWorld (Play board turn ai mode rule) (x,y))
handleInput (EventKey (Char k) Down _ _) (Play board turn ai mode rule)
    = trace ("Key " ++ show k ++ " down") (Play board turn ai mode rule)
handleInput (EventKey (Char k) Up _ _) (Play board turn ai mode rule)
    = trace ("Key " ++ show k ++ " up") (Play board turn ai mode rule)


--Handles UI on Victory screen, proceeds to menu if the user clicks
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Victory winner)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (Menu 10 5 PvE Black)

--Handles UI on Menu screen
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Menu size target mode colour)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) playPress (Menu size target mode colour) (x,y)

handleInput e b = b


--Calculates board coordinates from window coordinates
getBoardCoord :: (Float, Float) -> (Int, Int)
getBoardCoord (x,y) = (round ((x - xBase) / squareWidth), (round ((yBase - y) / squareWidth)))

--Updates world based on user input
makeWorld :: World -> (Float, Float) -> World
-- Checks for button clicks on board, makes a move if it's legal
makeWorld (Play board turn ai mode rule) (x, y) = do let val = makeMove board turn (getBoardCoord (x,y))
                                                     case val of Nothing  -> undoPress (Play board turn ai mode rule) (x, y)
                                                                 (Just b) -> Play b (other turn) ai mode rule

--Checks for button clicks in Play
undoPress :: World -> (Float, Float) -> World
undoPress (Play board turn ai mode rule) (x, y) = if x >= (xBase * 1.2 - 20 - buttonWidth/2)  --Undo button
                                                    && x <= (xBase * 1.2 - 20 + buttonWidth/2)
                                                    && y <= yBase
                                                    && y >= (yBase - buttonWidth)
                                                    then undoMove (Play board turn ai mode rule) --Updates world to previouss state
                                                else
                                                  if x >= (xBase * 1.2 - 20 - buttonWidth/2) --Save Button
                                                    && x <= (xBase * 1.2 - 20 + buttonWidth/2)
                                                    && y <= yBase - squareWidth
                                                    && y >= (yBase - squareWidth - buttonWidth)
                                                    then writeSave $ worldToSave (Play board turn ai mode rule)
                                                  else (Play board turn ai mode rule)

--Checks for button clicks in Menu
playPress :: World -> (Float, Float) -> World
playPress (Menu size target mode colour) (x,y) =
                                    if x >= -140 --New Game button
                                      && x <= 140
                                      && y <= -65
                                      && y >= -130 then (Play (Board size target []) Black colour mode Regular) -- Starts new game with settings
                              else if x >= -140 -- Load Game button
                                      && x <= 140
                                      && y <= -150
                                      && y >= -215 then saveToWorld readSave -- Reads from file to load saved state
                              else if x >= -20 -- AI colour button
                                      && x <= 20
                                      && y <= 170
                                      && y >= 130 then (Menu size target mode (other colour)) --Changes AI colour
                              else if x >= -155 -- Board size up
                                      && x <= -115
                                      && y <= 50
                                      && y >= 10 then if size + 1 <= 10 then(Menu (size+1) target mode colour) -- Increments board size by 1
                                                      else (Menu size target mode colour)
                              else if x >= -155 -- Board size down
                                      && x <= -115
                                      && y <= 5
                                      && y >= -38 then if size - 1 >= 3 && size - 1 >= target then(Menu (size-1) target mode colour) -- Decrements board size by 1
                                                       else (Menu size target mode colour)
                              else if x >= 125 -- Target size up
                                      && x <= 165
                                      && y <= 50
                                      && y >= 10 then if target + 1 <= size then(Menu size (target+1) mode colour) -- Increments target size by 1
                                                      else (Menu size target mode colour)
                              else if x >= 125 -- Target size down
                                      && x <= 165
                                      && y <= 5
                                      && y >= -38 then if target - 1 >= 3 then(Menu size (target-1) mode colour) -- Decrements target size by 1
                                                       else (Menu size target mode colour)
                              else if x >= -300 -- Change game mode
                                      && x <= -160
                                      && y <= 170
                                      && y >= 130 then (Menu size target (switchGameMode mode) colour) -- Changes game mode
                              else (Menu size target mode colour) --Click not on any buttons

-- GameMode button functionality, switches from one mode to another
switchGameMode :: GameMode -> GameMode
switchGameMode PvP = PvE
switchGameMode PvE = EvE
switchGameMode EvE = PvP
