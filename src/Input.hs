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

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
--Debugging
handleInput (EventMotion (x, y)) w
     = trace ("Mouse moved to: " ++ show (x,y)) w
     -- = trace (show(w)) w

--Handles UI during the game
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Play board turn ai mode)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (makeWorld (Play board turn ai mode) (x,y))
handleInput (EventKey (Char k) Down _ _) (Play board turn ai mode)
    = trace ("Key " ++ show k ++ " down") (Play board turn ai mode)
handleInput (EventKey (Char k) Up _ _) (Play board turn ai mode)
    = trace ("Key " ++ show k ++ " up") (Play board turn ai mode)


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
makeWorld (Play board turn ai mode) (x, y) = do let val = makeMove board turn (getBoardCoord (x,y))
                                                case val of Nothing  -> undoPress (Play board turn ai mode) (x, y)
                                                            (Just b) -> Play b (other turn) ai mode

--Checks for button clicks in Play
undoPress :: World -> (Float, Float) -> World
undoPress (Play board turn ai mode) (x, y) = if x >= (xBase * 1.2 - 20 - buttonWidth/2)  --Undo button
                                                && x <= (xBase * 1.2 - 20 + buttonWidth/2)
                                                && y <= yBase
                                                && y >= (yBase - buttonWidth)
                                                then undoMove (Play board turn ai mode)   --Updates world to previouss state
                                            else
                                              if x >= (xBase * 1.2 - 20 - buttonWidth/2) --Save Button
                                                && x <= (xBase * 1.2 - 20 + buttonWidth/2)
                                                && y <= yBase - squareWidth
                                                && y >= (yBase - squareWidth - buttonWidth)
                                                then writeSave $ worldToSave (Play board turn ai mode)
                                              else (Play board turn ai mode)

--Checks for button clicks in Menu
playPress :: World -> (Float, Float) -> World
playPress (Menu size target mode colour) (x,y) =
                                    if x >= -140 --Play Game button
                                      && x <= 140
                                      && y <= -65
                                      && y >= -130 then (Play (Board size target []) Black colour PvE) -- needs to be changed
                              else if x >= -140 -- Load Game button
                                      && x <= 140
                                      && y <= -150
                                      && y >= -215 then saveToWorld readSave--TODO read from file instead of initworld
                              else if x >= -140 -- Game mode button
                                      && x <= 140
                                      && y <= -150
                                      && y >= -215 then initWorld --TODO change game mode
                              else if x >= -20 -- AI colour button
                                      && x <= 20
                                      && y <= 170
                                      && y >= 130 then (Menu size target mode (other colour)) --Changes AI colour
                              else(Menu size target mode colour) --Click not on any buttons

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}
