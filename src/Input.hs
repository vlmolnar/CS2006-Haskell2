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
xBase = (-240)

yBase :: Float
yBase = 200

buttonWidth :: Float
buttonWidth = squareWidth / 1.5

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

--Handles UI during the game
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Play board turn)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (makeWorld (Play board turn) (x,y))
handleInput (EventKey (Char k) Down _ _) (Play board turn)
    = trace ("Key " ++ show k ++ " down") (Play board turn)
handleInput (EventKey (Char k) Up _ _) (Play board turn)
    = trace ("Key " ++ show k ++ " up") (Play board turn)


--Handles UI on Victory screen, proceeds to menu if the user clicks
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Victory winner)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) Menu Black

--Handles UI on Menu screen
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Menu colour)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) initWorld

handleInput e b = b


--Calculates board coordinates from window coordinates
getBoardCoord :: (Float, Float) -> (Int, Int)
getBoardCoord (x,y) = (round ((x - xBase) / squareWidth), (round ((yBase - y) / squareWidth)))

--Updates world based on user input
makeWorld :: World -> (Float, Float) -> World
makeWorld (Play board turn) (x, y) = do let val = makeMove board turn (getBoardCoord (x,y))
                                        case val of Nothing  -> undoPress (Play board turn) (x, y)
                                                             -- (Play board turn)
                                                    (Just b) -> Play b (other turn)

--Activates undo
undoPress :: World -> (Float, Float) -> World
undoPress (Play board turn) (x, y) | x >= (xBase * 1.2)
                                     && x <= (xBase * 1.2 + buttonWidth)
                                     && y <= yBase
                                     && y >= (yBase - buttonWidth) = undoMove (Play board turn)   --Updates world to previouss state
                                   | otherwise = (Play board turn)                                --Undo button not recognised

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}
