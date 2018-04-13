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

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
--b = world
handleInput (EventMotion (x, y)) (Play board turn winner)
    = trace ("Mouse moved to: " ++ show (x,y)) (Play board turn winner)
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (Play board turn winner)
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (makeWorld (Play board turn winner) (getBoardCoord (x,y)))
handleInput (EventKey (Char k) Down _ _) (Play board turn winner)
    = trace ("Key " ++ show k ++ " down") (Play board turn winner)
handleInput (EventKey (Char k) Up _ _) (Play board turn winner)
    = trace ("Key " ++ show k ++ " up") (Play board turn winner)
handleInput e b = b

--If Replay button is clicked

--Calculates board coordinates from window coordinates
getBoardCoord :: (Float, Float) -> (Int, Int)
getBoardCoord (x,y) = (round ((x - xBase) / squareWidth), (round ((yBase - y) / squareWidth)))

--Updates world based on user input
makeWorld :: World -> (Int, Int) -> World
makeWorld (Play board turn winner) pos = do let val = makeMove board turn pos
                                            case val of Nothing  -> (Play board turn winner)
                                                        (Just b) -> Play b (other turn) Nothing


{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}
