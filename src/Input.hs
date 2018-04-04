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
handleInput (EventMotion (x, y)) b
    = trace ("Mouse moved to: " ++ show (x,y)) b
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b
    = trace ("Left button pressed at: " ++ show (getBoardCoord (x,y))) (makeWorld b (getBoardCoord (x,y)))
handleInput (EventKey (Char k) Down _ _) b
    = trace ("Key " ++ show k ++ " down") b
handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
handleInput e b = b

--Calculates board coordinates from window coordinates
getBoardCoord :: (Float, Float) -> (Int, Int)
getBoardCoord (x,y) = (round ((x - xBase) / squareWidth), (round ((yBase - y) / squareWidth)))

--Updates world based on user input
makeWorld :: World -> (Int, Int) -> World
makeWorld w pos = do let val = makeMove (board w) (turn w) pos
                     case val of Nothing  -> w
                                 (Just b) -> updateWorld b (turn w)
                                 -- (Just b) -> World b (other (turn w))


{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}
