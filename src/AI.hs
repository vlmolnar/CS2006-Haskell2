module AI where

import Board
import Data.Maybe

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }
        deriving Show
-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen board col = let moves = gen board col in -- generated moves
                        GameTree board col (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove board col pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other col)) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove n g = fst (maximum [ (pos, (minimax g' n True)) | (pos, g') <- (next_moves g)])

minimax :: GameTree -> Int -> Bool -> Int
minimax g 0 isMax = evaluate (game_board g) (game_turn g) -- depth 0
minimax (GameTree b col []) n isMax = evaluate b col -- leaf node
minimax g n True = maximum [(minimax (snd nextMove) (n -1) False) | nextMove <- (next_moves g)]
minimax g n False = minimum [(minimax (snd nextMove) (n -1) True) | nextMove <- (next_moves g)]


-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w | (turn w) == Black = w -- if human
                | otherwise
                      = World (fromJust (makeMove (board w) col pos)) col
                                where col = (turn w)
                                      pos = getBestMove 1 (buildTree gen (board w) col )
                                      gen = moveGenerator

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either
 player has won and display a message if so.
-}

-- currently generating all possible moves
moveGenerator :: Board -> Col -> [Position]
moveGenerator board col = [(x, y) | x <- [0.. (size board) - 1],
                                    y <- [0.. (size board) - 1],
                                    not ((elem ((x, y), col) (pieces board)) || (elem ((x, y), (other col)) (pieces board)))]
