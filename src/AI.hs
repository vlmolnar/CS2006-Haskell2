module AI where

import Board
import Data.Maybe
import Data.Set

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
getBestMove n g = snd (maximum list)
                        where list = [((minimax g' (n - 1) False), pos) | (pos, g') <- (next_moves g)]


-- Gets the heuristic value of each state in the game tree and returns the best value to getBestMove
-- This value is supplied by evaluate in the Board module
minimax :: GameTree -> Int -> Bool -> Int
minimax g 0 True = evaluate (game_board g) (game_turn g) -- depth 0
minimax g 0 False = -1 * (evaluate (game_board g) (game_turn g)) -- depth 0
minimax g n True = maximum [(minimax (snd nextMove) (n - 1) False) | nextMove <- (next_moves g)]
minimax g n False = minimum [(minimax (snd nextMove) (n - 1) True) | nextMove <- (next_moves g)]

-- currently generating all possible moves
moveGenerator :: Board -> Col -> [(Int, Int)]
moveGenerator board col = [(x, y) | x <- [0.. (b_size board) - 1],
                                    y <- [0.. (b_size board) - 1],
                                    not ((elem ((x, y), col) (pieces board)) || (elem ((x, y), (other col)) (pieces board)))]
-- remove duplicates from list
-- convert to Data.set then back to list
-- ref: https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell
rmDup :: Ord a => [a] -> [a]
rmDup = toList . fromList

-- generate all the empty positions adjacent to a piece from a list of pieces
moveGeneratorAdj :: Board -> Col -> [(Int, Int)]
moveGeneratorAdj b c = moveGeneratorAdjDiffParam b (pieces b)

moveGeneratorAdjDiffParam :: Board -> [(Position, Col)] -> [Position]
moveGeneratorAdjDiffParam b [] = []
moveGeneratorAdjDiffParam board (p:ps) = rmDup ((getAdj board p) ++ moveGeneratorAdjDiffParam board ps)
      where
        getAdj :: Board -> (Position, Col) -> [Position]
        getAdj board ((a, b), col)
            = [(a + x, b + y) | x <- [-1, 0, 1],
                                y <- [-1, 0, 1],
                                (checkNextPiece board (x, y) ((a, b), col)) == Empty
                                && (boundsCheck (b_size board) ((a, b), col) (x, y))]

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
--updateWorld t w = w
updateWorld t (Play board turn)
                 = do let winner = checkWon board (pieces board)
                      case winner of Nothing -> makeAIMove (Play board turn)
                                     (Just c) -> Victory (Just c)

makeAIMove :: World -> World
makeAIMove (Play board turn)
              | turn == Black = Play board turn
              | otherwise
                    = Play (fromJust (makeMove board turn pos)) (other turn)
                              where pos = getBestMove 3 (buildTree gen board turn )
                                    gen = moveGeneratorAdj
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

undoMove :: World -> World
undoMove (Play b turn)
            = Play board (other turn)
                where board = Board (b_size b) (target b) (drop 1 (pieces b))
