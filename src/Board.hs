module Board where

data Col = Black | White
  deriving (Show, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col }

initWorld = World initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board col pos | fst pos < 0 = Nothing
                       | snd pos < 0 = Nothing
                       | fst pos > (size board) - 1 = Nothing
                       | snd pos > (size board) - 1 = Nothing
                       | elem (pos, col) (pieces board) = Nothing
                       | otherwise = Just (Board (size board) (target board) ((pos, col) : (pieces board)))

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> [(Position, Col)]-> Maybe Col
checkWon board [] = Nothing
checkWon board (x:xs) = if checkDirections board x
                                            then Just (snd x)
                                            else checkWon board xs

checkDirections :: Board -> (Position, Col) -> Bool
checkDirections board piece = or [checkDirection board (target board) (x, y) piece | x <- [-1, 0, 1], y <- [-1, 0, 1]]


checkDirection :: Board -> Int -> (Int, Int) -> (Position, Col) -> Bool
checkDirection board n (dirX, dirY) ((x,y), col) =
                  if n == 1
                        then True
                        else if elem ((x - dirX, y - dirY), col) (pieces board)
                                  then checkDirection board (n - 1) (dirX, dirY) ((x - dirX, y - dirY), col)
                                  else False

{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined
