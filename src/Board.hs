{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Board where

import Data.Aeson
import GHC.Generics

-- P: Player, E: Environment (AI)
data GameMode = PvP | PvE | EvE
  deriving (Show, Eq, Generic, Read)

instance FromJSON GameMode
instance ToJSON GameMode

--House rules
data Rule = Regular | Three | Four
  deriving (Show, Eq, Generic, Read)

instance FromJSON Rule
instance ToJSON Rule

-- Piece colours
data Col = Black | White | Empty
  deriving (Show, Eq, Generic, Read)

instance FromJSON Col
instance ToJSON Col

-- Switches colours
other :: Col -> Col
other Black = White
other White = Black
other Empty = Empty

type Position = (Int, Int)

type Direction = (Int, Int)

--Reverses direction
oppDir :: Direction -> Direction
oppDir (x, y) = (-x, -y)

boundsCheck :: Int -> (Position, Col) -> (Direction) -> Bool
boundsCheck n ((x, y), col) (dirX, dirY)
    |x + dirX < 0 = False          --bounds checks
    | y + dirY < 0 = False         --bounds checks
    | x + dirX >= n = False --bounds checks
    | y + dirY >= n = False --bounds checks
    | otherwise = True

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board {
                     b_size :: Int,
                     b_target :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving (Show, Generic, Read)

instance FromJSON Board
instance ToJSON Board

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = Play { board :: Board,
                    turn   :: Col,
                    ai_colour :: Col,
                    game_mode :: GameMode,
                    rule :: Rule
                  }
              | Menu {
                  size :: Int,
                  target :: Int,
                  game_mode :: GameMode,
                  ai_color :: Col
                  }
              | Victory { winner :: Maybe Col }
    deriving (Show, Generic, Read)

data Save = File { s_board :: Board,
                    s_turn   :: Col,
                    s_ai_colour :: Col,
                    s_game_mode :: GameMode,
                    s_Rule :: Rule
                  }
                deriving (Show, Generic, Read)

instance FromJSON Save
instance ToJSON Save

initWorld = Play initBoard Black White PvE Regular

----------------
-- GAME LOGIC --
----------------

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there, or breaks the Rule applied)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board col pos | fst pos < 0 = Nothing
                       | snd pos < 0 = Nothing
                       | fst pos > (b_size board) - 1 = Nothing
                       | snd pos > (b_size board) - 1 = Nothing
                       | elem (pos, col) (pieces board) = Nothing
                       | otherwise = Just (Board (b_size board) (b_target board) ((pos, col) : (pieces board)))

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> [(Position, Col)]-> Maybe Col
checkWon board [] = Nothing
checkWon board (x:xs) = if checkDirections board x
                            then Just (snd x)
                            else checkWon board xs

-- Check every direction for a winning row
-- build a [Bool] with the value for every direction
-- if list contains a True value return True else return False
checkDirections :: Board -> (Position, Col) -> Bool
checkDirections board piece =
    or [checkDirection board (b_target board) (x, y) piece | x <- [-1, 0, 1],
                                                           y <- [-1, 0, 1],
                                                           (x, y) /= (0, 0)]

-- This function implements the hint provided below
-- Params are the board, n in a row, the direction of travel, a piece
-- returns true if a winning row exists
-- return false if no row exists in the given direction
checkDirection :: Board -> Int -> Direction -> (Position, Col) -> Bool
checkDirection board 1 (dirX, dirY) ((x,y), col) = True
checkDirection board n (dirX, dirY) ((x,y), col)
                  = if elem ((x - dirX, y - dirY), col) (pieces board)
                        then checkDirection board (n - 1) (dirX, dirY) ((x - dirX, y - dirY), col)
                        else False

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
-- calls eval if no winner
-- sets winners to +ve a million or -ve a million
evaluate :: Board -> Col -> Int
evaluate b c = case checkWon b (pieces b) of Nothing -> evalBoard b c
                                             Just x -> 10 ^ (b_size b)

-- Evaluates a board with no winners checking all nodes for lines of
-- consecutive colours
evalBoard :: Board -> Col -> Int
evalBoard b c = sum [evalPiece b (pos, col) | (pos, col) <- filter ((== c).snd) (pieces b)]

-- evaluate all lines that start at a piece
-- line is consective pieces of the same colour
evalPiece :: Board -> (Position, Col) -> Int
evalPiece b (pos, col) = sum [evalDirection b 1 dir (pos, col) | dir <- (getDirectionsToEval b col (pos, col))]

-- This function returns a list of direction in which a piece of the same colour
-- is not present
getDirectionsToEval :: Board -> Col -> (Position, Col) -> [Direction]
getDirectionsToEval b c piece = [ oppDir (x, y) | x <- [-1, 0, 1],
                                                  y <- [-1, 0, 1],
                                                  (checkNextPiece b (x, y) piece) /= c]
-- This function returns the colour of the next piece in the direction passed
-- Empty is used for a position not yet occupied
checkNextPiece :: Board -> Direction -> (Position, Col) -> Col
checkNextPiece b (dirX, dirY) ((x, y), col)
                | elem ((x + dirX, y + dirY), col) (pieces b) = col
                | elem ((x + dirX, y + dirY), (other col)) (pieces b) = (other col)
                | otherwise = Empty

-- evaluate direction
-- if same colour +1
-- if other other colour
-- if blank return value
-- This function takes a board, number of consective peices, direction of travel
-- and a piece
-- It returns a value assigned to this line, 10 ^ 4 for four pieces in a row.
evalDirection :: Board -> Int -> Direction -> (Position, Col) -> Int
evalDirection b n (dirX, dirY) ((x,y), col)
                  |  elem ((x + dirX, y + dirY), col) (pieces b) -- if same colour
                        = evalDirection b (n + 1) (dirX, dirY) ((x + dirX, y + dirY), col)
                  |  elem ((x + dirX, y + dirY), (other col)) (pieces b) = 0 -- if closed line
                  | x + dirX < 0 = 0         --bounds checks
                  | y + dirY < 0 = 0         --bounds checks
                  | x + dirX >= (b_size b) = 0 --bounds checks
                  | y + dirY >= (b_size b) = 0 --bounds checks
                  | otherwise = 10 ^ n --if at the end of the line
