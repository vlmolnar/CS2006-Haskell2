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

-- AI that stores the colour of the AI and level of difficulty
data AI = AI {
              ai_colour :: Col,
              ai_level :: Int
            }
      deriving (Show, Eq, Generic, Read)
instance FromJSON AI
instance ToJSON AI

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
    | x + dirX < 0 = False          --bounds checks
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
                     b_rule :: Rule,
                     pieces :: [(Position, Col)]
                   }
  deriving (Show, Generic, Read)

instance FromJSON Board
instance ToJSON Board

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 Regular []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = Play {
                    board :: Board,
                    turn   :: Col,
                    ai :: [AI],
                    game_mode :: GameMode
                  }
              | Menu {
                  size :: Int,
                  target :: Int,
                  ai :: [AI],
                  game_mode :: GameMode
                  }
              | Victory { winner :: Maybe Col }
    deriving (Show, Generic, Read)

data Save = File { s_board :: Board,
                    s_turn   :: Col,
                    s_ai :: [AI],
                    s_game_mode :: GameMode
                  }
                deriving (Show, Generic, Read)

instance FromJSON Save
instance ToJSON Save

initWorld = (Menu 6 3 [(AI White 2), (AI Black 1)] PvE)

----------------
-- GAME LOGIC --
----------------

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there, or breaks the Rule applied)
makeMove :: Board -> Col -> Position -> Rule -> Maybe Board
makeMove b col pos r  | fst pos < 0 = Nothing
                      | snd pos < 0 = Nothing
                      | fst pos > (b_size b) - 1 = Nothing
                      | snd pos > (b_size b) - 1 = Nothing
                      | elem (pos, col) (pieces b) = Nothing
                      | otherwise
                            = if (enforceRules b r col pos)
                                then Just (Board (b_size b) (b_target b) (b_rule b) ((pos, col) : (pieces b)))
                                else Nothing
-----------------
-- HOUSE RULES --
-----------------

-- This function check for the rules chosen by the user at the command line
-- It preemptively places the move on the board and checks whether it conforms
-- to the rules chosen
-- Regular: no rules chosen
-- Three: three and three rule
-- Four: Four and Four
enforceRules :: Board -> Rule -> Col -> Position -> Bool
enforceRules b Regular c p = True
enforceRules b Three c p = houseRule (Board s t r ps) ps 3
                                      where ps = ((p,c) : (pieces b))
                                            s = (b_size b)
                                            t = (b_target b)
                                            r = (b_rule b)
enforceRules b Four c p = houseRule (Board s t r ps) ps 4
                                      where ps = ((p,c) : (pieces b))
                                            s = (b_size b)
                                            t = (b_target b)
                                            r = (b_rule b)

-- This function checks that each piece is the list of pieces conforms to the rule
-- for each piece in list return true is pass or false if not
houseRule :: Board -> [(Position, Col)] -> Int -> Bool
houseRule b [] n = True
houseRule b (x:xs) n = if houseRulePiece b x n
                          then houseRule b xs n
                          else False

-- This function check all the directions that could fail the rule
-- Only directions that would have a line with both ends open are checked
-- @getDirections returns a list of directions that pass the function provided
-- b = board, p = piece,
houseRulePiece :: Board -> (Position, Col) -> Int -> Bool
houseRulePiece b (pos, col) n =
  and [houseRuleDirection b n (oppDir dir) (pos, col) |
          dir <- getDirections func b (pos, col)]
                  where func = (\b p x y -> (checkNextPiece b (x, y) p) == Empty)

houseRuleDirection :: Board -> Int -> Direction -> (Position, Col) -> Bool
houseRuleDirection b 1 dir piece = if checkNextPiece b dir piece == Empty then False else True
houseRuleDirection b n (dirX, dirY) ((x, y), col)
                                | checkNextPiece b (dirX, dirY) ((x, y), col) == col -- if same colour
                                    = houseRuleDirection b (n - 1) (dirX, dirY) ((x + dirX, y + dirY), col)
                                | checkNextPiece b (dirX, dirY) ((x, y), col) == (other col)
                                    = True
                                | checkNextPiece b (dirX, dirY) ((x, y), col) == Empty
                                    = True
                                | boundsCheck (b_size b) ((x, y), col) (dirX, dirY) = True
                                | otherwise = False -- empty

----------------------
-- CHECK FOR WINNER --
----------------------
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
checkDirections board (pos, col) =
    or [checkDirection board (b_target board) dir (pos, col) |
          dir <- getDirections func board (pos, col)]
                    where func = (\b p x y -> (x,y) /= (0, 0))

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


-------------------------------
-- EVALUATE THE BOARD FOR AI --
-------------------------------

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
evalPiece b (pos, col) =
        sum [evalDirection b 0 (oppDir dir) (pos, col) |
                dir <- getDirections func b (pos, col)]
                      where func = (\b p x y -> (checkNextPiece b (x, y) p) /= col)

-- evaluate direction
-- if same colour +1
-- if other other colour
-- if blank return value
-- This function takes a board, number of consective peices, direction of travel
-- and a piece
-- It returns a value assigned to this line, 10 ^ 4 for four pieces in a row.
evalDirection :: Board -> Int -> Direction -> (Position, Col) -> Int
evalDirection b n (dirX, dirY) ((x,y), col)
                  | checkNextPiece b (dirX, dirY) ((x, y), col) == col -- if same colour
                        = evalDirection b (n + 1) (dirX, dirY) ((x + dirX, y + dirY), col)
                  | checkNextPiece b (dirX, dirY) ((x, y), col) == (other col) = 0 -- if closed line
                  | boundsCheck (b_size b) ((x, y), col) (dirX, dirY) = 0
                  | otherwise = 10 ^ n --if at the end of the line


-- This function returns a list of directions dependent on the predicate passed
-- check for win uses: (x, y) /= (0, 0) -- all 8 directions (N, NE, E, SE, S, SW, W, NW)
-- Eval board uses: (checkNextPiece b (x, y) piece) /= c -- does the next piece that direction have the same colour
-- House rules uses: (checkNextPiece b (x, y) piece) == Empty
getDirections :: (Board -> (Position, Col) -> Int -> Int -> Bool) -> Board -> (Position, Col) -> [Direction]
getDirections predicate b (pos, col) = [ (x, y) | x <- [-1, 0, 1],
                                                  y <- [-1, 0, 1],
                                                  predicate b (pos, col) x y]

-- This function returns the colour of the next piece in the direction passed
-- Empty is used for a position not yet occupied
checkNextPiece :: Board -> Direction -> (Position, Col) -> Col
checkNextPiece b (dirX, dirY) ((x, y), col)
                | elem ((x + dirX, y + dirY), col) (pieces b) = col
                | elem ((x + dirX, y + dirY), (other col)) (pieces b) = (other col)
                | otherwise = Empty
