module A3 where

import A1
import A2

import Data.List (transpose)

-- *** Assignment 3-1 ***

-- Q#01

showInts :: [Int] -> [String]
showInts [] = []  -- Base case: an empty list results in an empty list of strings
showInts (x:xs) = show x : showInts xs  -- Convert the head of the list to a string and recursively process the tail



_HEADER_ :: String
_HEADER_ = formatLine _SEP_ (showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares [] = []  -- Base case: an empty list results in an empty list of strings
showSquares (x:xs) = showSquare x : showSquares xs  -- Convert the head of the list to a string and recursively process the tail



-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []  -- Base case: an empty list results in an empty list of strings
formatRows (row:rows) = formatLine _SEP_ (showSquares row) : formatRows rows  -- Format the current row and recursively process the rest


-- Q#04

isColEmpty :: Row -> Int -> Bool
-- Base pattern 1: If the row is empty, no square is present, so return False
isColEmpty [] _ = False
-- Base pattern 2: If the index is 0 and the first square is empty, return True
isColEmpty (x:_) 0 = x == E
-- Recursive pattern: Check the rest of the row with a decremented index
isColEmpty (_:xs) col = isColEmpty xs (col - 1)


-- Q#05

-- Function to remove the first column from the board
dropFirstCol :: Board -> Board
dropFirstCol [] = []  -- Base case: An empty board remains empty
dropFirstCol (row:rows) = case row of
  [] -> []  -- Base case: An empty row remains empty
  (_:xs) -> xs : dropFirstCol rows  -- Remove the first square from the row and proceed to the next row

-- Function to remove the last column from the board
dropLastCol :: Board -> Board
dropLastCol [] = []  -- Base case: An empty board remains empty
dropLastCol rows = map init rows  -- Use `map` and `init` to remove the last square from each row


-- Q#06

type Diagonal = [Square]

-- Function to get the diagonal from top left to bottom right
getDiag1 :: Board -> Line
getDiag1 [] = []  -- Base case: An empty board has an empty diagonal
getDiag1 (row:rows) = case row of
  [] -> []  -- Base case: An empty row has an empty diagonal
  (x:_) -> x : getDiag1 (map tail rows)  -- Get the first square and proceed to the next row by dropping the first column

-- Function to get the diagonal from top right to bottom left
getDiag2 :: Board -> Line
getDiag2 [] = []  -- Base case: An empty board has an empty diagonal
getDiag2 rows = case reverse rows of
  [] -> []  -- Base case: An empty row has an empty diagonal
  (row:rows) -> case  row of
    [] -> []  -- Base case: An empty row has an empty diagonal
    (x:_) -> x : getDiag2 (map init rows)  -- Get the last square and proceed to the next row by dropping the last column







-- Function to get all lines from a board
getAllLines :: Board -> [Line]
getAllLines board = horizontalLines ++ verticalLines ++ diagonalLines
  where
    -- Horizontal lines are just the rows of the board
    horizontalLines = board

    -- Vertical lines can be obtained by transposing the board
    verticalLines = transpose board

    -- Diagonal lines
    diagonalLines = [getDiag1 board, getDiag2 board]

-- *** Assignment 3-2 ***

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []  -- Base case: An empty board remains empty
putSquare player (row:rows) (r, c)
  | r == 0    = replaceSquareInRow player c row : rows  -- If we're on the correct row, replace the square and continue
  | otherwise = row : putSquare player rows (r - 1, c)  -- Otherwise, continue with the current row

-- Q#08

-- Function to prepend row indices to a list of strings
prependRowIndices :: [String] -> [String]
prependRowIndices strings = prependRowIndicesWorker (indexRowStrings strings)
  where
    -- Worker function for recursive processing
    prependRowIndicesWorker :: [(Char, String)] -> [String]
    prependRowIndicesWorker [] = []  -- Base case: An empty list of pairs results in an empty list of strings
    prependRowIndicesWorker ((index, str):rest) = (index : str) : prependRowIndicesWorker rest


-- Q#09

-- Function to check if a line is winning for a player
isWinningLine :: Player -> Line -> Bool
isWinningLine player line = isWinningLineWorker False line
  where
    -- Worker function with an accumulator
    isWinningLineWorker :: Bool -> Line -> Bool
    isWinningLineWorker acc [] = acc  -- Base case: Return the accumulator
    isWinningLineWorker acc (square:squares)
      | square == player = isWinningLineWorker True squares  -- Square matches player, continue with True accumulator
      | otherwise = False  -- Square does not match player, return False immediately


-- Q#10
-- Function to check if a move is valid on the board
isValidMove :: Board -> Move -> Bool
isValidMove board move = isMoveInBounds move && isValidMoveWorker board move
  where
    -- Worker function to check if the square at the move's coordinates is empty
    isValidMoveWorker :: Board -> Move -> Bool
    isValidMoveWorker [] _ = False  -- Base case: An empty board can't have a valid move
    isValidMoveWorker (row:rows) (r, c)
      | r == 0 = isColEmpty row c   -- If we're on the correct row, check if the column is empty
      | otherwise = isValidMoveWorker rows (r - 1, c)  -- Otherwise, continue with the next row


