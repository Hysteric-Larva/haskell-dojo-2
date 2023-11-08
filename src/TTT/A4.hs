module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01
-- Refactored _HEADER_ using map
_HEADER_ = ' ' :formatLine _SEP_ (showInts _RANGE_)

-- Q#02
-- Refactored showSquares using map
showSquares :: [Square] -> [String]
showSquares squares = map showSquare squares
-- Q#03
-- Refactored dropFirstCol using map
dropFirstCol :: Board -> Board
dropFirstCol board = map tail board
-- Q#04
-- Refactored dropLastCol using map
dropLastCol :: Board -> Board
dropLastCol board = map init board

--Q#05

-- Refactored formatRows using a lambda expression
formatRows :: [Row] -> [String]
formatRows = map (\row -> formatLine _SEP_ (showSquares row))

-- Q#06

-- New version of isWinningLine using filter
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ player line
  | null line = False  -- An empty line is not a winning line
  | otherwise = null (filter (/= player) line)


-- *** Assignment 4-2 *** --





-- Q#07

isWinningLine :: Player -> Line -> Bool
isWinningLine player line
  | null line = False  -- An empty line is not a winning line
  | otherwise = foldr (\square acc -> (square == player) && not acc) False line


-- Q#08

hasWon :: Player -> Board -> Bool
hasWon player board = foldr checkLine False (getAllLines board)
  where
    checkLine :: Line -> Bool -> Bool
    checkLine line acc
      | acc = True  -- If a winning line has been found, no need to check further
      | isWinningLine_ player line = True  -- If the current line is a winning line
      | otherwise = acc  -- Continue checking with the remaining lines



-- Q#09

getGameState :: Board -> GameState
getGameState board
  | hasWon X board = XWon
  | hasWon O board = OWon
  | isTied board = Tie
  | otherwise = InProgress



playMove :: Player -> Board -> Move -> (GameState, Board)
playMove player board move =
  let updatedBoard = putSquare player board move
  in (getGameState updatedBoard, updatedBoard)

-- Q#10

prependRowIndices :: [String] -> [String]
prependRowIndices strings = zipWith (\index str -> index : str) ['A'..'Z'] strings



-- Q#11

formatBoard :: Board -> String
formatBoard = unlines . ( _HEADER_ :) . prependRowIndices . formatRows

_X_WIN_ = [ [X, O, O]
              , [O, X, O]
              , [O, O, X]
              ]

_O_WIN_ = [ [O, X, O]
              , [X, X, O]
              , [X, O, O]
              ]