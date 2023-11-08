{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)
import Data.Char (chr, ord)
-- *** Assignment 2-1 *** --

-- Q#01

promptPlayer :: Player -> String
promptPlayer player = "Player " ++ showSquare player ++ ", please enter a row and column position: "

-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ - 1)]

-- Q#03

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']


readDigit :: Char -> Int
readDigit c
  | isDigit c = read [c]  -- Convert the character to a string and then to an Int
  | otherwise = -1

-- Q#04

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_
-- Q#05

isTied :: Board -> Bool
isTied board = all (/= E) (concat board)

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]
-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings strings = zipWith (\index str -> (chr (ordA + index), str)) [0..] strings
  where
    ordA = ord 'A'

-- Q#07
formatLine :: String -> [String] -> String
formatLine sep strings = _SEP_ ++ intercalate sep strings  ++ _SEP_


-- *** Assignment 2-2 *** --

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (row, col) = and [row >= 0, row < _SIZE_, col >= 0, col < _SIZE_]

-- Q#09
stringToMove :: String -> Move
stringToMove [col, row] | length [col, row] == 2 = (convertRowIndex col, readDigit row)
stringToMove _ = _INVALID_MOVE_

-- Q#10

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player col row
  | col < 0 || col >= length row = row  -- Return the original row unchanged for out-of-bounds column indices
  | otherwise = let (before, _:after) = splitAt col row
                in before ++ [player] ++ after

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O               