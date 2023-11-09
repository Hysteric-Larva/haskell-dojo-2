module HM.A6 where

import Data.Char (isAlpha)
import HM.Provided

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02

data GameException
    = InvalidChars
    | InvalidLength
    | NotInDict
    | InvalidMove
    | RepeatMove
    | GameOver    

-- Q#03

type LengthRange = (Int, Int)
lengthInRange :: Secret -> Bool
lengthInRange secret = let
    (minLength, maxLength) = _LENGTH_
    secretLength = length secret
  in
    secretLength >= fst _LENGTH_ && secretLength <= snd _LENGTH_

-- Q#04

invalidMove :: Move -> Bool
invalidMove move = not (isLetter move)
  where
    isLetter :: Char -> Bool
    isLetter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

-- Q#05

-- Function to reveal letters in the Guess based on the Move and Secret
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters move secret guess =
  let revealedChars = zipWith revealChar secret guess
  in revealedChars

-- Helper function to reveal a character in the Guess
revealChar :: Char -> Char -> Char
revealChar secretChar guessChar
  | secretChar == guessChar = secretChar
  | otherwise = guessChar

-- Q#06

-- Function to update Chances based on Move and Secret
updateChances :: Move -> Secret -> Chances -> Chances
updateChances move secret chances
  | move `elem` secret = chances -- If Move is in Secret, chances remain unchanged
  | otherwise = chances - 1 -- If Move is not in Secret, decrease chances by 1


-- Q#07

-- Action to set the Secret value with input hidden as the user types
setSecret :: IO ()
setSecret = do
    putStr "Enter a secret word:\t"  -- Print the prompt without a newline
    secret <- showInput True         -- Hide user input
    _ <- showInput False             -- Unhide user input
    _SPACE_                          -- Print a blank line
    return secret                    -- Return the user-provided string

