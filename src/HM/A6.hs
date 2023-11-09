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


lengthInRange :: Secret -> Bool
lengthInRange secret = lengthSecret >= fst _LENGTH_ && lengthSecret <= snd _LENGTH_ 
                            where
                                lengthSecret :: Int
                                lengthSecret = length secret
                                
-- Q#04

invalidMove :: Move -> Bool
invalidMove move = not $ isAlpha move

-- Q#05

-- Function to reveal letters in the Guess based on the Move and Secret
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters move secret guess = zipWith (\s g -> if s == move then s else g) secret guess

-- Q#06

-- Function to update Chances based on Move and Secret
updateChances :: Move -> Secret -> Chances -> Chances
updateChances move secret chances = if move `notElem` secret 
                                        then chances - 1
                                        else chances


-- Q#07

-- Action to set the Secret value with input hidden as the user types
setSecret :: IO String 
setSecret = do putStr "Enter a secret word:\t"
               showInput False
               secret <- getLine
               showInput True
               _SPACE_
               return secret
