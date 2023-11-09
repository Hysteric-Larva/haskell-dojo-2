{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module HM.A7 where

import Data.Char (isAlpha, toLower, toUpper, isLetter)
import HM.A6
import HM.Provided
import System.Directory (doesFileExist)
import Data.List (intersperse, sort)

-- Q#01
data Game = Game
    { secretWord :: String
    , currentGuess :: String
    , guessedMoves :: [Char]
    , chancesRemaining :: Int
    }


-- Q#02

repeatedMove :: Char -> Game -> Bool
repeatedMove move game = elem move (guessedMoves game)



-- Q#03

makeGame :: String -> Game
makeGame secret = Game
    { secretWord = map toUpper secret
    , currentGuess = replicate (length secret) '_'
    , guessedMoves = []
    , chancesRemaining = _CHANCES_
    }

-- Q#04

updateGame :: Char -> Game -> Game
updateGame move game = game { currentGuess = newGuess, guessedMoves = newMoves, chancesRemaining = newChances }
  where
    -- Step 1: Update the `currentGuess` field with an updated `Guess` value
    newGuess = revealLetters move (secretWord game) (currentGuess game)
    
    -- Step 2: Add the `Move` character to the `guessedMoves` field
    newMoves = move : guessedMoves game

    -- Step 3: Replace the `chancesRemaining` field with the result of `updateChances`
    newChances = updateChances move (secretWord game) (chancesRemaining game)


-- Q#05

instance Show Game where
    show game = showGameHelper (secretWord game) (guessedMoves game) (chancesRemaining game)

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper secret moves chances =
    unlines $
    [ _STARS_,
      "\tSecret Word:\t" ++ intersperse ' ' secret ++ "\n",
      "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n",
      "\tChances:\t" ++ show chances,
      _STARS_
    ]


-- Q#06


instance Show GameException where
    show InvalidChars = "Invalid characters in the secret word."
    show InvalidLength = "Invalid word length. Word must be between " ++ lb ++ " and " ++ ub ++ " characters."
      where
        lb = show $ fst _LENGTH_
        ub = show $ snd _LENGTH_
    show NotInDict = "The secret word is not in the dictionary."
    show InvalidMove = "Invalid move. You must guess a single character."
    show RepeatMove = "You have already guessed this character."
    show GameOver = "The game is over."

-- Q#07

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True value = Just value


-- Q#08

validateSecret :: (Secret -> Bool) -> GameException -> Secret -> Either GameException Secret
validateSecret predicate exception secret
    | predicate secret = Right secret -- The secret passes the predicate
    | otherwise = Left exception       -- The secret does not pass the predicate



-- Q#09

hasValidChars :: GameException -> Secret -> Either GameException Secret
hasValidChars = validateSecret (all isLetter)

isValidLength :: GameException -> Secret -> Either GameException Secret
isValidLength = validateSecret (\secret -> length secret >= fst _LENGTH_ && length secret <= snd _LENGTH_)

isInDict :: Dictionary -> GameException -> Secret -> Either GameException Secret
isInDict dict = validateSecret (\secret -> toLowerSecret secret `elem` dict)
  where
    toLowerSecret = map toLower

-- Q#10

validateNoDict :: Secret -> Either GameException Secret
validateNoDict secret = case hasValidChars InvalidChars secret of
    Left e -> Left e
    Right s -> case isValidLength InvalidLength s of
        Left e -> Left e
        Right s -> Right s

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dict secret = case validateNoDict secret of
    Left e -> Left e
    Right s -> case isInDict dict NotInDict s of
        Left e -> Left e
        Right s -> Right s




-- Q#11

processTurn :: Move -> Game -> Either GameException Game
processTurn move game
    | not (isAlpha move) = Left InvalidMove
    | repeatedMove move game = Left RepeatMove
    | chancesRemaining newGame <= 0 = Left GameOver
    | otherwise = Right newGame
  where
    newGame = updateGame move game
