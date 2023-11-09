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
 { getSecret  :: Secret,
  getGuess   :: Guess,
  getMoves   :: [Move],
  getChances :: Chances }


-- Q#02

repeatedMove :: Move -> Game -> Bool
repeatedMove move game = if move `elem` getMoves game
                            then True
                            else False


-- Q#03

makeGame :: Secret -> Game
makeGame secret = Game { getSecret  = map toUpper secret
                       , getGuess   = map (const '_') secret
                       , getMoves   = []
                       , getChances = _CHANCES_ }

-- Q#04

updateGame :: Move -> Game -> Game
updateGame move game = Game { getSecret  = getSecret game
                            , getGuess   = revealLetters move (getSecret game) (getGuess game)
                            , getMoves   = getMoves game ++ [move]
                            , getChances = updateChances move (getSecret game) (getChances game) }


-- Q#05

instance Show Game where
    show :: Game -> String
    show game = showGameHelper currentGuess currentMoves currentChances
                  where
                      currentGuess :: Guess
                      currentGuess   = getGuess game
                      currentMoves :: [Move]
                      currentMoves   = if null moves 
                                          then "[]"
                                          else moves
                                                where
                                                    moves :: [Move]
                                                    moves = getMoves game
                      currentChances :: Chances
                      currentChances = getChances game

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances =
    unlines
        [ _STARS_,
          "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n",
          "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n",
          "\tChances:\t" ++ show chances,
          _STARS_
        ]



-- Q#06


instance Show GameException where
    show :: GameException -> String
    show InvalidChars  = "Invalid Characters Exception: There are invalid characters in the input."
    show InvalidLength = 
        concat ["Invalid String Length Exception: Secret word length must be between ", lb, " and ", ub, "."]
            where
                lb :: String
                lb = show $ fst _LENGTH_
                ub :: String
                ub = show $ snd _LENGTH_
    show NotInDict     = "Not In Dictionary Exception: Word is not found in the dictionary."
    show InvalidMove   = "Invalid Move Eception: Move input is invalid." 
    show RepeatMove    = "Repeated Move Exception: This move has been entered previously."
    show GameOver      = "Game Over Exception: No more chances left. Game is lost."

-- Q#07

toMaybe :: Bool -> a -> Maybe a
toMaybe bool a = if bool
                      then Just a
                      else Nothing


-- Q#08

validateSecret :: (Secret -> Bool) -> GameException -> Secret -> Either GameException Secret
validateSecret validate exception secret = if validate secret
                                                then Right secret
                                                else Left exception



-- Q#09
hasValidChars :: Secret -> Either GameException Secret
hasValidChars = validateSecret checkAllIsAlpha InvalidChars
                            where
                                checkAllIsAlpha :: Secret -> Bool
                                checkAllIsAlpha xs = all isAlpha xs

isValidLength :: Secret -> Either GameException Secret
isValidLength = validateSecret lengthInRange InvalidLength

isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict dictionary = validateSecret (\xs -> map toLower xs `elem` dictionary) NotInDict

-- Q#10
validateNoDict :: Secret -> Either GameException Secret
validateNoDict secret = case hasValidChars secret of
                            Left eException -> Left eException
                            Right eSecret   -> isValidLength eSecret

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dictionary secret = case validateNoDict secret of
                                            Left eException -> Left eException
                                            Right eSecret   -> isInDict dictionary eSecret




-- Q#11

processTurn :: Move -> Game -> Either GameException Game
processTurn move game
    | invalidMove move            = Left InvalidMove
    | repeatedMove move game      = Left RepeatMove
    | getChances updatedGame == 0 = Left GameOver
    | otherwise                   = Right updatedGame
        where
            updatedGame :: Game
            updatedGame = updateGame move game
