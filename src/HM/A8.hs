module HM.A8 where

import Control.Monad (when)
import Data.Char (toUpper)
import HM.A6
import HM.A7 hiding (validateNoDict, validateWithDict)
import HM.Provided
import System.Directory (doesFileExist)

-- Q#01



getUpperChar :: IO Char
getUpperChar = fmap toUpper getChar


-- Q#02
_DICT_ :: IO Dictionary
_DICT_ = do
    fileExists <- doesFileExist _DICT_FILE_
    if fileExists
        then return words <*> readFile _DICT_FILE_
        else pure []

isDictNonEmpty :: IO Bool
isDictNonEmpty = not . null <$> _DICT_

-- Q#03

makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid e =
    case e of
        Left ge -> Left ge
        Right secret -> Right (makeGame secret)


-- Q#04

getDict :: IO (Maybe Dictionary)
getDict = toMaybe <$> isDictNonEmpty <*> _DICT_


-- Q#05

validateNoDict :: Secret -> Either GameException Secret
validateNoDict secret = hasValidChars secret >>= isValidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dictionary secret = validateNoDict secret >>= isInDict dictionary

-- Q#06

playGame :: Game -> IO ()
playGame game = do
        promptGuess
        guessMove <- getUpperChar
        _SPACE_
        case processTurn guessMove game of
            Left GameOver     -> print GameOver >>
                                 putStr "The secret word is: " >>
                                 putStrLn (getSecret game)
            Left InvalidMove  -> print InvalidMove >>
                                 putStrLn "Try again." >>
                                 playGame game
            Left RepeatMove   -> print RepeatMove >>
                                 putStrLn "Try again." >>
                                 playGame game
            Right updatedGame -> do print updatedGame
                                    if getGuess updatedGame == getSecret game
                                        then putStrLn "You win!"
                                        else putStrLn "Game continues." >>
                                             playGame updatedGame






-- Q#07

startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame secretValidate = do
        inputSecret <- secretValidate <$> setSecret
        case makeGameIfValid inputSecret of
                Left eException -> print eException >>
                                   startGame secretValidate
                Right eNewGame  -> print eNewGame >>
                                   playGame eNewGame




-- Q#08

runHM :: IO ()
runHM = do --putStrLn "Not implemented... yet!"
           mDictionary <- getDict
           case mDictionary of
                Just dictionary -> startGame $ validateWithDict dictionary
                Nothing         -> do putStrLn "Missing dictionary! Continue without dictionary? [Y/N]"
                                      continue <- getUpperChar
                                      when (continue == 'Y') $ startGame validateNoDict
