module HM.A8 where

import Control.Monad (when)
import Data.Char (toUpper)
import HM.A6
import HM.A7 hiding (validateNoDict, validateWithDict)
import HM.Provided
import System.Directory (doesFileExist)

-- Q#01

import Data.Char (toUpper)

getUpperChar :: IO Char
getUpperChar = toUpper <$> getChar


-- Q#02
-- Define the filename
_DICT_FILE :: FilePath
_DICT_FILE = "dictionary.txt"
-- Define _DICT_
_DICT_ :: IO Dictionary
_DICT_ = do
    fileExists <- doesFileExist _DICT_FILE
    if fileExists
        then words <$> readFile _DICT_FILE
        else pure []

-- Define isDictNonEmpty
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
validateNoDict secret =
    hasValidChars InvalidChars secret >>=
    isValidLength InvalidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dict secret =
    validateNoDict secret >>=
    isInDict dict NotInDict

-- Q#06

playGame :: Game -> IO ()
playGame game = do
    promptGuess
    move <- getUpperChar
    _SPACE_
    case processTurn move game of
        Left GameOver -> do
            putStrLn $ show GameOver
            putStrLn $ "The correct word was: " ++ secretWord game
            return ()
        Left e -> do
            putStrLn $ show e
            playGame game
        Right updatedGame -> do
            putStrLn $ show updatedGame
            if currentGuess updatedGame == secretWord updatedGame
                then putStrLn "Congratulations! You've won the game."
                else playGame updatedGame






-- Q#07

startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame validator = do
    putStrLn "Enter a secret word: "
    secretWord <- getLine
    let result = validator secretWord
    case makeGameIfValid result of
        Left e -> do
            putStrLn $ show e
            startGame validator
        Right game -> do
            putStrLn $ show game
            playGame game




-- Q#08

runHM :: IO ()
runHM = do
    maybeDict <- getDict
    case maybeDict of
        Just dict -> startGame (validateWithDict dict)
        Nothing -> do
            putStrLn "Missing dictionary! Continue without dictionary? [Y/N]"
            choice <- getUpperChar
            when (choice == 'Y') $ startGame validateNoDict
