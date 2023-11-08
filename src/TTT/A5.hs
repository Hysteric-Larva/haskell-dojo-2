module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)
import System.IO
-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO ()
printBoard board = putStrLn (formatBoard board)


-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo = do
  logoContents <- readFile _LOGO_PATH_
  putStr logoContents

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen


firstPlayer :: IO Player
firstPlayer = do
  isX <- _RANDOM_BOOL_
  return (getFirstPlayer isX)

-- Q#04

getMove :: Board -> IO Move
getMove board = do
  putStrLn "Enter your move (row and column, e.g., 'A1'): "
  userString <- getLine
  moveResult <- getMoveWorker board userString
  case moveResult of
    Right validMove -> return validMove
    Left errorMessage -> do
      putStrLn errorMessage
      getMove board

getMoveWorker :: Board -> String -> IO (Either String Move)
getMoveWorker board userString = do
  let move = stringToMove userString
  if isValidMove board move
    then return (Right move)
    else return (Left "Invalid move. Please try again.")






-- Q#05

play :: Board -> Player -> IO ()
play board player = do
  when _DISPLAY_LOGO_ printLogo
  putStrLn " "
  printBoard board
  putStrLn (promptPlayer player)

  move <- getMove board
  let (newGameState, newBoard) = playMove player board move

  case newGameState of
    InProgress -> do
      let nextPlayer = switchPlayer player
      play newBoard nextPlayer
    _ -> do
      printBoard newBoard
      putStrLn (showGameState newGameState)

-- Q#06






-- *** Assignment 5-2 *** --

-- Q#07


printLogoDo :: IO ()
printLogoDo = do
  logoContents <- readFile _LOGO_PATH_
  putStr logoContents


-- Q#08

firstPlayerDo :: IO Player
firstPlayerDo = do
  isX <- _RANDOM_BOOL_
  return (getFirstPlayer isX)


-- Q#09


getMoveDo :: Board -> IO Move
getMoveDo board = do
  putStrLn "Enter your move (row and column, e.g., 'A1'): "
  userString <- getLine

  let move = stringToMove userString

  if isValidMove board move
    then return move
    else do
      putStrLn "Invalid move. Please try again."
      getMove board

-- Q#10

playDo :: Board -> Player -> IO ()
playDo board player = do
  when _DISPLAY_LOGO_ printLogo

  printBoard board
  putStrLn (promptPlayer player)

  move <- getMove board
  let (newGameState, newBoard) = playMove player board move

  case newGameState of
    InProgress -> do
      let nextPlayer = switchPlayer player
      play newBoard nextPlayer
    _ -> do
      printBoard newBoard
      putStrLn (showGameState newGameState)
