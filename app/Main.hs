module Main where

import System.Random

main :: IO ()
main = do
  file <- readFile "words.txt"
  let wordList = words file

  putStrLn "Guess the word!"
  n <- randomRIO (0, length wordList - 1)
  let secret = wordList !! n

  turn 8 secret [] []

  return ()

type Secret = String

turn :: Int -> Secret -> [Char] -> [Char] -> IO()
turn 0 secret _ _ = do
  hangman <- readFile "./levels/level8.txt"
  putStr hangman
  putStrLn "Game Over!"
  putStrLn "The word was: "
  print secret

turn n secret wrong right
  | all (`elem` right) secret = do
      putStrLn "Well done! The word was: "
      putStrLn secret
  | otherwise = do
      if (8 - n) /= 0
        then do
          hangman <- readFile ("./levels/level" ++ show (8 - n) ++ ".txt")
          putStr hangman
        else do
          putStrLn ""
      putStrLn "Current state of the word:"
      print (getWordState secret right)
      putStrLn "Wrong letters:"
      putStrLn wrong
      putStrLn "Right letters:"
      putStrLn right
      putStrLn "What is your guess?"
      c <- getLetter
      
      if c `elem` secret
        then do
          putStrLn "Correct!"
          turn n secret wrong (c:right)
        else do
          putStrLn "Incorrect"
          turn (n - 1) secret (c:wrong) right

getLetter :: IO Char
getLetter = do
  putStrLn "Please guess a character"
  cs <- getLine
  case cs of
    [c] -> return c
    _   -> getLetter


getWordState :: Secret -> [Char] -> String
getWordState [] _ = []
getWordState (ch:chs) right
  | ch `elem` right = ch : getWordState chs right
  | otherwise       = '_' : getWordState chs right
