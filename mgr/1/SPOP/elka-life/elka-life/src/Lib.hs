module Lib
    ( start
    ) where

import System.Exit

data GameState = GameState
  {questScores :: [Maybe Int]
  } deriving Show

setScore :: [Maybe Int] -> Int -> Int -> [Maybe Int]
setScore (x:xs) score pos = case pos of 
                                0 -> Just score : xs
                                _ -> x : (setScore xs score (pos - 1))

start :: IO ()
start = faculty (GameState [Nothing, Nothing])

questFaculty :: GameState -> IO GameState
questFaculty state = do putStrLn "questFaculty"
                        return (GameState (setScore (questScores state) 10 0))

faculty :: GameState -> IO ()
faculty state = do putStrLn "faculty"
                   print state
                   input <- getLine
                   case input of
                       "quest" -> do newState <- (questFaculty state)
                                     print newState
                                     faculty newState
                       "dorm" -> dorm state
                       _ -> do putStrLn "quest or dorm"
                               faculty state


dorm :: GameState -> IO ()
dorm state = do putStrLn "dorm" 
                input <- getLine
                faculty state

