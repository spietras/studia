module Lib
    ( start
    ) where

import System.Exit

data GameState = GameState
  {questScores :: [Maybe Int]
  } deriving Show

checkQuestPassed :: GameState -> Int -> Bool
checkQuestPassed state pos = case ((questScores state)!!pos) of 
                                Nothing -> False
                                _ -> True

setScore :: [Maybe Int] -> Int -> Int -> [Maybe Int]
setScore (x:xs) score pos = case pos of 
                                0 -> Just score : xs
                                _ -> x : (setScore xs score (pos - 1))

start :: IO ()
start = dorm (GameState [Nothing, Nothing, Nothing, Nothing])

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
                       "test" -> test state
                       "lab" -> lab state
                       _ -> do putStrLn "TODO: pomocy - co wpisac"
                               faculty state


questDorm :: GameState -> IO GameState
questDorm state = do putStrLn "questDorm"
                     return (GameState (setScore (questScores state) 10 0))

dorm :: GameState -> IO ()
dorm state = do putStrLn "dorm"
                print state
                input <- getLine
                case input of
                    "quest" -> do newState <- (questDorm state)
                                  print newState
                                  dorm newState
                    "faculty" -> faculty state
                    "project" -> project state
                    _ -> do putStrLn "TODO: pomocy - co wpisac"
                            dorm state

questProject :: GameState -> IO GameState
questProject state = do putStrLn "questProject"
                        return (GameState (setScore (questScores state) 10 1))

project :: GameState -> IO ()
project state = do putStrLn "project"
                   print state
                   input <- getLine
                   case input of
                       "quest" -> do newState <- (questProject state)
                                     print newState
                                     project newState
                       "dorm" -> dorm state
                       _ -> do putStrLn "TODO: pomocy - co wpisac"
                               project state

questTest :: GameState -> IO GameState
questTest state = do putStrLn "questTest"
                     return (GameState (setScore (questScores state) 10 2))

test :: GameState -> IO ()
test state = do putStrLn "test"
                print state
                input <- getLine
                case input of
                    "quest" -> do newState <- (questTest state)
                                  print newState
                                  test newState
                    "faculty" -> faculty state
                    "lab" -> lab state
                    _ -> do putStrLn "TODO: pomocy - co wpisac"
                            test state

questLab :: GameState -> IO GameState
questLab state = if (checkQuestPassed state 3)
                    then do putStrLn "Niestety, do laboratorium mogłeś podejść tylko raz. Niech to będzie dla Ciebie nauczka na przyszłość!"
                            return state
                    else do putStrLn "questLab"
                            return (GameState (setScore (questScores state) 10 3))

lab :: GameState -> IO ()
lab state = do putStrLn "lab"
               print state
               input <- getLine
               case input of
                   "quest" -> do newState <- (questLab state)
                                 print newState
                                 lab newState
                   "faculty" -> faculty state
                   "test" -> test state
                   _ -> do putStrLn "TODO: pomocy - co wpisac"
                           lab state