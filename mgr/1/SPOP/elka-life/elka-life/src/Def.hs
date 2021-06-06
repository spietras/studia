-- Game scenario definitions

module Def(start, help, quit, badInput, availableCommands) where

import           System.Exit

import           Map
import           Types
import           Utils

questCompleted :: GameState -> PlaceId -> Bool
questCompleted (GameState map) placeId = mapKeyExists map placeId

questsCompleted :: GameState -> [PlaceId] -> Bool
questsCompleted state [] = True
questsCompleted state (x:xs) = (questCompleted state x) && (questsCompleted state xs)

dormQuest :: Quest
dormQuest state = if (questCompleted state 0)
                      then do putStrLn "Quest already completed."
                              return Nothing
                      else do putStrLn "dormQuest"
                              return (Just 10)

dorm :: Place
dorm = Place 0
             "Dorm"
             "quest"
             dormQuest
             [("F", faculty), ("P", project)]
             []

projectQuest :: Quest
projectQuest state = if (questCompleted state 1)
                         then do putStrLn "Quest already completed."
                                 return Nothing
                         else do putStrLn "projectQuest"
                                 return (Just 10)

project :: Place
project = Place 1
                "Project"
                "quest"
                projectQuest
                [("D", dorm)]
                []

facultyQuest :: Quest
facultyQuest state = if not (questsCompleted state [0, 1, 3, 4])
                         then do putStrLn "Complete all the other quests first."
                                 return Nothing
                         else do putStrLn "facultyQuest"
                                 putStrLn "Game Over"
                                 exitSuccess

faculty :: Place
faculty = Place 2
                "Faculty"
                "quest"
                facultyQuest
                [("D", dorm), ("T", test), ("L", lab)]
                []

testQuest :: Quest
testQuest state = if (questCompleted state 3)
                      then do putStrLn "Quest already completed."
                              return Nothing
                      else do putStrLn "testQuest"
                              return (Just 10)

test :: Place
test = Place 3
             "Test"
             "quest"
             testQuest
             [("F", faculty), ("L", lab)]
             []

labQuest :: Quest
labQuest state = if (questCompleted state 4)
                     then do putStrLn "Quest already completed."
                             return Nothing
                     else do putStrLn "labQuest"
                             return (Just 10)

lab :: Place
lab = Place 4
            "Test"
            "quest"
            labQuest
            [("F", faculty), ("T", test)]
            []


start :: Place
start = dorm

help :: Action
help = "HELP"

quit :: Action
quit = "QUIT"

badInput :: String
badInput = "Bad input."

availableCommands :: [Action] -> String
availableCommands cmds = "Available commands: " ++ (prettyStrings cmds)
