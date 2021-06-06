-- Main game processing logic

module Engine(
    startGame
) where

import           System.Exit

import           Def
import           Map
import           Types

-- starts the game loop with initial conditions
startGame :: IO ()
startGame = gameLoop start (GameState [])

-- changes score associated with place's quest in game state
setScore :: GameState -> PlaceId -> QuestScore -> GameState
setScore (GameState map) i score = GameState (mapSet map i score)

-- main game loop where you can move around places and interact with things
gameLoop :: Place -> GameState -> IO ()
gameLoop (Place placeId prompt questAction quest moveMap extrasMap) state =
    do putStrLn prompt  -- print place's prompt
       input <- getLine  -- get command from user
       let parse x | x == quit          = exitSuccess
                   | x == help          =  -- print all available commands in current situation after help command
                         putStrLn (availableCommands ([quit, help, questAction] ++ (mapKeys moveMap) ++ (mapKeys extrasMap)))
                   | (x == questAction) =  -- perform quest on quest action
                         do score <- quest state
                            case score of
                                Just value -> gameLoop (Place placeId prompt questAction quest moveMap extrasMap)
                                                       (setScore state placeId (QuestScore value))
                                Nothing -> return ()  -- quest locked
                   | otherwise          =
                         case (mapLookup moveMap input) of  -- check for place transition
                             Just place -> gameLoop place state
                             Nothing    -> case (mapLookup extrasMap input) of  -- check for extra interactions
                                               Just text -> do putStrLn text
                                               Nothing   -> do putStrLn badInput
       parse input
       gameLoop (Place placeId prompt questAction quest moveMap extrasMap) state  -- repeat current situation on interactions that don't change state
