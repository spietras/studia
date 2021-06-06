-- Type and data definitions

module Types where

type Action = String
type QuestAction = Action
type MoveAction = Action
type ExtraAction = Action

type Quest = GameState -> IO (Maybe Int)

type Map a b = [(a, b)]

type PlaceId = Int
type PlacePrompt = String
type MoveMap = Map MoveAction Place
type ExtrasMap = Map ExtraAction String
data Place = Place PlaceId  -- numerical id of place
                   PlacePrompt  -- prompt for visiting place
                   QuestAction  -- command for taking quest
                   Quest  -- quest handler
                   MoveMap  -- map of possible place changing commands
                   ExtrasMap  -- map of extra interactions

type ScoreValue = Int
data QuestScore = QuestScore ScoreValue
type ScoreMap = Map PlaceId QuestScore
data GameState = GameState ScoreMap  -- map of scores associated with each place after completing quests
