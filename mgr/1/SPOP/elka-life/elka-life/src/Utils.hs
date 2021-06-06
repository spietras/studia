-- Utilities

module Utils where

-- Pretty string from multiple strings
-- Example: ["A", "B", "C"] -> "A, B, C"
prettyStrings :: [String] -> String
prettyStrings []     = []
prettyStrings (x:[]) = x
prettyStrings (x:xs) = x ++ ", " ++ (prettyStrings xs)
