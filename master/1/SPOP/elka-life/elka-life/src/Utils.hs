-- Utilities

module Utils where

-- Pretty string from multiple strings
-- Example: ["A", "B", "C"] -> "A, B, C"
prettyStrings :: [String] -> String
prettyStrings []     = []
prettyStrings (x:[]) = x
prettyStrings (x:xs) = x ++ ", " ++ (prettyStrings xs)

putLn :: IO ()
putLn = putStrLn ""

putLnStrLn :: String -> IO ()
putLnStrLn s = do putLn
                  putStrLn s

putStrLnLn :: String -> IO ()
putStrLnLn s = do putStrLn s
                  putLn

putLnStrLnLn :: String -> IO ()
putLnStrLnLn s = do putLn
                    putStrLnLn s
