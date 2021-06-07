module Main where

import           Engine
import           GHC.IO.Encoding

main :: IO ()
main = do setLocaleEncoding utf8
          startGame
