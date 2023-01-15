module Main (main) where

import qualified Apps.HazardHunter.Main as HH (run)
import qualified Apps.Todo as Todo (run)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["HazardHunter"] -> HH.run 8092
    ["TodoList"] -> Todo.run 8092
    _ -> putStrLn "Unknown app. Exit"
