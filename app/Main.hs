module Main (main) where

import qualified Apps.HazardHunter.Main as HH (run)
import qualified Apps.Memory as Memrory (run)
import qualified Apps.Seed.Main as Seed (run)
import qualified Apps.Todo as Todo (run)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  let defaultPort = 8092
  args <- getArgs
  case args of
    ["HazardHunter"] -> HH.run defaultPort
    ["TodoList"] -> Todo.run defaultPort
    ["Memeory"] -> Memory.run defaultPort
    ["Seed"] -> Seed.run defaultPort
    _ -> putStrLn "Unknown app. Exit"
