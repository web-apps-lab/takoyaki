module Main (main) where

import qualified Apps.HazardHunter.Main as HH (run)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  let defaultPort = 8092
  args <- getArgs
  case args of
    ["HazardHunter"] -> HH.run defaultPort
    _ -> putStrLn "Unknown app. Exit"
