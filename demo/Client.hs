module Main where
import System.Environment
import DIAL.Client
import Player.Client

main :: IO ()
main = do
  args <- getArgs
  case args of
    (_:_:[]) -> fromApp ClientPlayer
    _         -> putStrLn "Usage: dial-client <ip> <port>"
