module Main where
import DIAL.Server.State
import Player.Server
import qualified Data.Map as M
import Control.Applicative

main :: IO ()
main = do
  mapping <- M.singleton "Player" <$> newPlayer
  toServer defaultConfig mapping "apps"
