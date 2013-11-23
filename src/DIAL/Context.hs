module DIAL.Context where

data Transaction = Unsent | Sending | Receiving deriving Show
data Context = Context { tran :: Transaction, state :: String } deriving Show
