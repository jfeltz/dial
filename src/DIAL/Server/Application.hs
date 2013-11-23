module DIAL.Server.Application where

type AppInfo = String
type Params = String
type Content = String
type AppState = Bool

class Application a where
  launch :: a -> IO Bool
  delete :: a -> IO ()
  postReply :: a -> Content -> IO AppInfo
  infoReply :: a -> IO AppInfo
