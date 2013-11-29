module DIAL.Diagnostic where
import Control.Monad.IO.Class
import System.Log.Logger

client :: (String, String)
client = ("DIAL Client", "C")

server :: (String, String)
server = ("DIAL Server", "S")

upnp :: (String, String)
upnp = ("UPnP Server", "U")

dbg :: (MonadIO m) => String -> String -> m ()
dbg comp = liftIO . debugM comp

line :: String -> String
line orig = '\n':orig

msg :: (Monad m, MonadIO m) => String -> Int -> String -> String -> String -> m ()
msg comp arrow src dst msg' =
  dbg comp $ src ++ ' ':replicate arrow '-' ++ "> "++ dst ++ ": " ++ msg'
