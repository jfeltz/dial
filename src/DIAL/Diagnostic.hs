module DIAL.Diagnostic where
import Control.Monad.IO.Class

client :: (String, String)
client = ("DIAL Client", "C")

server :: (String, String)
server = ("DIAL Server", "S")

upnp :: (String, String)
upnp = ("UPnP Server", "U")

diag :: (MonadIO m) => String -> m ()
diag = liftIO . putStr

line :: String -> String
line orig = '\n':orig

debug :: (MonadIO m) => String -> m ()
debug = diag . line . (++) "debug: "

msg :: (Monad m, MonadIO m) => Int -> String -> String -> String -> m ()
msg arrow src dst msg' =
  diag $
    '\n':src ++ ' ':replicate arrow '-'  ++ "> "++ dst ++ ": " ++ msg' ++ "\n"
