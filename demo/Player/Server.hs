module Player.Server where
import qualified DIAL.Server.Application as SA
import Control.Concurrent
import Player
import Control.Applicative
import Control.Exception
import Network.URI

data Player = Player {
    playerState :: MVar PlayerState, command :: MVar Command
  }

newPlayer :: IO Player
newPlayer = Player <$> newEmptyMVar <*> newEmptyMVar

decodeSeconds :: String -> Either String Seconds
decodeSeconds str =
  case reads str :: [(Seconds, String)] of
    []                     ->
      Left $ "failed to parse seconds in: " ++ str
    [(seconds, [])]        ->
      Right seconds
    [(_, remainder)]       ->
      Left $ "erroneous trailing input: " ++ remainder

decode :: String -> Either String Command
-- Forward
decode ('F':rest)        = Forward <$> decodeSeconds rest
decode ('B':rest)        = Back <$> decodeSeconds rest
-- Stopped
decode ('P':'0':[])      = Right Stop
decode ('P':'1':' ':uri) =
 maybe
   (Left $ "invalid uri to be played" ++ show uri)
   (Right . Play)
   $ parseURI uri

-- Paused
decode ('P':[])         = Right Pause
decode unrecognized     = Left $ "unrecognized: " ++ unrecognized

playerThread :: MVar Command -> MVar PlayerState -> IO ()
playerThread c s = do
  -- Set the initial state
  putMVar s (Playing False)
  -- Wait & Check over a list
  fromExpectedL [
    (Play nullURI, Playing True), (Pause, Playing False), (Stop, Playing False)
    ]
  where
    fromExpectedL []            =
      return ()
    fromExpectedL (r:rest) = do
      putStrLn "waiting on next command"
      retrieved <- takeMVar c
      putStrLn $ "retrieved command from Mvar: " ++ show retrieved
      if fst r == retrieved
        then putMVar s (snd r) >> fromExpectedL rest
        else putStrLn $ "wrong command given, should be " ++ show (fst r)

instance (SA.Application Player) where
  launch p            =
    playerThread (command p) (playerState p) >> return True
  delete _            = return () -- TODO unemplemented
  postReply p content =
    case decode content of
      Left e  -> evaluate e
      Right c -> putMVar (command p) c >> show <$> takeMVar (playerState p)
  infoReply p         = show <$> takeMVar (playerState p)
