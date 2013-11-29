module Player where
import Network.URI
import Network.HTTP
import Control.Applicative

playing, stopped :: String
playing = "P1"
stopped = "P0"

data PlayerState = Playing Bool deriving Eq
instance (Show PlayerState) where
  show (Playing False) = stopped
  show (Playing True)  = playing

type Seconds = Int
data Command = Play URI | Pause | Stop | Back Seconds | Forward Seconds
instance (Show Command) where
  show = encode

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

encode :: Command -> String
encode (Play uri)  = playing ++ ' ' : show uri
encode Stop        = stopped
encode Pause       = take 1 playing
encode (Forward n) = 'F' : show n
encode (Back n)    = 'B' : show n

play :: URI -> String
play uri = playing ++ ' ' : show uri

instance Eq Command where
  (Play _) == (Play _) = True
  (Play _) == _        = False
  Pause  == Pause  = True
  Pause  == _      = False
  Stop   == Stop   = True
  Stop   == _      = False
  (Back _)  == (Back _) = True
  (Back _ )   == _ = False
  (Forward _)  == (Forward _) = True
  (Forward _ )   == _ = False

toState :: Response String -> Either String PlayerState
toState response = parseCode >> parseBody
  where
    parseCode =
      if rspCode response == (2, 0, 0) then Right ()
      else Left $ "unsupported response code: " ++ show (rspCode response)
    parseBody =
      if length body /= 1 then
        Left "response body of player state should be a single line"
      else
        let err = Left $ "invalid player state: " ++ show body in
        maybe err Right $
          lookup (head body) [
            (playing, Playing True),(stopped, Playing False)
          ]
      where body = lines . rspBody $ response
