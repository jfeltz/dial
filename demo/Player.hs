module Player where
import Network.URI
import Network.HTTP

playing, stopped, caching :: String
playing = "P1"
stopped = "P0"
caching = "C"

data PlayerState = Launched | Caching | Playing Bool deriving Eq
instance (Show PlayerState) where
  show (Playing False) = stopped
  show (Playing True)  = playing
  show Caching         = caching

type Seconds = Int
data Command = Play URI | Pause | Stop | Back Seconds | Forward Seconds
instance (Show Command) where
  show = encode

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
            (playing, Playing True),(stopped, Playing False),(caching, Caching)
          ]
      where body = lines . rspBody $ response
