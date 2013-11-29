module DIAL.Discovery.UserAgent where
import Network.HTTP.Headers
import Prelude hiding (product)

data UserAgent = UserAgent {
    operatingSystem :: String
    , osVersion :: String
    , product :: String
    , productVersion :: String
  }

toHeader :: UserAgent -> Header
toHeader agent =
  Header HdrUserAgent $
    operatingSystem agent ++ '/': osVersion agent ++ ' '
      :product agent ++ '/': productVersion agent

defaultUserAgent :: UserAgent
defaultUserAgent =
  UserAgent "Haskell supporting platform" "undefined" "dial product" "undefined"
