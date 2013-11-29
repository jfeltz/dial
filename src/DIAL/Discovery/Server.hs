-- TODO UDP multicast service announcements

module DIAL.Discovery.Server where
import Network.URI
import Network.HTTP
import DIAL.Discovery.Common
import DIAL.Discovery.UserAgent hiding (toHeader)
--import Control.Lens
import Prelude hiding (product)

type Seconds = Int

toHeader :: UserAgent -> Header
toHeader agent =
  Header HdrServer $
    operatingSystem agent ++ '/': osVersion agent
      ++ " UPnP/1.1 " ++ product agent ++ '/': productVersion agent

response :: URI -> UserAgent -> Response String
response service_location agent =
  Response (2,0,0) "OK" serverHeaders []
  where
    serverHeaders :: [Header]
    serverHeaders =
      (:) (toHeader agent)  $
        map (uncurry Header) [
           ( HdrLocation, show service_location)
           , ( HdrCacheControl, "max-age=180" )
           , ( HdrHost, "239.255.255.250:1900" )
           , ( HdrCustom "EXT", "")
           , ( HdrCustom "ST", urn)
           , ( HdrCustom "BOOTID.UPNP.ORG", "1")
          ]

-- TODO
server = do
  -- Create the stream from UDP socket listening on the multicast address?

  -- receive and response
  request <- receiveHTTP
  -- response if service match in request is valid
    -- respond via HTTP UDP "unicaast"

  -- Close the stream
  server
