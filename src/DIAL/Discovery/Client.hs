
module DIAL.Discovery.Client where
import Network.URI
import Network.HTTP hiding (defaultUserAgent)
import Network.Socket
import DIAL.Discovery.Common
import DIAL.Discovery.UserAgent
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Either.Combinators
import Network.HTTP.Base hiding (defaultUserAgent)


type Seconds = Int

searchReq :: UserAgent -> Seconds -> Request String
searchReq agent seconds_to_delay =
  Request (nullURI { uriScheme = "*" }) (Custom "M-SEARCH") clientHeaders []
  where
    clientHeaders :: [Header]
    clientHeaders =
      (:) (toHeader agent)  $
        map (uncurry Header)
          [
           ( HdrHost, "239.255.255.250:1900" )
           , ( HdrCustom "ST", urn)
           , ( HdrCustom "MAN", show "ssdp:discover")
           , ( HdrCustom "MX", show seconds_to_delay)
          ]

-- Iterate Broadcast request over HTTP UDP, until
-- receipt of a unicast response

-- multicast broadcast -> unicast response
toAddressInfo :: IO [AddrInfo]
toAddressInfo = getAddrInfo Nothing (Just "239.255.255.250") (Just "1900")

broadcast :: IO ()
broadcast = do
  multicast_address <- head <$> toAddressInfo
  s <- socket AF_INET Datagram 0

  source <- inet_addr "localhost"
  bindSocket s $ SockAddrInet aNY_PORT source
  sendTo s (show $ searchReq defaultUserAgent 1) (addrAddress multicast_address)
  sClose s
  return ()

--Receive the uni-casted response for a search.
-- receive :: EitherT String IO (Response String)
-- receive = do
--   received <-
--     liftIO $ do
--       s <- socket AF_INET Datagram 0
--       bindSocket s (SockAddrInet 1900 iNADDR_ANY)
--       (s', _) <- accept s
--       stream <- socketConnection "localhost" 1900 s'
--       sClose s
--       --receiveHTTP stream
--   hoistEither $ mapBoth show id received

-- TODO error handling
-- search :: EitherT String IO (Response String)
-- search = do
--   liftIO $ do
--     putStrLn "broad-casting"
--     broadcast
--     putStrLn "broad-casted, receiving"
--   response <- receive
--   putStrLn $ "received: " ++ show response
--   return response
