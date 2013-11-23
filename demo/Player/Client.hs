{-# LANGUAGE TupleSections #-}
module Player.Client where
import DIAL.Client.Application
import Player
import Network.HTTP
import Network.URI
import Control.Monad.Trans.Either
import DIAL.Context
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import DIAL.Diagnostic
import Control.Applicative
import Network.Socket hiding (recv)
import Control.Monad.Reader
import Debug.Trace

data ClientPlayer = ClientPlayer

address :: String
address = "http://www.sos.siena.edu/~mmalak/modules/whale/whale-hi.mov"
-- TODO This should be paramatized dependant on the use-case

instance Application ClientPlayer where
  name _      = "Player"
  operation _ = goalpost

fromreq :: (HStream s, Show s) => Request s -> App (Response s)
fromreq r = do
  response <- liftIO $ simpleHTTP r
  bimapEitherT ((Sending,) .  show) id $ hoistEither response

-- | retrieve the Results of an info request
infoRequest :: App (Response String)
infoRequest = do
  app_uri <- ask
  response <- fromreq (Request app_uri GET [] [])
  return $ trace ("response: " ++ show response) response

verify :: PlayerState -> Response String -> App ()
verify expected response =
  let toEitherT = bimapEitherT (Receiving,) id . hoistEither in
  toEitherT $
    do
      actual <- toState response
      if actual == expected
        then Right ()
        else
          Left $
            "response does not contain expected state, "
            ++ '\n' : show expected
            ++ '\n' : "actual: " ++ show actual

-- | Take application content and post it to the server
post :: String -> App (Response String)
post content = do
  app_uri <- ask
  let rq = req app_uri
  debug $ "post request: \n" ++ show rq
  debug $ "post request body: \n" ++ (show . rqBody $ rq)
  fromreq rq
  --debug $ "post response: \n" ++ show either_response
  --bimapEitherT ((Sending,) . show) id $ hoistEither either_response
  where
    req app_uri =
      Request
        app_uri
        POST
        [Header HdrContentType "text/plain; charset=utf-8",
         Header HdrContentLength (show . length $ content)
        ]
        content

goalpost :: App ()
goalpost = do
  -- debug " player: issuing info request"
  verify (Playing False) =<< infoRequest

  played_uri <-
    maybe
      (left (Sending, "uri parse failure"))
      right
      $ parseURI address

  debug " player: issuing play command"

  -- TODO Parse address
  verify (Playing True) =<< (post . show $ Play played_uri)

  delay $ seconds 1

  verify (Playing False) =<< (post . show $ Pause)

  delay $ seconds 1

  verify (Playing False) =<< (post . show $ Stop)
  where
    delay = liftIO . threadDelay
    seconds sec = sec * 1000000
