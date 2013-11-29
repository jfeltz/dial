{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment
import DIAL.Client
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
import System.Log.Logger

data ClientPlayer = ClientPlayer

address :: String
address = "file:///home/jpf/test.mp4"

instance Application ClientPlayer where
  name _      = "VLC"
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
  return $ response

verify :: PlayerState -> Response String -> App ()
verify expected response =
  let toEitherT = bimapEitherT (Receiving,) id . hoistEither in
  toEitherT $ do
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
  fromreq rq
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
  verify (Playing False) =<< infoRequest

  played_uri <-
    maybe (left (Sending, "uri parse failure")) right $ parseURI address

  delay . seconds $ 1
  verify (Playing True) =<< (post . show $ Play played_uri)

  delay . seconds $ 2
  verify (Playing True) =<< (post . show $ Forward 20)

  delay . seconds $ 1
  verify (Playing True) =<< (post . show $ Forward 25)

  delay . seconds $ 1
  verify (Playing False) =<< (post . show $ Pause)

  delay . seconds $ 1
  verify (Playing True) =<< (post . show $ Pause)

  delay . seconds $ 18

  verify (Playing False) =<< (post . show $ Pause)

  delay . seconds $ 5

  where
    delay = liftIO . threadDelay
    seconds sec = sec * 1000000

main :: IO ()
main = do
  updateGlobalLogger "Client" (setLevel DEBUG)
  exampleApp ClientPlayer
