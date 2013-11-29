-- FIXME Response codes for errors need to be identified as such
{-# LANGUAGE TupleSections #-}
module DIAL.Client.State where

import Network.URI
import Network.HTTP
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Applicative

import DIAL.Client.Application
import DIAL.Discovery.Client
import DIAL.Context
import DIAL.Diagnostic
import qualified Data.List as L
import Debug.Trace
import Control.Concurrent

short,long :: Int
short = 1
long  = 2

data Location = Location { locUri :: URI }
type ClientState a r = EitherT (Context, String) (ReaderT a IO) r

delay :: (MonadIO m) => Int -> m ()
delay = liftIO . threadDelay
seconds :: Int -> Int
seconds sec = sec * 1000000

--  TODO this should be a toResponse , and handle the failing response
--  codes here, instead of at 10 diff places
fromreq :: (Application a, HStream s) =>
  String -> Request s -> ClientState a (Response s)
fromreq str r = do
  response <- liftIO $ simpleHTTP r
  bimapEitherT ((Context Sending str,) .  show) id $ hoistEither response

discovering :: ClientState a URI
discovering = do
  msg "Client" short (snd client) "broadcast" "M-SEARCH"
  msg "Client" long "broadcast" (snd client) "(2) response with LOCATION header"
  delay . seconds $ 2

  --response <- fromreq "discovery" (searchReq $ defaultUserAgent 1)
  --response <- search
  location <- bimapEitherT (Context Unsent "Discovery",) id $
    hoistEither presetLocation

  -- location <- hoistEither $
  --   toUri context =<< firstField context HdrLocation response
  dbg "Client" $ "received: " ++ show location

  -- uri <- bimapEitherT (Context Unsent "Discovery",) id $
  --   hoistEither presetLocation
  --uri <- bimapEitherT (Context Unsent "Discovery",) id $ hoistEither location
  return location
  where
    context = Context Receiving "Client Discovery"

presetLocation :: Either String URI
presetLocation =
  let uri = "http://localhost:8000/dd.xml" in
    maybe (Left $ "unable to construct uri from " ++ uri) Right $ parseURI uri

firstField ::
  Context ->
  HeaderName ->
  Response String ->
  Either (Context, String) String
firstField c hname r =
  let retrieved = retrieveHeaders hname r in
  if L.null retrieved then
    Left (c,"missing header, " ++ show hname ++ " in response: \n" ++ show r)
  else Right . hdrValue . head $ retrieved

toUri :: Context -> String -> Either (Context, String) URI
toUri c str =
  maybe (Left (c, "invalid url received, " ++ str)) Right $ parseURI str

-- Find out if the application exists.
ddr :: (Application a) => URI -> ClientState a URI
ddr server_uri = do
  msg "Client" short (snd client) (snd server) " (3) HTTP GET LOCATION URL"
  delay . seconds $ 1

  response <- fromreq "Lookup" (Request server_uri GET [] [])

  --debug $ "received: " ++ show response
  location <-
    hoistEither $
      toUri context =<< firstField context header response

  msg "Client" long (snd server) (snd client) $
    " (4) Device Context with " ++ show header ++  " header"

  dbg "Client" $ "received : " ++ show location

  return location
  where
    header  = HdrCustom "Application-URL"
    context = Context Receiving "Device Description Request"

-- TODO verify appropriate content type generation, and is this in-line with
--      the spec?

launching :: (Application a) => URI -> ClientState a URI
launching app_url =
  let context = Context Sending "Launching application" in do
    app_name <- name <$> ask
    delay . seconds $ 1
    dbg "Client" $ "issuing launch request on app url: \n" ++ show app_url
    response <-
      fromreq (state context) $
        Request
          (app_url { uriPath = uriPath app_url ++ '/':app_name } )
          POST
          [Header HdrContentType "text/plain; charset=\"utf-8\""]
          []
    location <- hoistEither $
      toUri context_rec =<< firstField context HdrLocation response
    delay . seconds $ 1
    dbg "Client" $ "received: " ++ show location
    if rspCode response /= (2, 0, 1)
      then
        left $ toError context response
      else
        right $
          -- Trim /run from the path
          location {
            uriPath = take (length (uriPath location) - 4) $ uriPath location
          }
  where
    context_rec = Context Receiving "Launching"
    toError c response =
      (,) c $
        "incorrect application launch response code: "
          ++ show (rspCode response) ++ " of " ++ show response

toRequest :: URI -> Get a -> Request a
toRequest uri g = Request uri GET (headers g) (body g)

operating :: (Application a) => URI -> ClientState a URI
operating full_app_url = do
  app <- operation <$> lift ask
  dbg "Client" $ "starting app operation with app url: " ++ show full_app_url
  result <- liftIO $ runReaderT (runEitherT app) full_app_url
  case result of
    Left t   -> left (Context (fst t) "In application", snd t)
    Right () -> return full_app_url

deleting :: (Application a) => URI -> ClientState a ()
deleting full_app_url =
  void . liftIO . simpleHTTP $ (Request full_app_url DELETE [] "")

