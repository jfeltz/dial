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
import DIAL.Context
import DIAL.Diagnostic
import qualified Data.List as L
import Debug.Trace

short,long :: Int
short = 1
long  = 2

data Location = Location { locUri :: URI }
type ClientState a r = EitherT (Context, String) (ReaderT a IO) r

-- TODO implement discovery server
discovering :: ClientState a URI
discovering = do
  msg short (snd client) "broadcast" "M-SEARCH"
  msg long "broadcast" (snd client) "(2) response with LOCATION header"

  uri <- bimapEitherT (Context Unsent "Discovery",) id $
    hoistEither presetLocation

  debug $ "unimplemented, using preset location: " ++ show uri
  return uri

presetLocation :: Either String URI
presetLocation =
  let uri = "http://localhost:8000/dd.xml" in
    maybe (Left $ "unable to construct uri from " ++ uri) Right $ parseURI uri

-- Convenience functions, TODO move to Network module scope
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
  maybe
    (Left (c, "invalid url received, " ++ str))
    Right
    $ parseURI str

--  TODO this should be a toResponse , and handle the failing response
--  codes here, instead of at 10 diff places
fromreq :: (Application a, HStream s) =>
  String -> Request s -> ClientState a (Response s)
fromreq str r = do
  response <- liftIO $ simpleHTTP r
  bimapEitherT ((Context Sending str,) .  show) id $ hoistEither response

-- Find out if the application exists.
ddr :: (Application a) => URI -> ClientState a URI
ddr server_uri = do
  msg short (snd client) (snd server) " (3) HTTP GET LOCATION URL"

  response <- fromreq "Lookup" (Request server_uri GET [] [])

  debug $ "received: " ++ show response
  location <-
    hoistEither $
      toUri context =<< firstField context header response

  msg long (snd server) (snd client) $
    " (4) Device Context with " ++ show header ++  " header"

  debug $ "app header" ++ show header
  return location
  where
    header  = HdrCustom "Application-URL"
    context = Context Receiving "Device Description Request"

-- TODO verify appropriate gontent type generation, and is this in-line with
--      the spec?

launching :: (Application a) => URI -> ClientState a URI
launching app_url =
  let context = Context Sending "Launching application" in do
  app_name <- name <$> ask
  debug $ "issuing launch request on app url: " ++ show app_url
  response <-
    fromreq (state context) $
      Request
        (app_url { uriPath = uriPath app_url ++ '/':app_name } )
        POST
        [Header HdrContentType "text/plain; charset=\"utf-8\""]
        []
  debug $ "\n******  (launched) received: " ++ show response
  -- Verify CREATED and Location
  location <- hoistEither $
    toUri context_rec =<< firstField context HdrLocation response
  debug $ "location: " ++ show location
  --let running_app_url = app_url { uriPath = subpath }

  -- location_uri <- hoistEither $
  --   toUri context =<< firstField context HdrLocation response
  debug "checking launch response"
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

operating :: (Application a) => URI -> ClientState a ()
operating full_app_url = do
  app <- operation <$> lift ask
  debug $ "starting app operation with app url: " ++ show full_app_url
  result <- liftIO $ runReaderT (runEitherT app) full_app_url
  debug "got result "
  case result of
    Left t   -> left (Context (fst t) "In application", snd t)
    Right () -> return ()
