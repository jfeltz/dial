{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module DIAL.Server.State where
import Network.URI
import Network.HTTP
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Data.IORef
import Control.Applicative
import Control.Lens hiding (Context)
import DIAL.Server.Application
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified DIAL.Diagnostic as D
import qualified Data.List as L
import System.Log.Logger
import DIAL.Diagnostic

isReq :: URI -> RequestMethod -> Request String -> Bool
isReq uri method r = rqMethod r == method && rqURI r == uri

utf8 :: Header
utf8 = Header HdrContentType "text/plain; charset=\"utf-8\""

-- Various responses
created,deleted,dd :: String -> Response String
created loc = Response (2, 0, 1) "CREATED" [Header HdrLocation loc] []
deleted loc = Response (2, 0, 0) [] [Header HdrLocation loc] []
dd url = Response (2, 0, 0) [] [Header (HdrCustom "Application-URL") url] []

clHeader :: [a] -> Header
clHeader = Header HdrContentLength . show . length

info :: String -> Response String
info body = Response (2, 0, 0) "OK" [utf8, clHeader . F.toList $ body] body

relPath :: Request r -> String
relPath = uriPath . rqURI

toResponse :: forall a. (Application a) =>
  String ->
  IORef (M.Map String (Bool, a)) ->
  Request String ->
  IO (Response String)
toResponse rel_app_subpath io_ref r =
  fmap (either id id) . runEitherT $
  case rqMethod r of
    POST  -> do -- Launch or app post
      (app, name, launched) <- toApp =<< liftIO (readIORef io_ref)
      D.msg "Server" 2 (snd D.server) (snd D.client) $
        "received post for app: " ++ show name
      if null $ rqBody r then
        if launched
          then left $ Response (4, 0, 5) [] [] []
          else do
            liftIO $
              launch app >> modifyIORef io_ref (M.adjust (set _1 True) name)
            right . created $
              show ((rqURI r) {
                uriPath = ('/':rel_app_subpath) ++ '/':name  ++ "/run" }
              )
      else
        info <$> liftIO (postReply app (rqBody r))
    DELETE -> do
      (app, name, launched) <- toApp =<< liftIO (readIORef io_ref)
      dbg "Server" $ "received DELETE request for app: " ++ show name
      when launched $
        liftIO $ modifyIORef io_ref (M.adjust (set _1 False) name) >> delete app
      right $ Response (2, 0, 0) [] [] ""
    GET    ->
      if relPath r == "/dd.xml" then do
        D.msg "Server" 2 (snd D.server) (snd D.client)
          "returning app-url response"
        return . dd $ show ((rqURI r) { uriPath = '/':rel_app_subpath })
      else do
        (app, _, _) <- toApp =<< liftIO (readIORef io_ref)
        info <$> liftIO (infoReply app)
    _      -> return $ Response (4, 0, 5) [] [] ""
  where
    toApp :: (MonadIO m) =>
      M.Map String (Bool, a) ->
      EitherT (Response String) m (a, String, Bool)
    toApp m = do
      name <- appName
      maybe (left $ Response (4, 0, 4) [] [] [])
            (right . \t -> (snd t, name, fst t))
            $ M.lookup name m
      where
        appName =
          let prefix = '/':rel_app_subpath in
            if prefix `L.isPrefixOf` relPath r
              then
                -- TODO behavior for the trailing slash is ambiguous
                right $ drop (L.length prefix + 1) (relPath r)
              else
                left $ Response (4, 0, 4) [] [] []

defaultConfig :: Config
--defaultConfig = Config stdLogger "localhost"  8000
defaultConfig = Config quietLogger "localhost"  8000

toServer :: (Application a) => Config -> M.Map String a -> String -> IO ()
toServer c applications rel_app_subpath = do
  updateGlobalLogger "Server" (setLevel DEBUG)
  io_ref <- newIORef $ M.map (False,) applications
  serverWith c (\_ _ r -> toResponse rel_app_subpath io_ref r)
