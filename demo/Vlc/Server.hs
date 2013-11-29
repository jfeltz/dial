module Main where
import DIAL.Server.State
import Control.Concurrent
import Control.Monad
import System.Cmd
import Player
import Network.URI hiding (query)
import Network.HTTP hiding (host, port)
import Control.Applicative
import Text.HTML.TagSoup
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import qualified Data.List as L
import qualified Data.Map as M
import qualified DIAL.Server.Application as SA
import Data.Either.Combinators (mapLeft)
import DIAL.Diagnostic

data VLC = VLC { port :: Int, host :: String }

defaultVlc :: VLC
defaultVlc = VLC 9090 "localhost"

statusURI ::VLC -> URI
statusURI v =
  nullURI {
    uriScheme      = "http:"
    , uriAuthority = Just . URIAuth [] (host v) $ ':':show (port v)
    , uriPath      = "/requests/status.xml"
  }

commandURI :: VLC -> String -> URI
commandURI v query =
  (statusURI v) { uriQuery = "?command=" ++ query }

toURI :: VLC -> Command -> URI
toURI v = commandURI v . queryRhs
  where
    queryRhs (Play uri)  = "in_play&input=" ++ show uri
    queryRhs Stop        = "pl_stop"
    queryRhs Pause       = "pl_pause"
    queryRhs (Back n)    = "seek&val=-" ++ show n ++ "s"
    queryRhs (Forward n) = "seek&val=+" ++ show n ++ "s"

-- TODO My monad Ninjitsu isn't good enough to one-line this
fromStatusReq :: Request String -> EitherT String IO PlayerState
fromStatusReq r = do
  response <- liftIO $ mapLeft show <$> simpleHTTP r
  hoistEither $ parseState . rspBody =<< response

toStatus :: VLC -> EitherT String IO PlayerState
toStatus = fromStatusReq . vlcRequest . statusURI

vlcRequest :: URI -> Request String
vlcRequest uri = Request uri GET [] ""

fromCommand :: VLC -> Command -> EitherT String IO PlayerState
fromCommand v c =
  -- Current tested VLC version reports state change
  -- after the first request
  fromStatusReq (vlcRequest $ toURI v c) >> toStatus v

fromState :: String -> Either String PlayerState
fromState state =
  maybe (Left $ "unable to match received state: " ++ state) Right $
    L.lookup state [
      ("stop", Playing False),
      ("playing", Playing True),
      ("paused", Playing False)
    ]

parseState :: String -> Either String PlayerState
parseState content = do
  after_tag <- drop 1 <$> tagContent
  toText after_tag >>= fromState
  where
    toText :: [Tag String] -> Either String String
    toText rest =
      if L.null rest || (not . isTagText . head $ rest) then
        Left "empty state tag content"
      else
        Right . fromTagText . head $ rest
    tagContent :: Either String [Tag String]
    tagContent =
      if L.null start then Left "missing state tag" else Right start
      where
        start = dropWhile (not . isTagOpenName "state") $ parseTags  content


instance (SA.Application VLC) where
  launch v = (forkIO . void . system $ command) >> return True
    where
      command = "xterm -e vlc -I http --http-host " ++ host v ++ ':' : show (port v)
  -- TODO there may be something more informative sent by vlc

  delete  v =
    either (const ()) (const ()) <$> runEitherT (fromCommand v $ Play quitPath)
    where
      quitPath :: URI
      quitPath =
        nullURI {
          uriScheme = "vlc:",
          uriAuthority = Just (URIAuth "" "" ""),
          uriPath = "quit"
        }

  postReply v c =
    case decode c of
      Left e        -> return e
      Right command -> do
        dbg "Server" $ "vlc app received: " ++ show command
        either id show <$> runEitherT (fromCommand v command)
  infoReply v = either id show <$> runEitherT (toStatus v)

main :: IO ()
main = toServer defaultConfig (M.singleton "VLC" defaultVlc) "apps"
