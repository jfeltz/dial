{-# LANGUAGE TupleSections #-}
module DIAL.Client where

--import Network.URI
{- import Network.HTTP -}
--import Network.Stream
--import System.Environment (getArgs)
import Control.Monad.Trans.Either
import qualified Data.List as L
import DIAL.Client.Application
import Control.Monad.Reader
import qualified DIAL.Client.State as State
import Prelude hiding (lookup)
import DIAL.Diagnostic

-- This is an example use-case only.
exampleApp :: (Application a) => a -> IO ()
exampleApp app = do
  dbg "Client" participants
  print =<< runReaderT (
    runEitherT $
      State.discovering
        >>= State.ddr
        >>= State.launching
        >>= State.operating
        >>= State.deleting
    ) app
  where
    participants :: String
    participants =
      let
        participant p abbr = "participant: \"" ++ p ++ "\"" ++ " as " ++ abbr
      in
        L.intercalate ",\n" $
          map (uncurry participant) [client, server, upnp]
