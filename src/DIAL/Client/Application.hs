{-# LANGUAGE ScopedTypeVariables, TupleSections, RankNTypes #-}
module DIAL.Client.Application where
import Network.URI
import Control.Monad.Trans.Either
import DIAL.Context
import Control.Monad.Reader
import Network.HTTP

data Status = Starting | Running | NotRunning
type URL = String

data Get a = Get { headers :: [Header], body :: a }

class Application a where
  name :: a -> String
  operation :: a -> App ()

-- This reads the application URI.
type App r = EitherT (Transaction, String) (ReaderT URI IO) r
