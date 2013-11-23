module DIAL.Server where
import DIAL.Server.Application
import DIAL.Server.State

import Network.HTTP.Server

-- TODO/FIXME This assumes a single application server,
-- until I get a better feel for DIAL

-- fromApp :: (Application a) => ServerState a ()
-- fromApp = do
--   diag participants
--   dormant >>= launched >> fromApp
--
-- dialHandler :: Handler a
-- dialHandler = undefined -- TODO
--
-- f :: ServerState a ()
-- f = server dialHandler
--
