module DIAL.Discovery.Common where
import Network.HTTP.Headers

urn :: String
urn = "urn:dial-multiscreen-org:service:dial:1"

stHeader :: Header
stHeader = Header (HdrCustom "ST") urn
