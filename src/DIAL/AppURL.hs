-- TODO Since we haven't implemented a discovery server yet,
-- it is assumed that the client knows appUrl, hence this shared module

module DIAL.AppURL where

relAppURL :: String
relAppURL = "/apps/"

fromRoot :: String -> String
fromRoot = flip (++) relAppURL
