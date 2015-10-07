{-|
Module      : GeoResolver.Requester
Description : Request helper definitions for Googles geocoding API. Using http-conduit.
Copyright   : (c) 2015, Markenwerk, Jan Greve
License     : MIT
Maintainer  : jg@markenwerk.net
-}
module GeoResolver.Requester (
    -- * Requests
    requestLookup,
    requestReverseLookup,
    requestRaw
    ) where

import Text.URI
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as LBS

baseURL :: URI
baseURL = nullURI {
    uriScheme = Just "https",
    uriRegName = Just "maps.googleapis.com",
    uriPath = "/maps/api/geocode/json"
}

uriFromQueryPairs :: [(String, String)] -> URI
uriFromQueryPairs ps = baseURL {uriQuery = Just query}
    where query = pairsToQuery ps

-- | Constructs the URI to be used for the web service
-- invocation from the input. Sends a request and
-- returns the Lazy ByteString from IO.
requestRaw :: [(String, String)] -> IO LBS.ByteString
requestRaw = simpleHttp . show . uriFromQueryPairs

uriFromAddress :: String -> URI
uriFromAddress x = uriFromQueryPairs [("address",x)]

uriFromLocation :: (Double, Double) -> URI
uriFromLocation (lat, lng) = uriFromQueryPairs [("latlng", show lat ++ ',':show lng)]

-- | Conveniance function to request a given address.
requestLookup :: String -> IO LBS.ByteString
requestLookup = simpleHttp . show . uriFromAddress 

-- | Conveniance function to request a given location.
requestReverseLookup :: (Double, Double) -> IO LBS.ByteString
requestReverseLookup = simpleHttp . show . uriFromLocation




