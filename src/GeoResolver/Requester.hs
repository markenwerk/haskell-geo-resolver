{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : GeoResolver.Requester
Description : Request helper definitions for Googles geocoding API. Using http-conduit.
Copyright   : (c) 2015, Markenwerk, Jan Greve
License     : MIT
Maintainer  : jg@markenwerk.net
-}
module GeoResolver.Requester (
    -- * Requests
    requestEncode,
    requestDecode,
    requestRaw
    ) where

import Network.HTTP.Types
import Network.HTTP.Conduit
import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, append, pack)

baseURL :: Builder
baseURL = "https://maps.googleapis.com/maps/api/geocode/json"

uriFromQueryPairs :: [(Text, Text)] -> LBS.ByteString
uriFromQueryPairs ps = toLazyByteString $ baseURL `mappend` query
    where query = renderQueryBuilder True $ queryTextToQuery (map (\(k,v) -> (k, Just v)) ps)

-- | Constructs the URI to be used for the web service
-- invocation from the input. Sends a request and
-- returns the Lazy ByteString from IO.
requestRaw :: [(Text, Text)] -> IO LBS.ByteString
requestRaw = simpleHttp . show . uriFromQueryPairs

uriFromAddress :: Text -> LBS.ByteString
uriFromAddress x = uriFromQueryPairs [("address",x)]

uriFromLocation :: (Double, Double) -> LBS.ByteString
uriFromLocation (lat, lng) = uriFromQueryPairs [("latlng", pack (show lat) `append` (pack $ ',':show lng))]

-- | Convenience function to request a given address.
requestEncode :: Text -> IO LBS.ByteString
requestEncode = simpleHttp . show . uriFromAddress 

-- | Convenience function to request a given location.
requestDecode :: (Double, Double) -> IO LBS.ByteString
requestDecode = simpleHttp . show . uriFromLocation




