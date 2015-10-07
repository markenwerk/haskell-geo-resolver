module GeoResolver.Requester (requestLookup,
    requestReverseLookup,
    uriFromQueryPairs,
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

requestRaw :: [(String, String)] -> IO LBS.ByteString
requestRaw = simpleHttp . show . uriFromQueryPairs

uriFromAddress :: String -> URI
uriFromAddress x = uriFromQueryPairs [("address",x)]

uriFromLocation :: (Double, Double) -> URI
uriFromLocation (lat, lng) = uriFromQueryPairs [("latlng", show lat ++ ',':show lng)]

requestLookup :: String -> IO LBS.ByteString
requestLookup = simpleHttp . show . uriFromAddress 

requestReverseLookup :: (Double, Double) -> IO LBS.ByteString
requestReverseLookup = simpleHttp . show . uriFromLocation




