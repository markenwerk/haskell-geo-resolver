{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : GeoResolver.Requester
Description : Request helper definitions for Googles geocoding API. Using http-conduit.
Copyright   : (c) 2015, Markenwerk, Jan Greve
License     : MIT
Maintainer  : jg@markenwerk.net
-}
module GeoResolver.Requester (
    -- * Data Types
    GoogleAPIRequest(..),
    GoogleComponents(..),
    GoogleLocationTypes(..),
    GoogleResultTypes(..),
    -- * Request methodsv
    requestEncode,
    requestDecode,
    requestRaw,
    requestRequest

    ) where

import Network.HTTP.Types
import Network.HTTP.Conduit
import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Text (Text, append, pack)
import GeoResolver.Parser (GoogleBoundingBox(..), Location(..), GoogleArgumentListShow(..))
import Data.Maybe
import Control.Arrow (second)
import Data.String (IsString(..))

-- | A Type abstracting possible API request argument combinations.
--
-- For convenience, the 'IsString' instance generates a encoding request and assumes the String
-- is the address.
data GoogleAPIRequest = 
  EncodingRequest {
    encodeParameter :: Either String GoogleComponents,
    encodeBounds :: Maybe GoogleBoundingBox,
    encodeLanguage :: Maybe String,
    encodeKey :: Maybe String
  }
  | DecodingRequest {
    decodeParameter :: Either Location String,
    decodeKey :: Maybe String,
    decodeLanguage :: Maybe String,
    decodeResultType :: Maybe GoogleResultTypes,
    decodeLocationType :: Maybe GoogleLocationTypes
}

instance IsString GoogleAPIRequest where
  fromString s = EncodingRequest (Left s) Nothing Nothing Nothing

instance GoogleArgumentList GoogleAPIRequest where
  argShow (EncodingRequest (Left addr) b l k) = map (second fromJust) $ filter (isJust . snd) $
    zip
      ["address", "bounds", "language", "key"]
      [Just . pack $ addr, fmap (pack . argListShow) b, fmap pack l, fmap pack k]
  argShow (EncodingRequest (Right c) b l k) = map (second fromJust) $ filter (isJust . snd) $
    zip
      ["components", "bounds", "language", "key"]
      [Just . pack . argListShow $ c, fmap (pack . argListShow) b, fmap pack l, fmap pack k]
  argShow (DecodingRequest (Left loc) k l rt lt) = map (second fromJust) $ filter (isJust . snd) $
    zip
      ["latlng", "key", "language", "result_type", "location_type"]
      [Just . pack . argListShow $ loc, fmap pack k, fmap pack l, fmap (pack . argListShow) rt, fmap (pack . argListShow) lt]
  argShow (DecodingRequest (Right pid) k l rt lt) = map (second fromJust) $ filter (isJust . snd) $
    zip
      ["place_id", "key", "language", "result_type", "location_type"]
      [Just . pack $ pid, fmap pack k, fmap pack l, fmap (pack . argListShow) rt, fmap (pack . argListShow) lt]

class GoogleArgumentList a where
  argShow :: a -> [(Text, Text)]


-- | Abstraction for google's components
data GoogleComponents = Components [String]
instance GoogleArgumentListShow GoogleComponents where
  argListShow (Components []) = ""
  argListShow (Components (x : xs)) = x ++ concatMap ('|' :) xs

-- | Abstraction for google's result types
data GoogleResultTypes = ResultTypes [String]
instance GoogleArgumentListShow GoogleResultTypes where
  argListShow (ResultTypes []) = ""
  argListShow (ResultTypes (x : xs)) = x ++ concatMap ('|' :) xs
-- | Abstraction for google's location types
data GoogleLocationTypes = LocationTypes [String]
instance GoogleArgumentListShow GoogleLocationTypes where
  argListShow (LocationTypes []) = ""
  argListShow (LocationTypes (x : xs)) = x ++ concatMap ('|' :) xs


baseURL :: Builder
baseURL = "https://maps.googleapis.com/maps/api/geocode/json"

uriFromQueryPairs :: [(Text, Text)] -> LBS.ByteString
uriFromQueryPairs ps = toLazyByteString $ baseURL `mappend` query
  where query = renderQueryBuilder True $ queryTextToQuery (map (second Just) ps)

-- | Constructs the URI to be used for the web service
-- invocation from the input. Sends a request and
-- returns the Lazy ByteString from IO.
requestRaw :: [(Text, Text)] -> IO LBS.ByteString
requestRaw = simpleHttp . LBSC.unpack . uriFromQueryPairs

uriFromAddress :: Maybe Text -> Text -> LBS.ByteString
uriFromAddress Nothing x = uriFromQueryPairs [("address",x)]
uriFromAddress (Just k) x = uriFromQueryPairs [("address",x), ("key", k)]


uriFromLocation :: Maybe Text -> (Double, Double) -> LBS.ByteString
uriFromLocation Nothing (lat, lng) = uriFromQueryPairs [("latlng", pack (show lat) `append` (pack $ ',':show lng))]
uriFromLocation (Just k) (lat, lng) = uriFromQueryPairs [("latlng", pack (show lat) `append` (pack $ ',':show lng)), ("key", k)]

-- | Convenience function to request a given address.
requestEncode :: Maybe Text -> Text -> IO LBS.ByteString
requestEncode mk = simpleHttp . LBSC.unpack . uriFromAddress mk

-- | Convenience function to request a given location.
requestDecode :: Maybe Text  -> (Double, Double) -> IO LBS.ByteString
requestDecode mk = simpleHttp . LBSC.unpack . uriFromLocation mk

-- | Sends a request based on a 'GoogleApiRequest'.
requestRequest :: GoogleAPIRequest -> IO LBS.ByteString
requestRequest = simpleHttp . LBSC.unpack . uriFromQueryPairs . argShow