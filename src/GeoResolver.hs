{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GeoResolver
Description : Simple way to access Google's geocoding API. Using http-conduit.
Copyright   : (c) 2015, Markenwerk, Jan Greve
License     : MIT
Maintainer  : jg@markenwerk.net


This is supposed to offer an easy to use abstraction of the google geocoding
web service.

A minimum example of usage is:

@
main = geoEncode Nothing "Lornsenstraße 43, Kiel" >>= putStrLn . show
@

While it remains to be implemented to send all possible requests,
it is possible to send raw requests.

If you do so, please see "Geocoder.Parser" for ways to access the results.

-}

module GeoResolver
    ( 
      -- * Encoding
      geoEncode,
      geoEncodeLanguage,
      -- * Decoding
      geoDecode,
      geoDecodeLanguage,
      -- * Raw Requesting
      geoRaw
    ) where
import GeoResolver.Requester
import GeoResolver.Parser
import Control.Monad
import Data.Text (pack, append)


-- | Encodes a given address into a coordinate.
geoEncode :: Maybe String -- ^ Optional API key to use if you intend to go over the courtesy limit
  -- Google imposes 
  -> String -- ^ The address to be looked up.
  -> IO (Either String (Double, Double)) -- ^ Either an error message or the result.
geoEncode mk x =
   liftM (parseAnswer >=> getLocation) (requestEncode (fmap pack mk) (pack x))

-- | Encodes a given address into a coordinate.
geoEncodeLanguage :: Maybe String -- ^ Optional API key to use if you intend to go over the courtesy limit
  -- Google imposes 
  -> String -- ^ The address to be looked up.
  -> String -- ^ Language to be used. cf. https://developers.google.com/maps/faq#languagesupport
  -> IO (Either String (Double, Double)) -- ^ Either an error message or the result.
geoEncodeLanguage Nothing addr lang =
    liftM (parseAnswer >=> getLocation)
        (requestRaw [("address", pack addr), ("language", pack lang)])

geoEncodeLanguage (Just k) addr lang =
    liftM (parseAnswer >=> getLocation)
        (requestRaw [("address", pack addr), ("language", pack lang), ("key", pack k)])

-- | Reverse geocoding of a given coordinate pair into an address.
geoDecode :: Maybe String -- ^ Optional API key to use if you intend to go over the courtesy limit
  -- Google imposes 
  -> (Double, Double) -- ^ coordinate to be decoded
  -> IO (Either String String) -- ^ Either an error message (Left) or the result (Right).
geoDecode mk x =
    liftM (parseAnswer >=> getAddress)
        (requestDecode (fmap pack mk) x)

-- | Reverse geocoding of a given coordinate pair into an address.
geoDecodeLanguage :: Maybe String -- ^ Optional API key to use if you intend to go over the courtesy limit
  -- Google imposes 
  -> (Double, Double) -- ^ coordinate to be decoded
  -> String -- ^ Language to be used. cf. https://developers.google.com/maps/faq#languagesupport
  -> IO (Either String String) -- ^ Either an error message (Left) or the result (Right).
geoDecodeLanguage Nothing (lat, lng) lang =
    liftM (parseAnswer >=> getAddress)
        (requestRaw
            [("latlng",pack (show lat) `append` pack (',' : show lng)), ("language", pack lang)])
geoDecodeLanguage (Just k) (lat, lng) lang =
    liftM (parseAnswer >=> getAddress)
        (requestRaw
            [("latlng",pack (show lat) `append` pack (',' : show lng)), ("language", pack lang), ("key", pack k)])

-- | Sending a raw request to the api, if you want more control than the above methods offer.
-- Uses a pair of key-value pairs to generate the actual query.
-- See "GeoResolver.Parser" for a helping hand using the resulting 'GoogleAnswer'.
geoRaw :: [(String, String)] -> IO (Either String GoogleAnswer)
geoRaw xs = liftM parseAnswer (requestRaw (map (\(k,v) -> (pack k, pack v)) xs))