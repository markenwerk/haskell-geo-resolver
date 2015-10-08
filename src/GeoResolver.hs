{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GeoResolver
Description : Simple way to access Google's geocoding API. Using http-conduit.
Copyright   : (c) 2015, Markenwerk, Jan Greve
License     : MIT
Maintainer  : jg@markenwerk.net


Moar about using this.

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
geoEncode :: String -- ^ The address to be looked up.
  -> IO (Either String (Double, Double)) -- ^ Either an error message or the result.
geoEncode x =
   liftM (parseAnswer >=> getLocation) (requestEncode $ pack x)

-- | Encodes a given address into a coordinate.
geoEncodeLanguage :: String -- ^ The address to be looked up.
  -> String -- ^ Language to be used. cf. https://developers.google.com/maps/faq#languagesupport
  -> IO (Either String (Double, Double)) -- ^ Either an error message or the result.
geoEncodeLanguage addr lang =
    liftM (parseAnswer >=> getLocation)
        (requestRaw [("address", pack addr), ("language", pack lang)])

-- | Reverse geocoding of a given coordinate pair into an address.
geoDecode :: (Double, Double) -- ^ coordinate to be decoded
  -> IO (Either String String) -- ^ Either an error message (Left) or the result (Right).
geoDecode x =
    liftM (parseAnswer >=> getAddress)
        (requestDecode x)

-- | Reverse geocoding of a given coordinate pair into an address.
geoDecodeLanguage :: (Double, Double) -- ^ coordinate to be decoded
  -> String -- ^ Language to be used. cf. https://developers.google.com/maps/faq#languagesupport
  -> IO (Either String String) -- ^ Either an error message (Left) or the result (Right).
geoDecodeLanguage (lat, lng) lang =
    liftM (parseAnswer >=> getAddress)
        (requestRaw
            [("latlng",pack (show lat) `append` pack (',' : show lng)), ("language", pack lang)])

-- | Sending a raw request to the api, if you want more control than the above methods offer.
-- See "GeoResolver.Parser" for a helping hand using the resulting 'GoogleAnswer'.
geoRaw :: [(String, String)] -> IO (Either String GoogleAnswer)
geoRaw xs = liftM parseAnswer (requestRaw (map (\(k,v) -> (pack k, pack v)) xs))