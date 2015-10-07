module GeoResolver
    ( geoEncode,
      geoEncodeLanguage,
      geoDecode,
      geoDecodeLanguage,
      geoRaw
    ) where
import Internal.Requester
import Internal.Parser
import Control.Monad

geoEncode :: String -> IO (Either String (Double, Double))
geoEncode x =
   liftM (parseAnswer >=> getLocation) (requestLookup x)

geoEncodeLanguage :: String -> String -> IO (Either String (Double, Double))
geoEncodeLanguage addr lang =
    liftM (parseAnswer >=> getLocation)
        (requestRaw [("address", addr), ("language", lang)])


geoDecode :: (Double, Double) -> IO (Either String String)
geoDecode x =
    liftM (parseAnswer >=> getAddress)
        (requestReverseLookup x)

geoDecodeLanguage :: (Double, Double) -> String -> IO (Either String String)
geoDecodeLanguage (lat, lng) lang =
    liftM (parseAnswer >=> getAddress)
        (requestRaw
            [("latlng", show lat ++ ',' : show lng), ("language", lang)])


geoRaw :: [(String, String)] -> IO (Either String GoogleAnswer)
geoRaw xs = liftM parseAnswer (requestRaw xs)