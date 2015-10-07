{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-|
Module      : GeoResolver.Parser
Description : Parsing helper definitions for Googles geocoding API
Copyright   : (c) 2015, Markenwerk, Jan Greve
License     : MIT
Maintainer  : jg@markenwerk.net
-}
module GeoResolver.Parser (
    -- * Data type definition for parsed types
    Status,
    GoogleAnswer(..),
    GoogleResult(..),
    Component(..),
    Geometry(..),
    Location(..),
    -- * parsing
    parseAnswer,
    -- * conveniance functions
    getLocation,
    getAddress,
    getProperty
    ) where 

import GHC.Generics
import Data.Aeson
import Data.HashMap.Strict
import Data.Text
import Data.Maybe (fromMaybe)
import Control.Arrow
import qualified Data.ByteString.Lazy.Char8 as LBS

-- | Represents the status of the operation returned by google. Constructors represent possible 'String' values
-- according to googles documentation.
data Status = OK | ZERO_RESULTS | OVER_QUERY_LIMIT | REQUEST_DENIED | INVALID_REQUEST | UNKNOWN_ERROR
    deriving (Generic, Show)
instance FromJSON Status

-- | Represents the answer google returned.
data GoogleAnswer = GoogleAnswer {
    -- | The 'Status' returned by google.
    status :: Status,
    -- | An optional error message. 
    errorMessage :: Maybe Text,
    -- | Optional list of actual 'GoogleResult' values. 
    results :: Maybe [GoogleResult]
    } deriving (Generic, Show)
instance FromJSON GoogleAnswer where
    parseJSON (Object v) = GoogleAnswer <$>
                           v .: "status" <*>
                           v .:? "error_message" <*>
                           v .:? "results"
    parseJSON _          = mempty


-- | A single Result from the list of results from google.
data GoogleResult = GoogleResult {
    -- | List of 'Component' values for this result
    addressComponents :: [Component],
    -- | The formatted address google returned
    formattedAddress :: String,
    -- | The 'Geometry' value for this result
    geometry :: Geometry,
    -- | The google places ID for this result
    placeId :: String,
    -- | Some list of strings. Goole says:
    -- 
    -- The types[] array indicates the type of the returned result. 
    -- This array contains a set of zero or more tags identifying the type of feature returned in the result.
    -- For example, a geocode of "Chicago" returns "locality" which indicates that "Chicago" is a city, 
    -- and also returns "political" which indicates it is a political entity.
    types :: [String],
    -- | If present, hinting that this result only partially matches the requested entity.
    partialMatch :: Maybe Bool
    } deriving (Generic, Show)
instance FromJSON GoogleResult where
    parseJSON (Object v) = GoogleResult <$>
                           v .: "address_components" <*>
                           v .: "formatted_address" <*>
                           v .: "geometry" <*>
                           v .: "place_id" <*>
                           v .: "types" <*>
                           v .:? "partial_match"
    parseJSON _          = mempty

-- | A part of the address in a 'GoogleResult'
data Component = Component {
    -- | A long name for the component
    longName :: Text,
    -- | A short name for the component
    shortName :: Text,
    -- | indicating the type of the address component.
    cTypes :: [Text]
    } deriving (Generic, Show)
instance FromJSON Component where
    parseJSON (Object v) = Component <$>
                           v .: "long_name" <*>
                           v .: "short_name" <*>
                           v .: "types"
    parseJSON _          = mempty


-- | Holds geometry information about a 'GoogleResult' 
data Geometry = Geometry {
    -- | The result's location
    location :: Location,
    -- | The kind of location. As of 2015/10/07, the
    -- following values are to be expected. For future compatibility,
    -- no Enum type is introduced to map this.
    -- 
    -- * @ROOFTOP@ indicates that the returned result is a precise geocode for 
    -- which we have location information accurate down to street address precision.
    --
    -- * @RANGE_INTERPOLATED@ indicates that the returned result reflects 
    -- an approximation (usually on a road) interpolated between two precise points 
    -- (such as intersections). Interpolated results are generally returned 
    -- when rooftop geocodes are unavailable for a street address.
    --
    -- * @GEOMETRIC_CENTER@ indicates that the returned result is the geometric 
    -- center of a result such as a polyline (for example, a street) or 
    -- polygon (region).
    --
    -- * @APPROXIMATE@ indicates that the returned result is approximate.
    locationType :: Text,
    -- | contains the recommended viewport for displaying the returned result, 
    -- specified as two latitude,longitude values defining the southwest 
    -- and northeast corner of the viewport bounding box. Generally the viewport
    -- is used to frame a result when displaying it to a user.
    viewport :: HashMap Text Location
    } deriving (Generic, Show)
instance FromJSON Geometry where
    parseJSON (Object v) = Geometry <$>
                           v .: "location" <*>
                           v .: "location_type" <*>
                           v .: "viewport"
    parseJSON _          = mempty

-- | Abstraction of a geo location 
data Location = Location {
    -- | Latitude of the location
    lat :: Double,
    -- | Longitude of the location
    lng :: Double
    } deriving (Generic, Show)
instance FromJSON Location

-- | Takes a 'GoogleAnswer' and applies the function to the first 'GoogleResult'.
-- Returns a 'Left' with an error description if anything unexpected happens.
--
-- For example, 'getLocation' uses this with 
-- @
--  ((lat &&& lng) . location . geometry)
-- @
getProperty :: GoogleAnswer -- ^ The answer to process.
    -> (GoogleResult -> a) -- ^ The function to be applied to the first (if any) result.
    -> Either String a -- ^ Error or result of the function application
getProperty a f = case status a of
    OK -> fromMaybe (Left "No results.") (results a >>= (\res -> case res of
        (x:_) -> (Just . Right . f) x
        _ -> Just $ Left "Empty resultset"))
    otherwise -> Left (show otherwise ++ show (errorMessage a))

-- | Gets the location from a 'GoogleAnswer', or returns an error.
getLocation :: GoogleAnswer -> Either String (Double, Double)
getLocation a = a `getProperty` ((lat &&& lng) . location . geometry)

-- | Gets the formatted address from a GoogleAnswer (or an error)
getAddress :: GoogleAnswer -> Either String String
getAddress a = a `getProperty` formattedAddress

-- | Parses a Lazy ByteString into a 'GoogleAnswer' or returns an error describing the problem.
parseAnswer :: LBS.ByteString -> Either String GoogleAnswer
parseAnswer = eitherDecode