{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Internal.Parser (
    -- * Data type definition for parsed types
    GoogleAnswer,
    GoogleResult,
    Component,
    Geometry,
    Location,
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

-- | Represents the status of the operation returned by google. Constructors represent possible 'String' values.
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
    -- > The types[] array indicates the type of the returned result. 
    -- > This array contains a set of zero or more tags identifying the type of feature returned in the result.
    -- > For example, a geocode of "Chicago" returns "locality" which indicates that "Chicago" is a city, 
    -- > and also returns "political" which indicates it is a political entity.
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


data Component = Component {
    longName :: Text,
    shortName :: Text,
    cTypes :: [String]
    } deriving (Generic, Show)
instance FromJSON Component where
    parseJSON (Object v) = Component <$>
                           v .: "long_name" <*>
                           v .: "short_name" <*>
                           v .: "types"
    parseJSON _          = mempty

data LocationType = ROOFTOP | RANGE_INTERPOLATED | GEOMETRIC_CENTER | APPROXIMATE
    deriving (Generic, Show)
instance FromJSON LocationType

data Geometry = Geometry {
    location :: Location,
    locationType :: LocationType,
    viewport :: HashMap String Location
    } deriving (Generic, Show)
instance FromJSON Geometry where
    parseJSON (Object v) = Geometry <$>
                           v .: "location" <*>
                           v .: "location_type" <*>
                           v .: "viewport"
    parseJSON _          = mempty

data Location = Location {
    lat :: Double,
    lng :: Double
    } deriving (Generic, Show)
instance FromJSON Location

getProperty :: GoogleAnswer -> (GoogleResult -> a) -> Either String a
getProperty a f = case status a of
    OK -> fromMaybe (Left "No results.") (results a >>= (\res -> case res of
        (x:_) -> (Just . Right . f) x
        _ -> Just $ Left "Empty resultset"))
    otherwise -> Left (show otherwise ++ show (errorMessage a))

getLocation :: GoogleAnswer -> Either String (Double, Double)
getLocation a = a `getProperty` ((lat &&& lng) . location . geometry)

getAddress :: GoogleAnswer -> Either String String
getAddress a = a `getProperty` formattedAddress

parseAnswer :: LBS.ByteString -> Either String GoogleAnswer
parseAnswer = eitherDecode