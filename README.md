# haskell-geo-resolver
A Haskell module offering easy access to the google geocoding web service

Please note that this package is also meant to demonstrate basic Haskell features
and thus not always takes the most elegant way but the most educational way.

## Install
### Stack
If you use [stack](https://github.com/commercialhaskell/stack), you can use the latest commit as an extra package in your stack.yaml under the packages-directive.:

````
packages:
- '.'
- location:
    git: git@git.markenwerk.net:werkstuecke/HaskellGeoResolver.git
    commit: whatever hash you'd like to use
````

### Otherwise
The module is a cabal package. Have fun.

### Haddock
The package is documented using Haddock.

## Usage
A minimum example of usage (with @OverloadedStrings@) is:

````
import GeoRequester
main = geoRequest "Lornsenstraße 43, Kiel" >>= putStrLn . show
````

While there are convenience functions for the most common use cases,
there are means to send arbitrary requests.

If you do so, please see "Geocoder.Parser" for ways to access the results.
As a hint, 'GoogleAnswer' is instance of 'Foldable' and 'Functor'.