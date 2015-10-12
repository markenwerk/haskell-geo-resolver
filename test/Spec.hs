import Network.Google.GeoResolver
import Network.Google.GeoResolver.Parser
import Test.Framework (defaultMain, testGroup)
import Test.HUnit
import Test.HUnit.Base
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Base64.Lazy (decodeLenient)
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests


tests = [

        testGroup "Resolving (Encode)" 
            [testCase "Resolving Home" testResolvingHere,
            testCase "Resolving Empty String" testResolvNowhere],
        testGroup "Reverse resolving (Decoding)"
            [
            testCase "Placeholder" ( True @=? True),
            testCase "Placeholder 2" ( True @=? True),
            testCase "Placeholder 3" ( True @=? True)



            ],

        testGroup "Parsing JSON"
            [testCase "invalid JSON" testInvalid,
            testCase "valid, but not sufficient" testValidMissing,
            testCase "Google Example" testExample,
            testCase "Google Example 2" testExample2
        ]




    ]

testInvalid = True @=? parserResult `hasLeft` (const True)
    where parserResult = parseAnswer (LBS.pack "[}")

testValidMissing = True @=? parserResult `hasLeft` (const True)
    where parserResult = parseAnswer (LBS.pack "{\"not status\":\"OK\"}")

testExample = True @=? parserResult `hasRight` (const True)
    where parserResult = parseAnswer (decodeLenient $ LBS.pack "ew0KICAgInJlc3VsdHMiIDogWw0KICAgICAgew0KICAgICAgICAgImFkZHJlc3NfY29tcG9uZW50cyIgOiBbDQogICAgICAgICAgICB7DQogICAgICAgICAgICAgICAibG9uZ19uYW1lIiA6ICJTYW50YSBDcnV6IGRlIFRlbmVyaWZlIiwNCiAgICAgICAgICAgICAgICJzaG9ydF9uYW1lIiA6ICJTYW50YSBDcnV6IGRlIFRlbmVyaWZlIiwNCiAgICAgICAgICAgICAgICJ0eXBlcyIgOiBbICJsb2NhbGl0eSIsICJwb2xpdGljYWwiIF0NCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICB7DQogICAgICAgICAgICAgICAibG9uZ19uYW1lIiA6ICJTYW50YSBDcnV6IGRlIFRlbmVyaWZlIiwNCiAgICAgICAgICAgICAgICJzaG9ydF9uYW1lIiA6ICJTYW50YSBDcnV6IGRlIFRlbmVyaWZlIiwNCiAgICAgICAgICAgICAgICJ0eXBlcyIgOiBbICJhZG1pbmlzdHJhdGl2ZV9hcmVhX2xldmVsXzQiLCAicG9saXRpY2FsIiBdDQogICAgICAgICAgICB9LA0KICAgICAgICAgICAgew0KICAgICAgICAgICAgICAgImxvbmdfbmFtZSIgOiAiQW5hZ2EiLA0KICAgICAgICAgICAgICAgInNob3J0X25hbWUiIDogIkFuYWdhIiwNCiAgICAgICAgICAgICAgICJ0eXBlcyIgOiBbICJhZG1pbmlzdHJhdGl2ZV9hcmVhX2xldmVsXzMiLCAicG9saXRpY2FsIiBdDQogICAgICAgICAgICB9LA0KICAgICAgICAgICAgew0KICAgICAgICAgICAgICAgImxvbmdfbmFtZSIgOiAiU2FudGEgQ3J1eiBkZSBUZW5lcmlmZSIsDQogICAgICAgICAgICAgICAic2hvcnRfbmFtZSIgOiAiVEYiLA0KICAgICAgICAgICAgICAgInR5cGVzIiA6IFsgImFkbWluaXN0cmF0aXZlX2FyZWFfbGV2ZWxfMiIsICJwb2xpdGljYWwiIF0NCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICB7DQogICAgICAgICAgICAgICAibG9uZ19uYW1lIiA6ICJDYW5hcmlhcyIsDQogICAgICAgICAgICAgICAic2hvcnRfbmFtZSIgOiAiQ04iLA0KICAgICAgICAgICAgICAgInR5cGVzIiA6IFsgImFkbWluaXN0cmF0aXZlX2FyZWFfbGV2ZWxfMSIsICJwb2xpdGljYWwiIF0NCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICB7DQogICAgICAgICAgICAgICAibG9uZ19uYW1lIiA6ICJTcGFpbiIsDQogICAgICAgICAgICAgICAic2hvcnRfbmFtZSIgOiAiRVMiLA0KICAgICAgICAgICAgICAgInR5cGVzIiA6IFsgImNvdW50cnkiLCAicG9saXRpY2FsIiBdDQogICAgICAgICAgICB9DQogICAgICAgICBdLA0KICAgICAgICAgImZvcm1hdHRlZF9hZGRyZXNzIiA6ICJTYW50YSBDcnV6IGRlIFRlbmVyaWZlLCBTYW50YSBDcnV6IGRlIFRlbmVyaWZlLCBTcGFpbiIsDQogICAgICAgICAiZ2VvbWV0cnkiIDogew0KICAgICAgICAgICAgImJvdW5kcyIgOiB7DQogICAgICAgICAgICAgICAibm9ydGhlYXN0IiA6IHsNCiAgICAgICAgICAgICAgICAgICJsYXQiIDogMjguNDg3NjE2LA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAtMTYuMjM1NjY0Ng0KICAgICAgICAgICAgICAgfSwNCiAgICAgICAgICAgICAgICJzb3V0aHdlc3QiIDogew0KICAgICAgICAgICAgICAgICAgImxhdCIgOiAyOC40MjgwMjQ4LA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAtMTYuMzM3MDA0NQ0KICAgICAgICAgICAgICAgfQ0KICAgICAgICAgICAgfSwNCiAgICAgICAgICAgICJsb2NhdGlvbiIgOiB7DQogICAgICAgICAgICAgICAibGF0IiA6IDI4LjQ2MzYyOTYsDQogICAgICAgICAgICAgICAibG5nIiA6IC0xNi4yNTE4NDY3DQogICAgICAgICAgICB9LA0KICAgICAgICAgICAgImxvY2F0aW9uX3R5cGUiIDogIkFQUFJPWElNQVRFIiwNCiAgICAgICAgICAgICJ2aWV3cG9ydCIgOiB7DQogICAgICAgICAgICAgICAibm9ydGhlYXN0IiA6IHsNCiAgICAgICAgICAgICAgICAgICJsYXQiIDogMjguNDg3NjE2LA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAtMTYuMjM1NjY0Ng0KICAgICAgICAgICAgICAgfSwNCiAgICAgICAgICAgICAgICJzb3V0aHdlc3QiIDogew0KICAgICAgICAgICAgICAgICAgImxhdCIgOiAyOC40MjgwMjQ4LA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAtMTYuMzM3MDA0NQ0KICAgICAgICAgICAgICAgfQ0KICAgICAgICAgICAgfQ0KICAgICAgICAgfSwNCiAgICAgICAgICJwbGFjZV9pZCIgOiAiQ2hJSmNVRWx6T3pNUVF3Ukx1VjMwbk1VRVVNIiwNCiAgICAgICAgICJ0eXBlcyIgOiBbICJsb2NhbGl0eSIsICJwb2xpdGljYWwiIF0NCiAgICAgIH0NCiAgIF0sDQogICAic3RhdHVzIiA6ICJPSyINCn0NCg==")

testExample2 = True @=? parserResult `hasRight` (const True)
    where parserResult = parseAnswer (decodeLenient $ LBS.pack "ew0KICAgInJlc3VsdHMiIDogWw0KICAgICAgew0KICAgICAgICAgImFkZHJlc3NfY29tcG9uZW50cyIgOiBbDQogICAgICAgICAgICB7DQogICAgICAgICAgICAgICAibG9uZ19uYW1lIiA6ICJBbm5lZ2F0YW4iLA0KICAgICAgICAgICAgICAgInNob3J0X25hbWUiIDogIkFubmVnYXRhbiIsDQogICAgICAgICAgICAgICAidHlwZXMiIDogWyAicm91dGUiIF0NCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICB7DQogICAgICAgICAgICAgICAibG9uZ19uYW1lIiA6ICJIZWxzaW5nZm9ycyIsDQogICAgICAgICAgICAgICAic2hvcnRfbmFtZSIgOiAiSGVsc2luZ2ZvcnMiLA0KICAgICAgICAgICAgICAgInR5cGVzIiA6IFsgImFkbWluaXN0cmF0aXZlX2FyZWFfbGV2ZWxfMyIsICJwb2xpdGljYWwiIF0NCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICB7DQogICAgICAgICAgICAgICAibG9uZ19uYW1lIiA6ICJGaW5sYW5kIiwNCiAgICAgICAgICAgICAgICJzaG9ydF9uYW1lIiA6ICJGSSIsDQogICAgICAgICAgICAgICAidHlwZXMiIDogWyAiY291bnRyeSIsICJwb2xpdGljYWwiIF0NCiAgICAgICAgICAgIH0NCiAgICAgICAgIF0sDQogICAgICAgICAiZm9ybWF0dGVkX2FkZHJlc3MiIDogIkFubmVnYXRhbiwgSGVsc2luZ2ZvcnMsIEZpbmxhbmQiLA0KICAgICAgICAgImdlb21ldHJ5IiA6IHsNCiAgICAgICAgICAgICJib3VuZHMiIDogew0KICAgICAgICAgICAgICAgIm5vcnRoZWFzdCIgOiB7DQogICAgICAgICAgICAgICAgICAibGF0IiA6IDYwLjE2ODk5NywNCiAgICAgICAgICAgICAgICAgICJsbmciIDogMjQuOTQyNzk1OQ0KICAgICAgICAgICAgICAgfSwNCiAgICAgICAgICAgICAgICJzb3V0aHdlc3QiIDogew0KICAgICAgICAgICAgICAgICAgImxhdCIgOiA2MC4xNjI2NjI3LA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAyNC45MzQNCiAgICAgICAgICAgICAgIH0NCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICAibG9jYXRpb24iIDogew0KICAgICAgICAgICAgICAgImxhdCIgOiA2MC4xNjU3ODA4LA0KICAgICAgICAgICAgICAgImxuZyIgOiAyNC45Mzg0NTENCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICAibG9jYXRpb25fdHlwZSIgOiAiR0VPTUVUUklDX0NFTlRFUiIsDQogICAgICAgICAgICAidmlld3BvcnQiIDogew0KICAgICAgICAgICAgICAgIm5vcnRoZWFzdCIgOiB7DQogICAgICAgICAgICAgICAgICAibGF0IiA6IDYwLjE2ODk5NywNCiAgICAgICAgICAgICAgICAgICJsbmciIDogMjQuOTQyNzk1OQ0KICAgICAgICAgICAgICAgfSwNCiAgICAgICAgICAgICAgICJzb3V0aHdlc3QiIDogew0KICAgICAgICAgICAgICAgICAgImxhdCIgOiA2MC4xNjI2NjI3LA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAyNC45MzQNCiAgICAgICAgICAgICAgIH0NCiAgICAgICAgICAgIH0NCiAgICAgICAgIH0sDQogICAgICAgICAicGxhY2VfaWQiIDogIkNoSUpBUlc3QzhzTGtrWVJnbDRqZTQtUlBVTSIsDQogICAgICAgICAidHlwZXMiIDogWyAicm91dGUiIF0NCiAgICAgIH0sDQogICAgICB7DQogICAgICAgICAiYWRkcmVzc19jb21wb25lbnRzIiA6IFsNCiAgICAgICAgICAgIHsNCiAgICAgICAgICAgICAgICJsb25nX25hbWUiIDogIkFubmV2w6RnZW4iLA0KICAgICAgICAgICAgICAgInNob3J0X25hbWUiIDogIkFubmV2w6RnZW4iLA0KICAgICAgICAgICAgICAgInR5cGVzIiA6IFsgInJvdXRlIiBdDQogICAgICAgICAgICB9LA0KICAgICAgICAgICAgew0KICAgICAgICAgICAgICAgImxvbmdfbmFtZSIgOiAiVmFuZGEiLA0KICAgICAgICAgICAgICAgInNob3J0X25hbWUiIDogIlZhbmRhIiwNCiAgICAgICAgICAgICAgICJ0eXBlcyIgOiBbICJhZG1pbmlzdHJhdGl2ZV9hcmVhX2xldmVsXzMiLCAicG9saXRpY2FsIiBdDQogICAgICAgICAgICB9LA0KICAgICAgICAgICAgew0KICAgICAgICAgICAgICAgImxvbmdfbmFtZSIgOiAiRmlubGFuZCIsDQogICAgICAgICAgICAgICAic2hvcnRfbmFtZSIgOiAiRkkiLA0KICAgICAgICAgICAgICAgInR5cGVzIiA6IFsgImNvdW50cnkiLCAicG9saXRpY2FsIiBdDQogICAgICAgICAgICB9LA0KICAgICAgICAgICAgew0KICAgICAgICAgICAgICAgImxvbmdfbmFtZSIgOiAiMDE0MjAiLA0KICAgICAgICAgICAgICAgInNob3J0X25hbWUiIDogIjAxNDIwIiwNCiAgICAgICAgICAgICAgICJ0eXBlcyIgOiBbICJwb3N0YWxfY29kZSIgXQ0KICAgICAgICAgICAgfQ0KICAgICAgICAgXSwNCiAgICAgICAgICJmb3JtYXR0ZWRfYWRkcmVzcyIgOiAiQW5uZXbDpGdlbiwgMDE0MjAgVmFuZGEsIEZpbmxhbmQiLA0KICAgICAgICAgImdlb21ldHJ5IiA6IHsNCiAgICAgICAgICAgICJib3VuZHMiIDogew0KICAgICAgICAgICAgICAgIm5vcnRoZWFzdCIgOiB7DQogICAgICAgICAgICAgICAgICAibGF0IiA6IDYwLjMyODI3MzgsDQogICAgICAgICAgICAgICAgICAibG5nIiA6IDI1LjExNjIxNjMNCiAgICAgICAgICAgICAgIH0sDQogICAgICAgICAgICAgICAic291dGh3ZXN0IiA6IHsNCiAgICAgICAgICAgICAgICAgICJsYXQiIDogNjAuMzI1NjQwMDk5OTk5OTksDQogICAgICAgICAgICAgICAgICAibG5nIiA6IDI1LjEwNzY0NzQNCiAgICAgICAgICAgICAgIH0NCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICAibG9jYXRpb24iIDogew0KICAgICAgICAgICAgICAgImxhdCIgOiA2MC4zMjcxMDY5LA0KICAgICAgICAgICAgICAgImxuZyIgOiAyNS4xMTE4MDQ2DQogICAgICAgICAgICB9LA0KICAgICAgICAgICAgImxvY2F0aW9uX3R5cGUiIDogIkdFT01FVFJJQ19DRU5URVIiLA0KICAgICAgICAgICAgInZpZXdwb3J0IiA6IHsNCiAgICAgICAgICAgICAgICJub3J0aGVhc3QiIDogew0KICAgICAgICAgICAgICAgICAgImxhdCIgOiA2MC4zMjgzMDU5MzAyOTE1LA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAyNS4xMTYyMTYzDQogICAgICAgICAgICAgICB9LA0KICAgICAgICAgICAgICAgInNvdXRod2VzdCIgOiB7DQogICAgICAgICAgICAgICAgICAibGF0IiA6IDYwLjMyNTYwNzk2OTcwODQ5LA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAyNS4xMDc2NDc0DQogICAgICAgICAgICAgICB9DQogICAgICAgICAgICB9DQogICAgICAgICB9LA0KICAgICAgICAgInBhcnRpYWxfbWF0Y2giIDogdHJ1ZSwNCiAgICAgICAgICJwbGFjZV9pZCIgOiAiQ2hJSjNVSkNOdDRHa2tZUjgtX2E4RGgyNWtBIiwNCiAgICAgICAgICJ0eXBlcyIgOiBbICJyb3V0ZSIgXQ0KICAgICAgfSwNCiAgICAgIHsNCiAgICAgICAgICJhZGRyZXNzX2NvbXBvbmVudHMiIDogWw0KICAgICAgICAgICAgew0KICAgICAgICAgICAgICAgImxvbmdfbmFtZSIgOiAiQW5uZXBsYXRzZW4iLA0KICAgICAgICAgICAgICAgInNob3J0X25hbWUiIDogIkFubmVwbGF0c2VuIiwNCiAgICAgICAgICAgICAgICJ0eXBlcyIgOiBbICJyb3V0ZSIgXQ0KICAgICAgICAgICAgfSwNCiAgICAgICAgICAgIHsNCiAgICAgICAgICAgICAgICJsb25nX25hbWUiIDogIkhlbHNpbmdmb3JzIiwNCiAgICAgICAgICAgICAgICJzaG9ydF9uYW1lIiA6ICJIZWxzaW5nZm9ycyIsDQogICAgICAgICAgICAgICAidHlwZXMiIDogWyAiYWRtaW5pc3RyYXRpdmVfYXJlYV9sZXZlbF8zIiwgInBvbGl0aWNhbCIgXQ0KICAgICAgICAgICAgfSwNCiAgICAgICAgICAgIHsNCiAgICAgICAgICAgICAgICJsb25nX25hbWUiIDogIkZpbmxhbmQiLA0KICAgICAgICAgICAgICAgInNob3J0X25hbWUiIDogIkZJIiwNCiAgICAgICAgICAgICAgICJ0eXBlcyIgOiBbICJjb3VudHJ5IiwgInBvbGl0aWNhbCIgXQ0KICAgICAgICAgICAgfSwNCiAgICAgICAgICAgIHsNCiAgICAgICAgICAgICAgICJsb25nX25hbWUiIDogIjAwMTAwIiwNCiAgICAgICAgICAgICAgICJzaG9ydF9uYW1lIiA6ICIwMDEwMCIsDQogICAgICAgICAgICAgICAidHlwZXMiIDogWyAicG9zdGFsX2NvZGUiIF0NCiAgICAgICAgICAgIH0NCiAgICAgICAgIF0sDQogICAgICAgICAiZm9ybWF0dGVkX2FkZHJlc3MiIDogIkFubmVwbGF0c2VuLCAwMDEwMCBIZWxzaW5nZm9ycywgRmlubGFuZCIsDQogICAgICAgICAiZ2VvbWV0cnkiIDogew0KICAgICAgICAgICAgImJvdW5kcyIgOiB7DQogICAgICAgICAgICAgICAibm9ydGhlYXN0IiA6IHsNCiAgICAgICAgICAgICAgICAgICJsYXQiIDogNjAuMTY5NTY2NCwNCiAgICAgICAgICAgICAgICAgICJsbmciIDogMjQuOTM1NzEyNQ0KICAgICAgICAgICAgICAgfSwNCiAgICAgICAgICAgICAgICJzb3V0aHdlc3QiIDogew0KICAgICAgICAgICAgICAgICAgImxhdCIgOiA2MC4xNjg5OTcsDQogICAgICAgICAgICAgICAgICAibG5nIiA6IDI0LjkzNA0KICAgICAgICAgICAgICAgfQ0KICAgICAgICAgICAgfSwNCiAgICAgICAgICAgICJsb2NhdGlvbiIgOiB7DQogICAgICAgICAgICAgICAibGF0IiA6IDYwLjE2OTI3NDEsDQogICAgICAgICAgICAgICAibG5nIiA6IDI0LjkzNDgwMTYNCiAgICAgICAgICAgIH0sDQogICAgICAgICAgICAibG9jYXRpb25fdHlwZSIgOiAiR0VPTUVUUklDX0NFTlRFUiIsDQogICAgICAgICAgICAidmlld3BvcnQiIDogew0KICAgICAgICAgICAgICAgIm5vcnRoZWFzdCIgOiB7DQogICAgICAgICAgICAgICAgICAibGF0IiA6IDYwLjE3MDYzMDY4MDI5MTUxLA0KICAgICAgICAgICAgICAgICAgImxuZyIgOiAyNC45MzYyMDUyMzAyOTE1DQogICAgICAgICAgICAgICB9LA0KICAgICAgICAgICAgICAgInNvdXRod2VzdCIgOiB7DQogICAgICAgICAgICAgICAgICAibGF0IiA6IDYwLjE2NzkzMjcxOTcwODUsDQogICAgICAgICAgICAgICAgICAibG5nIiA6IDI0LjkzMzUwNzI2OTcwODUNCiAgICAgICAgICAgICAgIH0NCiAgICAgICAgICAgIH0NCiAgICAgICAgIH0sDQogICAgICAgICAicGFydGlhbF9tYXRjaCIgOiB0cnVlLA0KICAgICAgICAgInBsYWNlX2lkIiA6ICJDaElKZWFoTXFzd0xra1lSMnZRZkcxbkhJM00iLA0KICAgICAgICAgInR5cGVzIiA6IFsgInJvdXRlIiBdDQogICAgICB9DQogICBdLA0KICAgInN0YXR1cyIgOiAiT0siDQp9DQo=")

testResolvingHere :: Assertion
testResolvingHere = do
    result <- geoEncode Nothing "Lornsenstraße 43\nKiel"
    True @=? result `hasRight` (\(lat, long) -> and [lat < 55, lat > 54, long < 11, long > 10])

testResolvNowhere :: Assertion
testResolvNowhere = do
    result <- geoEncode Nothing ""
    False @=? result `hasLeft` (\x -> x == "INVALID_REQUEST")


hasRight :: Either a b -> (b -> Bool) -> Bool
(Left _) `hasRight` _ = False
(Right x) `hasRight`  f = f x

hasLeft :: Either a b -> (a -> Bool) -> Bool
(Right _) `hasLeft` _ = False
(Left x) `hasLeft`  f = f x