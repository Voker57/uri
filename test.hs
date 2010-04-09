import Data.Maybe
import Text.URI
import Test.HUnit

complicatedQuery = assertEqual "Complicated query" [("name","admin"),("pass","admin"),("query","select * from dbusers;"),("dbact","sel"),("send","Send")] (queryToPairs "name=admin&pass=admin&query=select+*+from+dbusers%3B&dbact=sel&send=Send")

longRACURI =  "https://user:shmuser@www.rentacoder.com:85/RentACoder/authentication/MyRegistration/CoderInfo.asp?lngRegistrationScreenMode=3&strRegScreenReturnURL=https://www.rentacoder.com/RentACoder/authentication/MyRegistration/MyToDoList.asp?lngUserTypeId=2#wtf?gh"

preParsedLongRACURI = URI {
	uriScheme = Just "https",
	uriPath = "/RentACoder/authentication/MyRegistration/CoderInfo.asp",
	uriPort = Just 85,
	uriQuery = Just "lngRegistrationScreenMode=3&strRegScreenReturnURL=https://www.rentacoder.com/RentACoder/authentication/MyRegistration/MyToDoList.asp?lngUserTypeId=2",
	uriRegName = Just "www.rentacoder.com",
	uriUserInfo = Just "user:shmuser",
	uriFragment = Just "wtf?gh"
	}

parsedLongRACURI = fromMaybe (error "Terrible parsing error") $ parseURI longRACURI

componentTests = map (\(method, message) -> (assertEqual (message ++ " is equal in pre-parsed and parsed URI") (method preParsedLongRACURI) (method parsedLongRACURI))) [
	(uriScheme, "scheme"),
	(uriQuery, "query"),
	(uriRegName, "username"),
	(uriFragment, "URI fragment")
	] ++ {- few special cases -} [
			assertEqual "port is equal in pre-parsed and parsed URI" (uriPort preParsedLongRACURI) (uriPort parsedLongRACURI),
			assertEqual "path is equal in pre-parsed and parsed URI" (uriPath preParsedLongRACURI) (uriPath parsedLongRACURI)
			]

pathSegmentsTest = assertEqual "path is correctly split to segments" ["","RentACoder","authentication","MyRegistration","CoderInfo.asp"] (uriPathSegments parsedLongRACURI)

queryItemsTest = assertEqual "URI query is properly parsed to items" [("lngRegistrationScreenMode","3"),("strRegScreenReturnURL","https://www.rentacoder.com/RentACoder/authentication/MyRegistration/MyToDoList.asp?lngUserTypeId=2")] (uriQueryItems parsedLongRACURI)


tests = TestList $ map (TestLabel "" . TestCase) $ complicatedQuery : queryItemsTest : pathSegmentsTest : componentTests


main = runTestTT tests