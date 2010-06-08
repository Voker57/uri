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

testBase = "http://a/b/c/d;p?q"

testRel u b e = let
	baseM = parseURI b;
	uriM = parseURI u
		in case (baseM, uriM) of
			(Just base, Just uri) -> assertEqual ("For base "++ show base ++", URI " ++ show uri) e (show $ mergeURIs base uri)
			otherwise -> assertFailure ("Got error" ++ show e)

test1 = TestCase (testRel "g:h" testBase "g:h")
test2 = TestCase (testRel "g" testBase "http://a/b/c/g")
test3 = TestCase (testRel "./g" testBase "http://a/b/c/g")
test4 = TestCase (testRel "g/" testBase "http://a/b/c/g/")
test5 = TestCase (testRel "/g" testBase "http://a/g")
test6 = TestCase (testRel "//g" testBase "http://g")
test7 = TestCase (testRel "?y" testBase "http://a/b/c/d;p?y")
test8 = TestCase (testRel "g?y" testBase "http://a/b/c/g?y")
test9 = TestCase (testRel "#s" testBase "http://a/b/c/d;p?q#s")
test10 = TestCase (testRel "g#s" testBase "http://a/b/c/g#s")
test11 = TestCase (testRel "g?y#s" testBase "http://a/b/c/g?y#s")
test12 = TestCase (testRel ";x" testBase "http://a/b/c/;x")
test13 = TestCase (testRel "g;x" testBase "http://a/b/c/g;x")
test14 = TestCase (testRel "g;x?y#s" testBase "http://a/b/c/g;x?y#s")
test15 = TestCase (testRel "" testBase testBase)
test16 = TestCase (testRel "." testBase "http://a/b/c/")
test17 = TestCase (testRel "./" testBase "http://a/b/c/")
test18 = TestCase (testRel ".." testBase "http://a/b/")
test19 = TestCase (testRel "../" testBase "http://a/b/")
test20 = TestCase (testRel "../g" testBase "http://a/b/g")
test21 = TestCase (testRel "../.." testBase "http://a/")
test22 = TestCase (testRel "../../" testBase "http://a/")
test23 = TestCase (testRel "../../g" testBase "http://a/g")
test24 = TestCase (testRel "../../../g" testBase "http://a/g")
test25 = TestCase (testRel "../../../../g" testBase "http://a/g")

test26 = TestCase (testRel "/./g" testBase "http://a/g")
test27 = TestCase (testRel "/../g" testBase "http://a/g")
test28 = TestCase (testRel "g." testBase "http://a/b/c/g.")
test29 = TestCase (testRel ".g" testBase "http://a/b/c/.g")
test30 = TestCase (testRel "g.." testBase "http://a/b/c/g..")
test31 = TestCase (testRel "..g" testBase "http://a/b/c/..g")
test32 = TestCase (testRel "./../g" testBase "http://a/b/g")
test33 = TestCase (testRel "./g/." testBase "http://a/b/c/g/")
test34 = TestCase (testRel "g/./h" testBase "http://a/b/c/g/h")
test35 = TestCase (testRel "g/../h" testBase "http://a/b/c/h")
test36 = TestCase (testRel "g;x=1/./y" testBase "http://a/b/c/g;x=1/y")
test37 = TestCase (testRel "g;x=1/../y" testBase "http://a/b/c/y")
test38 = TestCase (testRel "g?y/./x" testBase "http://a/b/c/g?y/./x")
test39 = TestCase (testRel "g?y/../x" testBase "http://a/b/c/g?y/../x")
test40 = TestCase (testRel "g#s/./x" testBase "http://a/b/c/g#s/./x")
test41 = TestCase (testRel "g#s/../x" testBase "http://a/b/c/g#s/../x")
allTests = [test1,test2,test3,test4,test5,
                     test6,test7,test8,test9,test10,
                     test11,test12,test13,test14,test15,
                     test16,test17,test18,test19,test20,
                     test21,test22,test23,test24,test25,
                     test26,test27,test28,test29,test30,
                     test31,test32,test33,test34,test35,
                     test36,test37,test38,test39,test40,
                     test41]


tests = TestList $ (map (TestLabel "" . TestCase) $ complicatedQuery : queryItemsTest : pathSegmentsTest : componentTests) ++ allTests


main = runTestTT tests