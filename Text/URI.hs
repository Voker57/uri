{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Text.URI (
	URI(..)
	, dereferencePath
	, dereferencePathString
	, escapeString
	, isReference
	, isRelative
	, nullURI
	, okInFragment
	, okInPath
	, okInPathSegment
	, okInQuery
	, okInQueryItem
	, okInUserinfo
	, mergePaths
	, mergePathStrings
	, mergeURIs
	, mergeURIStrings
	, pairsToQuery
	, parseURI
	, pathToSegments
	, segmentsToPath
	, queryToPairs
	, unescapeString
	, uriPathSegments
	, uriQueryItems
	) where

import Codec.Binary.UTF8.String
import Data.Char
import Data.Data
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Word
import Safe
import Text.Parsec
import Text.Printf
------------------------------------------------------------
--  The URI datatype
------------------------------------------------------------

-- |Represents a general universal resource identifier using
--  its component parts.
--
--  For example, for the URI
--
--  >   foo://anonymous@www.haskell.org:42/ghc?query#frag
--
--  the components are:
--

data URI = URI {
	uriScheme :: Maybe String -- ^ @foo@
	, uriUserInfo :: Maybe String -- ^ @anonymous@
	, uriRegName :: Maybe String -- ^ @www.haskell.org@
	, uriPort :: Maybe Integer -- ^ @42@
	, uriPath :: String -- ^ @/ghc@
	, uriQuery :: Maybe String -- ^ @query@
	, uriFragment :: Maybe String -- ^ @frag@
	} deriving (Eq, Ord, Typeable, Data)

-- | Blank URI
nullURI :: URI
nullURI = URI {
	uriScheme = Nothing
	, uriRegName = Nothing
	, uriUserInfo = Nothing
	, uriPort = Nothing
	, uriPath = ""
	, uriQuery = Nothing
	, uriFragment = Nothing
	}

instance Show URI where
	show u = concat [
		maybe "" (++ ":") $ uriScheme u
		, if (isJust $ uriRegName u) then "//" else ""
		, maybe "" (++ "@") $ uriUserInfo u
		, fromMaybe "" $ uriRegName u
		, maybe "" (\s -> ":" ++ show s) $ uriPort u
		, if (isJust $ uriRegName u) && (not ("/" `isPrefixOf` uriPath u || uriPath u == "")) then ("/" ++ uriPath u) else uriPath u
		, maybe "" ("?" ++) $ uriQuery u
		, maybe "" ("#" ++) $ uriFragment u
		]

-- | Checks if character is OK in userinfo
okInUserinfo :: Char -> Bool
okInUserinfo = satisfiesAny [isUnreserved, isSubDelim, (==':')]
-- | Checks if character is OK in query
okInQuery :: Char -> Bool
okInQuery = satisfiesAny [isPChar, (`elem` "/?")]
-- | Checks if character is OK in urlencoded query item
okInQueryItem :: Char -> Bool
okInQueryItem c = okInQuery c && (not $ elem c "&=")
-- | Checks if character is OK in fragment
okInFragment :: Char -> Bool
okInFragment = okInQuery
-- | Checks if character is OK in path
okInPath :: Char -> Bool
okInPath = satisfiesAny [isPChar, (`elem` "/@")]
-- | Checks if character is ok in path segment
okInPathSegment :: Char -> Bool
okInPathSegment = satisfiesAny [isPChar, (== '@')]

-- | Parses URI
parseURI :: String -> Maybe URI
parseURI s = either (const Nothing) (Just) $ parse uriP "user input" s

-- | Escapes one char, see escapeString
escapeChar :: (Char -> Bool) -> Char -> String
escapeChar f c = if f c && c /= '%' then [c] else concat $ map (printf "%%%0.2X") (encode [c])

-- | Escapes string, using predicate to determine whether character is allowed
escapeString :: (Char -> Bool) -> String -> String
escapeString f s = concat $ map (escapeChar f) s

-- | Checks if uri is a reference
isReference :: URI -> Bool
isReference u = all (isNothing) [uriRegName u, uriScheme u]

-- | Checks if uri is relative
isRelative :: URI -> Bool
isRelative u = isReference u && (headDef ' ' (uriPath u) /= '/')

-- | Composes www-urlencoded query from key-value pairs
pairsToQuery :: [(String, String)] -> String
pairsToQuery = initSafe . foldl (\rest (k,v) -> concat [
	rest
	, escapeString (okInQueryItem) k
	, "="
	, escapeString (okInQueryItem) v
	, "&"
	]) ""

-- | Parses www-urlencoded string to key-value pairs
queryToPairs :: String -> [(String, String)]
queryToPairs q = either (const []) (id) $ parse urlEncodedPairsP "query" q

-- | Unescapes percent-sequences
unescapeString :: String -> String
unescapeString s = either (const s) (id) $ parse (many $ percentEncodedP <|> anyChar) "escaped text" s

-- | Convenience function for extracting www-urlencoded data
uriQueryItems :: URI -> [(String, String)]
uriQueryItems = maybe [] (queryToPairs) . uriQuery

-- | Splits path to segments
pathToSegments :: String -> [String]
pathToSegments = explode '/'

-- | Convenience function for extracting path segments
uriPathSegments :: URI -> [String]
uriPathSegments = pathToSegments . uriPath

-- | Joins path segments, with escaping
segmentsToPath :: [String] -> String
segmentsToPath [""] = "/"
segmentsToPath ss = intercalate "/" $ map (escapeString (okInPathSegment)) ss

-- | Merges two URIs
mergeURIs :: URI -- ^ Base URI
	-> URI -- ^ Reference URI
	-> URI -- ^ Resulting URI
mergeURIs t r = if isJust (uriScheme r) then
	t { uriScheme = uriScheme r
		, uriRegName = uriRegName r
		, uriPort = uriPort r
		, uriUserInfo = uriUserInfo r
		, uriPath = dereferencePathString (uriPath r)
		, uriQuery = uriQuery r
		, uriFragment = uriFragment r
		}
	else
	if isJust (uriRegName r) then
		t { uriRegName = uriRegName r
			, uriPort = uriPort r
			, uriUserInfo = uriUserInfo r
			, uriPath = dereferencePathString (uriPath r)
			, uriQuery = uriQuery r
			, uriFragment = uriFragment r
			}
		else -- Not 100% sure about how good i translated this, but seems right.
		if uriPath r == "" then
			t { uriQuery = maybe (uriQuery t) (Just) $ uriQuery r
				, uriPath = uriPath t
				, uriFragment = uriFragment r
				}
			else
			t { uriQuery = uriQuery r
				, uriPath = mergePathStrings (uriPath t) (uriPath r)
				, uriFragment = uriFragment r
				}

-- | mergeURIs for strings
mergeURIStrings :: String -> String -> String
mergeURIStrings s1 s2 = show $ mergeURIs (fromMaybe nullURI $ parseURI s1) (fromMaybe nullURI $ parseURI s2)

-- | mergePaths for strings
mergePathStrings :: String -> String -> String
mergePathStrings p1 p2 = segmentsToPath $ mergePaths (pathToSegments p1) (pathToSegments p2)

-- | Merges two paths
mergePaths :: [String] -> [String] -> [String]
mergePaths p1 p2@("":_) = dereferencePath p2
mergePaths p1 [] = dereferencePath p1
mergePaths p1 p2 = dereferencePath ((initSafe p1 ++ ["."]) ++ p2)

-- | Removes ".." and "." from path
dereferencePath :: [String] -> [String]
dereferencePath = reverse . dereferencePath' [] . map (\s -> if s == "" then "." else s)

-- | dereferencePath for strings
dereferencePathString :: String -> String
dereferencePathString = segmentsToPath . dereferencePath . pathToSegments

-- Private functions

dereferencePath' :: [String] -> [String] -> [String]
dereferencePath' processed [] = processed
dereferencePath' processed ["."] = "":processed
dereferencePath' (".":processed) ps@("..":_) = dereferencePath' processed ps
dereferencePath' processed ("..":ps) = dereferencePath' (tailSafe processed) (".":ps)
dereferencePath' processed (".":ps) = dereferencePath' processed ps
dereferencePath' processed (p:ps) = dereferencePath' (p:processed) ps

-- Parser

-- sepBy version thet returns full parsed string
sepByWSep p sep = sepByWSep1 p sep <|> return []

-- Character classes

isGenDelim = (`elem` ":/?#[]@")
isSubDelim = (`elem` "!$&'()*+,;=")
isReserved c = isGenDelim c || isSubDelim c
isUnreserved c = isAlphaNum c || c `elem` "-._~"
isPChar = satisfiesAny [isUnreserved, isSubDelim, (`elem` "%:@")]

satisfiesAny :: [a -> Bool] -> a -> Bool
satisfiesAny fs a = or (map ($ a) fs)

sepByWSep1 p sep = do
	first <- p
	rest <- many $ do
		sepV <- sep
		pV <- p
		return $ sepV ++ pV
	return $ concat (first : rest)

percentEncodedP = do
	string "%"
	d1 <- hexDigit
	d2 <- hexDigit
	return $ chr (read $ "0x" ++ [d1,d2]) -- What possibly can go wrong?

reservedP :: Stream s m Char => ParsecT s u m Char
reservedP = satisfy isReserved
unreservedP = satisfy isUnreserved
genDelimP :: Stream s m Char => ParsecT s u m Char
genDelimP = satisfy isGenDelim
subDelimP = satisfy isSubDelim
pCharP = satisfy isPChar

uriP = do
	schemeV <- optionMaybe $ try schemeP
	(authorityV, pathV) <- hierPartP
	let (userinfoV, hostV, portV) = fromMaybe (Nothing, Nothing, Nothing) authorityV
	queryV <- optionMaybe $ try $ do
		string "?"
		queryP
	fragmentV <- optionMaybe $ try $ do
		string "#"
		fragmentP
	return $ URI {
		uriScheme = schemeV
		, uriRegName = hostV
		, uriPort = portV
		, uriPath = pathV
		, uriUserInfo = userinfoV
		, uriQuery = queryV
		, uriFragment = fragmentV
		}

-- | scheme parser
schemeP = do
	l <- letter
	ls <- many (alphaNum <|> oneOf "+-.")
	string ":"
	return (l:ls)

hierPartP = do
	authorityV <- optionMaybe $ try $ do
		string "//"
		authorityP
	pathV <- pathP
	return (authorityV, pathV)

-- Path parser
pathP = (try pathRootlessP) <|> try pathAbsoluteP <|> try pathNoSchemeP <|> try pathABEmptyP <|> try pathEmptyP

pathABEmptyP = do
	segs <- many $ do
		string "/"
		segmentV <- segmentP
		return $ "/" ++ segmentV
	return (concat segs)

pathAbsoluteP = do
	string "/"
	rest <- option "" $ do
		s1 <- segmentNZP
		segs <- many $ do
			string "/"
			v <- segmentP
			return $ "/" ++ v
		return $ concat (s1 : segs)
	return $ "/" ++ rest

pathNoSchemeP = do
	first <- segmentNZNCP
	rest <- sepByWSep segmentP (string "/")
	return $ first ++ rest

pathRootlessP = do
	first <- segmentNZP
	rest <- sepByWSep segmentP (string "/")
	return $ first ++ rest

pathEmptyP = string ""

segmentP = many $ pCharP

segmentNZP = many1 $ pCharP

segmentNZNCP = many1 (subDelimP <|> unreservedP <|> oneOf "@%")

authorityP = do
	userinfoV <- optionMaybe (try $ do
		result <- userinfoP
		string "@"
		return result)
	hostV <- hostP
	portV <- optionMaybe (try $ do
		string ":"
		portP)
	return (userinfoV, Just hostV, portV)

hostP = ipLiteralP <|> try ipv4AddressP <|> regNameP

-- ip v6+ parser
ipLiteralP = do
	string "["
	result <- ipv6AddressP <|> ipvFutureP
	string "]"
	return result

-- Future IP parser
ipvFutureP = do
	v <- string "v"
	versionV <- many1 hexDigit
	dot <- string "."
	datV <- many1 (satisfy $ satisfiesAny [isUnreserved, isSubDelim, (==':')])
	return $ concat [v, versionV, dot, datV]

-- | Parse h16 followed by a colon, with no backtracking on failure.
h16Colon = do
	h <- h16
	c <- string ":"
	return (h ++ c)

-- | Process 0..n instances of the specified parser, backtracking on failure.
upTo n p = choice [try (count x p) | x <- [0..n]]

ipv6AddressP = try (do
		hs <- count 6 h16Colon
		s <- ls32
		return $ concat hs ++ s)
	<|> try (do
		co <- string "::"
		hs <- count 5 h16Colon
		s <- ls32
		return $ co ++ concat hs ++ s)
	<|> try (do
		p <- option "" h16
		co <- string "::"
		hs <- count 4 h16Colon
		s <- ls32
		return $ p ++ co ++ concat hs ++ s)
	<|> try (do
		ps <- upTo 1 h16Colon
		pp <- h16
		co <- string "::"
		hs <- count 3 h16Colon
		s <- ls32
		return $ concat ps ++ pp ++ co ++ concat hs ++ s)
	<|> try (do
		ps <- upTo 2 h16Colon
		pp <- h16
		co <- string "::"
		hs <- count 2 h16Colon
		s <- ls32
		return $ concat ps ++ pp ++ co ++ concat hs ++ s)
	<|> try (do
		ps <- upTo 3 h16Colon
		pp <- h16
		co <- string "::"
		h <- h16Colon
		s <- ls32
		return $ concat ps ++ pp ++ co ++ h ++ s)
	<|> try (do
		ps <- upTo 4 h16Colon
		pp <- h16
		co <- string "::"
		s <- ls32
		return $ concat ps ++ pp ++ co ++ s)
	<|> try (do
		ps <- upTo 5 h16Colon
		pp <- h16
		co <- string "::"
		h <- h16
		return $ concat ps ++ pp ++ co ++ h)
	<|> try (do
		ps <- upTo 6 h16Colon
		pp <- h16
		co <- string "::"
		return $ concat ps ++ pp ++ co)

h16 = count 4 hexDigit
ls32 = try (do
	h1 <- h16
	co <- string ":"
	h2 <- h16
	return $ h1 ++ co ++ h2)
	<|> ipv4AddressP

-- ipv4Address parser
ipv4AddressP = do
	d1 <- decOctetP
	string "."
	d2 <- decOctetP
	string "."
	d3 <- decOctetP
	string "."
	d4 <- decOctetP
	return $ concat [d1, ".", d2, ".", d3, ".", d4]

-- decimal octet
decOctetP = do
	a1 <- countMinMax 1 3 digit
	if read a1 > 255 then
		fail "Decimal octet value too large"
		else
		return a1

regNameP = many (unreservedP <|> subDelimP <|> oneOf "%")

-- helper
countMinMax m n p | m > 0 = do
	a1 <- p
	ar <- countMinMax (m-1) (n-1) p
	return (a1:ar)
countMinMax _ n _ | n <= 0 = return []
countMinMax _ n p = option [] $ do
	a1 <- p
	ar <- countMinMax 0 (n-1) p
	return (a1:ar)

-- port
portP = do
	digitV <- many digit
	return $ read digitV

-- userinfo
userinfoP = many $ satisfy $ satisfiesAny [isUnreserved, isSubDelim, (==':')]

queryP = many $ satisfy (isPChar) <|> oneOf "/?"

queryItemP = satisfy (isPChar) <|> oneOf "/?"

fragmentP = queryP

urlEncodedPairsP = many urlEncodedPairP

urlEncodedPairP = do
	keyV <- manyTill (percentEncodedP <|> plusP <|> queryItemP) (char '=')
	valueV <- manyTill (percentEncodedP <|> plusP <|> queryItemP) (skip (char '&') <|> eof)
	return (keyV, valueV)

plusP = do
	char '+'
	return ' '

skip a = do
	a
	return ()

explode :: (Eq a) => a -> [a] -> [[a]]
explode _ [] = []
explode delim xs = let (first, rest) = span (/= delim) xs
	in first : case rest of
		[] -> []
		x:[] -> [[]]
		x:xs -> explode delim xs
