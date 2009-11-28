module Text.URI (URI(..), nullURI, queryToPairs, parseURI) where

import Data.Char
import Data.Maybe
import Data.Word
import Codec.Binary.UTF8.String
import Safe
import Text.ParserCombinators.Parsec
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
	, uriUserInfo :: Maybe String -- ^ @anonymous\@
	, uriRegName :: Maybe String -- ^ @www.haskell.org@
	, uriPort :: Maybe Integer -- ^ @42@
	, uriPath :: String -- ^ @/ghc@
	, uriQuery :: Maybe String -- ^ @query@
	, uriFragment :: Maybe String -- ^ @frag@
	} deriving (Eq)

-- | Blank URI

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
		, if not (isReference u) then "//" else ""
		, maybe "" (++ "@") $ uriUserInfo u
		, maybe "" (id) $ uriRegName u
		, maybe "" (\p -> ":" ++ show p) $ uriPort u
		, uriPath u
		, maybe "" ("?" ++) $ uriQuery u
		, maybe "" ("#" ++) $ uriFragment u
		]

-- | Parser

-- Character classes

isGenDelim = (`elem` ":/?#[]@")
isSubDelim = (`elem` "!$&'()*+,;=")
isReserved c = isGenDelim c || isSubDelim c
isUnreserved c = isAlphaNum c || c `elem` "-._~"
isPChar = satisfiesAny [isUnreserved, isSubDelim, (`elem` ":@")]

satisfiesAny :: [a -> Bool] -> a -> Bool
satisfiesAny fs a = or (map ($ a) fs)

sepByWSep p sep = sepByWSep1 p sep <|> return []

sepByWSep1 p sep = do
	first <- p
	rest <- many $ do
		sepV <- sep
		pV <- p
		return $ sepV ++ pV
	return $ concat (first : rest)

-- Perfect, eliminates need to explicitly unescape
percentEncodedP = do
	string "%"
	d1 <- hexDigit
	d2 <- hexDigit
	return $ chr (read $ "0x" ++ [d1,d2]) -- What possibly can go wrong?

reservedP = satisfy isReserved
unreservedP = satisfy isUnreserved
genDelimP = satisfy isGenDelim
subDelimP = satisfy isSubDelim
pCharP = (percentEncodedP <|> plusP <|> satisfy isPChar)

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
	ls <- many1 (alphaNum <|> oneOf "+-.")
	string ":"
	return (l:ls)

hierPartP = do
	authorityV <- optionMaybe $ try $ do
		string "//"
		authorityP
	pathV <- pathP
	return (authorityV, pathV)

-- Path parser
pathP = pathRootlessP <|> pathAbsoluteP <|> pathNoSchemeP <|> pathABEmptyP <|> pathEmptyP

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

segmentNZNCP = many1 (percentEncodedP <|> subDelimP <|> unreservedP <|> char '@')

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

hostP = ipLiteralP <|> ipv4AddressP <|> regNameP

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

-- ipv6 parser
ipv6AddressP = do
	-- To hell with complicated rules
	many1 $ oneOf "ABCDEF1234567890:"

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

regNameP = many (percentEncodedP <|> unreservedP <|> subDelimP)

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

queryP = many $ pCharP <|> oneOf "/?"

fragmentP = queryP

queryToPairs q = either (const []) (id) $ parse urlEncodedPairsP "query" q

urlEncodedPairsP = many urlEncodedPairP

urlEncodedPairP = do
	keyV <- manyTill (percentEncodedP <|> plusP <|> unreservedP) (char '=')
	valueV <- manyTill (percentEncodedP <|> plusP <|> unreservedP) (skip (char '&') <|> eof)
	return (keyV, valueV)

plusP = do
	char '+'
	return ' '

skip a = do
	a
	return ()

parseURI :: String -> Maybe URI
parseURI s = either (const Nothing) (Just) $ parse uriP "user input" s

escapeChar :: (Char -> Bool) -> Char -> String
escapeChar f c = if f c then [c] else concat $ map (printf "%%%0.2X") (encode [c])

escapeString :: (Char -> Bool) -> String -> String
escapeString f s = concat $ map (escapeChar f) s

isReference :: URI -> Bool
isReference u = maybe (True) (const False) $ uriRegName u

isRelative :: URI -> Bool
isRelative u = headDef ' ' (uriPath u) /= '/'