import Text.ParserCombinators.Parsec

-- TODO/NOTE because the present state of affairs, of every single http
-- client library that I've run across, is to stupidly couple
-- response parsing to request processing- I've ripped a solution to
-- parse responses from the salvia-protocol code-base.

pResponse :: GenParser Char st (Http Response)
pResponse =
  (\v s h -> Http (Response (read s)) v h)
  <$> (pVersion <* many1 (oneOf ls))
  <*> (many1 digit <* many1 (oneOf ls) <* many1 (noneOf lf) <* eol)
  <*> (pHeaders <* eol)

-- pResponse :: GenParser Char st (Http Response)
-- pResponse =
--   (\v s h -> Http (Response (read s)) v h)
--   <$> (pVersion <* many1 (oneOf ls))
--   <*> (many1 digit <* many1 (oneOf ls) <* many1 (noneOf lf) <* eol)
--   <*> (pHeaders <* eol)
  -- | Parsec parser to parse one or more, possibly multiline, HTTP header lines.

pHeaders :: GenParser Char st Headers
pHeaders = Headers <$> p
  where
    p = (\k v -> ((k, v):))
        <$> many1 (noneOf (':':ws)) <* string ":"
        <*> (intercalate ws <$> (many $ many1 (oneOf ls) *> many1 (noneOf lf) <* eol))
        <*> option [] p

pVersion :: GenParser Char st Version
pVersion = do
    (\h l -> Version (ord h - ord '0') (ord l  - ord '0'))
    <$> (istring "HTTP/" *> digit)
    <*> (char '.'       *> digit)

parseResponse :: String -> Either String (Http Response)
parseResponse = either (Left . show) (Right . id) . parse pResponse ""
