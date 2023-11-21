{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (parseReq, runParser) where

import Syntax

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 ( Parser, endOfLine, isSpace, many', parseOnly, skipSpace, space, string, take, takeTill)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt)
import Data.Function
import Data.Functor
import Prelude hiding (take)

parseMethod :: Parser Method
parseMethod =
    (string "GET" $> GET)
        <|> (string "POST" $> POST)

parsePath :: Parser Path
parsePath = Path <$> takeTill isSpace

parseProtocol :: Parser Protocol
parseProtocol =
    (string "HTTP/1.0" $> HTTP1_0)
        <|> (string "HTTP/1.1" $> HTTP1_1)
        <|> (string "HTTP/2.0" $> HTTP2_0)

parseLine :: Parser ByteString
parseLine = takeTill (== '\r') <* endOfLine

parseHeader :: Parser KeyVal
parseHeader = liftA2 (,) (takeTill (== ':') <* string ":" <* skipSpace) parseLine

parseHeaders :: Parser Map
parseHeaders = many' parseHeader

headersToLength :: Map -> Int
headersToLength hds =
    hds
        & getHeader "Content-Length"
        & readInt
        & \case
            Just (int, _) -> int
            Nothing -> 0

parseHeadersAndBody :: Parser (Map, Body)
parseHeadersAndBody = parseHeaders >>= \h -> endOfLine *> (take . headersToLength) h <&> \b -> (h, Body b)

parseReq :: Parser Req
parseReq =
    (\m pa pr (he, bo) -> Req m pa pr he bo) <$> parseMethod
        <* space
        <*> parsePath
        <* space
        <*> parseProtocol
        <* endOfLine
        <*> parseHeadersAndBody

runParser :: ByteString -> Either String Req
runParser = parseOnly parseReq
