{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (parseReq, parseOnly) where

import Syntax

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, endOfInput, endOfLine, many', parseOnly, skipSpace, space, string, take, takeByteString, takeTill)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt)
import Data.Function
import Data.Functor
import Prelude hiding (take)

parseMethod :: Parser Method
parseMethod =
    (string "GET" $> GET)
        <|> (string "POST" $> POST)

parseRoute :: Parser Route
parseRoute = (string "/user-agent" *> endOfInput $> Agent)
        <|> (string "/sse" $> Sse)
        <|> (string "/files/" *> takeByteString <&> File)
        <|> (string "/html/" *> takeByteString <&> Html)
        <|> (string "/echo/" *> takeByteString <&> Echo)
        <|> (string "/" *> endOfInput $> Whack)

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
        <*> parseRoute
        <* space
        <*> parseProtocol
        <* endOfLine
        <*> parseHeadersAndBody
