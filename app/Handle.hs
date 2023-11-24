{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle (handle, sse) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Functor
import System.Directory (doesFileExist)

--import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, digit, endOfLine, isSpace, parseOnly, skipSpace, space, string, take, takeTill)
import Data.Attoparsec.ByteString.Char8 (Parser, endOfInput, parseOnly, string, takeByteString)
import qualified Data.ByteString as B
import Format
import Parser
import Syntax

toHeader :: ByteString -> ByteString -> (ByteString, ByteString)
toHeader = (,)

ok :: ByteString -> Integer -> ByteString -> Resp
ok contentType status body'  =
    Resp
        { protocol' = HTTP1_1
        , status = Status status
        , headers' =
            [ toHeader "Content-Type" contentType
            , toHeader "Content-Length" ((pack . show . B.length) body')
            ]
        , body'
        }

sse :: Resp
sse = ok "text/event-stream" 200 "data: chat"

txt :: ByteString -> Resp
txt = ok "text/plain" 200

file :: ByteString -> Resp
file = ok "application/octet-stream" 200

write :: Resp
write = ok "text/plain" 201 B.empty

html :: ByteString -> Resp
html = ok "text/html" 200

notFound :: Resp
notFound =
    Resp
        { protocol' = HTTP1_1
        , status = Status 404
        , headers' = []
        , body' = ""
        }

parseRoute :: Parser [ByteString]
parseRoute =
    (string "/" *> endOfInput $> ["/"])
        <|> (string "/user-agent" *> endOfInput $> ["user-agent"])
        <|> (string "/files/" *> takeByteString <&> \path -> ["files", path])
        <|> (string "/html/" *> takeByteString <&> \path -> ["html", path])
        <|> (string "/sse" $> ["sse"])
        <|> (string "/echo/" *> takeByteString <&> \echo -> ["echo", echo])

routeToResp :: Env -> Method -> Req -> [ByteString] -> IO Resp
routeToResp _ GET _ ["/"] = pure $ txt ""
routeToResp _ GET _ ["echo", echo] = pure $ txt echo
routeToResp _ GET _ ["sse"] = pure sse
routeToResp _ GET Req { headers } ["user-agent"] = pure $ txt (getHeader "User-Agent" headers)
routeToResp Env { dir } GET _ ["html", bsPath] =
    B.toFilePath (dir <> "/" <> bsPath)
        >>= doesFileExist
        >>= \exists ->
            if exists
                then B.toFilePath (dir <> "/" <> bsPath) >>= B.readFile <&> html
                else pure notFound
routeToResp Env { dir } GET _ ["files", bsPath] =
    B.toFilePath (dir <> "/" <> bsPath)
        >>= doesFileExist
        >>= \exists ->
            if exists
                then B.toFilePath (dir <> "/" <> bsPath) >>= B.readFile <&> file
                else pure notFound
routeToResp Env { dir } POST Req { body = Body body } ["files", bsPath] = do
    path <- B.toFilePath (dir <> "/" <> bsPath)
    B.writeFile path body
    pure write
routeToResp _ _ _ _ = pure notFound

handle' :: Env -> Req -> IO Resp
handle' env req@Req{path = (Path path), method } =
    case parseOnly parseRoute path of
        Right bs -> routeToResp env method req bs
        Left _ -> pure notFound

handle :: Env -> ByteString -> IO Resp
handle env bsReq =
    case runParser bsReq of
        Right req -> handle' env req
        Left _ -> pure notFound
