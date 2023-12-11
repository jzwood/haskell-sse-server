{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Handle (handle, notFound) where

import Control.Applicative()
import Control.Concurrent(ThreadId, forkIO, threadDelay)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Functor()
import Network.Socket (Socket, close)
import System.Directory (doesFileExist)
import Network.Socket.ByteString (send)
import qualified Format
import Syntax

toHeader :: ByteString -> ByteString -> (ByteString, ByteString)
toHeader = (,)

ok :: ByteString -> Integer -> ByteString -> ByteString
ok contentType status body' =
    Format.pack $
        Resp
            { protocol' = HTTP1_1
            , status = Status status
            , headers' =
                [ toHeader "Content-Type" contentType
                , toHeader "Content-Length" ((pack . show . B.length) body')
                ]
            , body'
            }

sse :: ByteString
sse =
    Format.pack $
        Resp
            { protocol' = HTTP1_1
            , status = Status 200
            , headers' =
                [ ("Content-Type", "text/event-stream")
                , ("Cache-Control", "no-cache")
                , ("Connection", "keep-alive")
                ]
            , body' = ""
            }

txt :: ByteString -> ByteString
txt = ok "text/plain" 200

file :: ByteString -> ByteString
file = ok "application/octet-stream" 200

html :: ByteString -> ByteString
html = ok "text/html" 200

notFound :: ByteString
notFound =
    Format.pack $
        Resp
            { protocol' = HTTP1_1
            , status = Status 404
            , headers' = []
            , body' = ""
            }

handle :: Env -> Socket -> Req -> IO ()
handle _ conn Req{method = GET, route = Whack} = send conn (txt "") >> close conn
handle _ conn Req{method = GET, route = Echo msg} = send conn (txt msg) >> close conn
handle _ conn Req{method = GET, route = Agent, headers} = send conn (txt $ getHeader "User-Agent" headers) >> close conn
handle _ conn Req{method = GET, route = Sse} = do
    _ <- send conn sse
    _ <- forever $ do
        _ <- threadDelay 1_000_000
        _ <- send conn "event: score\r\n"
        _ <- send conn "data: {\"cat\": 123}\r\n"
        _ <- send conn "data: {\"man\": 456}\r\n"
        _ <- send conn "\r\n"
        _ <- threadDelay 10_000_000
        return ()
    return ()
--close conn

handle Env{dir} conn Req{method = GET, route = Html bpath} = do
    fpath <- B.toFilePath (dir <> "/" <> bpath)
    isFile <- doesFileExist fpath
    if isFile
        then B.readFile fpath >>= send conn . html >> close conn
        else send conn notFound >> close conn
handle Env{dir} conn Req{method = GET, route = File bpath} = do
    fpath <- B.toFilePath (dir <> "/" <> bpath)
    isFile <- doesFileExist fpath
    if isFile
        then B.readFile fpath >>= send conn . file >> close conn
        else send conn notFound >> close conn
handle _ conn _ = send conn notFound >> close conn
