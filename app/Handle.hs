{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle (handle, notFound) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Functor
import Network.Socket (
    Family (..),
    PortNumber (..),
    SockAddr (..),
    Socket,
    SocketOption (..),
    SocketType (..),
    accept,
    bind,
    close,
    defaultProtocol,
    listen,
    setSocketOption,
    socket,
 )
import System.Directory (doesFileExist)

--import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, digit, endOfLine, isSpace, parseOnly, skipSpace, space, string, take, takeTill)
import Data.Attoparsec.ByteString.Char8 (Parser, endOfInput, parseOnly, string, takeByteString)
import Network.Socket.ByteString (recv, send, sendAll, sendAllTo, sendTo)

--import qualified Data.ByteString as B
import qualified Format
import Parser
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
sse = ok "text/event-stream" 200 "data: hi"

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

--Left _ -> send conn (Format.pack notFound) >> close conn
--res <- handle env req
--let isSse = B.empty == getHeader "text/event-stream" (headers' res)
--if isSse
--then do
--_ <- print "A"
--_ <- print (Format.pack res)
--_ <- sendTo conn (Format.pack res) addr
--_ <- sendTo conn hi addr
--return ()
--else --close conn
--do
--_ <- print "B"
--_ <- send conn (Format.pack res)
--close conn
