{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Format
import Handle

--import Network.Simple.TCP (HostPreference (..), recv, send, serve)
import Syntax
import System.Environment (getArgs)

import Control.Concurrent (ThreadId, forkIO, threadDelay)

--import Data.Function
--import Data.Functor
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
import Network.Socket.ByteString (recv, send, sendAll, sendAllTo)

did :: ByteString
did = "id: 0"

ddata :: ByteString
ddata = "data: hello world"

datum :: ByteString
datum = "data: hello world"

sser :: ByteString
--sser = "200 OK\r\ncontent-type: text/event-stream\r\n\r\n"
sser = "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nContent-Length: 0\r\n\r\n"

eachConn :: Env -> Socket -> SockAddr -> IO ()
eachConn env conn addr = do
    req <- recv conn 1024
    --print req
    res <- handle env req
    print res
    print "A"
    let isSse = B.empty == getHeader "text/event-stream" (headers' res)
    if isSse
        then do
            _ <- print (Format.pack res)
            _ <- send conn (Format.pack res)
            _ <- forkIO $ do
                _ <- threadDelay 1000
                _ <- sendAllTo conn (Format.pack sse) addr
                _ <- threadDelay 1000
                _ <- sendAllTo conn (Format.pack sse) addr
                _ <- threadDelay 1000
                _ <- sendAllTo conn (Format.pack sse) addr
                _ <- threadDelay 1000
                _ <- sendAllTo conn (Format.pack sse) addr
                _ <- threadDelay 1000
                _ <- sendAllTo conn (Format.pack sse) addr
                _ <- threadDelay 1000
                _ <- sendAllTo conn (Format.pack sse) addr
                _ <- threadDelay 1000
                _ <- sendAllTo conn (Format.pack sse) addr
                close conn
            return ()
        else do
            _ <- send conn (Format.pack res)
            close conn

--case runParser req of
--Right rast -> do
----res <- Handler.handle rast
--send conn "200" -- res
----_ <- forkIO $ handle conn
--return ()
--Left _ -> close conn

main :: IO ()
main = do
    argv <- getArgs

    BLC.putStrLn "Logs from your program will appear here"

    let port = 4221 :: PortNumber
        env = case argv of
            ["--directory", directory] -> Env $ pack directory
            _ -> Env B.empty

    BLC.putStrLn $ "Listening on port " <> BLC.pack (show port)

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port 0)
    listen sock 5
    forever $ do
        (conn, address) <- accept sock
        forkIO $ eachConn env conn address
