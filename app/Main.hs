{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Handle
import Parser (parseOnly, parseReq)
import Syntax
import System.Environment (getArgs)

import Control.Concurrent (forkIO)

import Network.Socket (
    Family (..),
    PortNumber (..),
    SockAddr (..),
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
import Network.Socket.ByteString (recv, send)

main :: IO ()
main = do
    argv <- getArgs

    BLC.putStrLn "Logs from your program will appear here"

    let host = 1024
        port = 4221 :: PortNumber
        -- TODO have env include host and port
        env = case argv of
            ["--directory", directory] -> Env $ pack directory
            _ -> Env B.empty

    BLC.putStrLn $ "Listening on port " <> BLC.pack (show port)

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port 0)
    listen sock 5
    forever $ do
        (conn, _address) <- accept sock
        bReq <- recv conn host
        --print bReq
        forkIO $ case parseOnly parseReq bReq of
            Left _ -> send conn notFound >> close conn
            Right req -> handle env conn req
