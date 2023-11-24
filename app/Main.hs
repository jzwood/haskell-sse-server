{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import Format
import Handle
import Network.Simple.TCP (HostPreference (..), recv, send, serve)
import Syntax
import System.Environment (getArgs)

did :: ByteString
did = "id: 0"

ddata :: ByteString
ddata = "data: hello world"

datum :: ByteString
datum = "data: hello world"

sser :: ByteString
sser = "200 OK\r\ncontent-type: text/event-stream\r\n\r\n"

main :: IO ()
main = do
    argv <- getArgs

    BLC.putStrLn "Logs from your program will appear here"

    let host = "127.0.0.1"
        port = "4221"
        env = case argv of
            ["--directory", directory] -> Env $ pack directory
            _ -> Env B.empty

    BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port
    serve (Host host) port $ \(serverSocket, serverAddr) ->
        do
            BLC.putStrLn $ "Accepted connection from " <> BLC.pack (show serverAddr) <> "."
            bsReq <- fromMaybe B.empty <$> recv serverSocket 1024
            bsRes <- handle env bsReq
            --print bsReq
            --print bsRes
            let isSse = B.empty == getHeader "text/event-stream" (headers' bsRes)
            if isSse
                then do
                    print "SSE START\n\n"
                    send serverSocket (toBs bsRes)
                    print (toBs bsRes)
                    forever $ do
                        --forkIO $ handle conn store
                        print "SSE\n\n"
                        send serverSocket did
                        send serverSocket ddata
                else send serverSocket (toBs bsRes)
