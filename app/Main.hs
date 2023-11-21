{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Handle
import Syntax
import Network.Simple.TCP (HostPreference (..), recv, send, serve)
import System.Environment (getArgs)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC

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
            mReq <- recv serverSocket 1024
            bsRes <- handle env $ fromMaybe B.empty mReq
            --print mReq
            --print bsRes
            send serverSocket bsRes
