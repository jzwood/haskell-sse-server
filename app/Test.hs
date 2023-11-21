{-# LANGUAGE OverloadedStrings #-}

module Test (xxx) where

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString (ByteString)
import Parser
import Syntax

req :: ByteString
req = "GET /user-agent HTTP/1.1\r\n\r\nHost: localhost:4221\r\nUser-Agent: curl/7.64.1\r\n"

req2 :: ByteString
req2 = "GET /echo/hello HTTP/1.1\r\nHost: 127.0.0.1:4221\r\nUser-Agent: curl/7.88.1\r\nAccept: */*\r\n\r\n"

xxx :: Either String Req
xxx = parseOnly parseReq req2
