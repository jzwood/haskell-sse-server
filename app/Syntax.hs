module Syntax where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Function
import Data.Maybe (fromMaybe)
import Data.List (lookup)

type KeyVal = (ByteString, ByteString)
type Map = [KeyVal]

getHeader :: ByteString -> Map -> ByteString
getHeader key hs = lookup key hs & fromMaybe B.empty

newtype Env = Env { dir :: ByteString }

newtype Path = Path ByteString
    deriving (Eq, Show)

newtype Body = Body ByteString
    deriving (Eq, Show)

newtype Status = Status Integer
    deriving (Eq, Show)

data Method = GET | POST
    deriving (Eq, Show)

data Protocol = HTTP1_0 | HTTP1_1 | HTTP2_0
    deriving (Eq, Show)

data Req = Req
    { method :: Method
    , path :: Path
    , protocol :: Protocol
    , headers :: Map
    , body :: Body
    }
    deriving (Eq, Show)

data Resp = Resp
    { protocol' :: Protocol
    , status :: Status
    , headers' :: Map
    , body' :: ByteString
    }
    deriving (Eq, Show)
