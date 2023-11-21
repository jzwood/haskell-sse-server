{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Format where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Syntax

class ToBs a where
    toBs :: a -> ByteString

instance ToBs Protocol where
    toBs HTTP1_0 = "HTTP/1.0"
    toBs HTTP1_1 = "HTTP/1.1"
    toBs HTTP2_0 = "HTTP/2.0"

instance ToBs Status where
    toBs (Status 200) = "200 OK"
    toBs (Status 201) = "201 CREATED"
    toBs (Status _) = "404 NOT FOUND"

instance ToBs Resp where
    toBs Resp{protocol', status, headers', body'} =
        toBs protocol'
            `B.append` " "
            `B.append` toBs status
            `B.append` "\r\n"
            `B.append` B.intercalate "\r\n" ((\(k, v) -> B.concat [k, ": ", v]) <$> headers')
            `B.append` "\r\n\r\n"
            `B.append` body'
