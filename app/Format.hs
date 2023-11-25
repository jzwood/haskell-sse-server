{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Format where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Syntax

class Pack a where
    pack :: a -> ByteString

instance Pack Protocol where
    pack HTTP1_0 = "HTTP/1.0"
    pack HTTP1_1 = "HTTP/1.1"
    pack HTTP2_0 = "HTTP/2.0"

instance Pack Status where
    pack (Status 200) = "200 OK"
    pack (Status 201) = "201 CREATED"
    pack (Status _) = "404 NOT FOUND"

instance Pack Resp where
    pack Resp{protocol', status, headers', body'} =
        pack protocol'
            `B.append` " "
            `B.append` pack status
            `B.append` "\r\n"
            `B.append` B.intercalate "\r\n" ((\(k, v) -> B.concat [k, ": ", v]) <$> headers')
            `B.append` "\r\n\r\n"
            `B.append` body'
