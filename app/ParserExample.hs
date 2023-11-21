{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ParserExample where

import Data.Char (digitToInt)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 ( Parser, parseOnly, string, take, digit)
import Data.ByteString (ByteString)
import Data.Functor
import Prelude hiding (take)

data Version = One | Two
data Protocol' = Protocol' { version :: Version, length :: Int, body :: ByteString }

parseVersion :: Parser Version
parseVersion = (string "1" $> One) <|> (string "2" $> Two)

parseBody :: Parser (Int, ByteString)
parseBody = digit <&> digitToInt >>= \len -> take len <&> (len,)

-- HELP! can parseProtocol' be rewritten without the lambda function??
parseProtocol' :: Parser Protocol'
--parseProtocol' = (\v (l, b) -> Protocol' v l b) <$> parseVersion <*> parseBody
parseProtocol' = uncurry . Protocol' <$> parseVersion <*> parseBody


data Protocol2 = Protocol2 { l :: Int, b :: ByteString }

parseLength :: Parser Int
parseLength = digitToInt <$> digit

parseBody' :: Int -> Parser ByteString
parseBody' = take

parseProtocol2 :: Parser Protocol2
parseProtocol2 = Protocol2 <$> parseLength >>= \p -> p <$> parseBody' 3
