-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Efficient encoding and decoding of base32 encoded bytestring
--   according to RFC 4648. <http://tools.ietf.org/html/rfc4648>
--
--   This module recommended to be imported as
--   @import Data.ByteString.Base32 as Base32@ to avoid name clashes
--   with @Data.Binary@ or @Data.ByteString.Base64@ modules.
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Base32
       ( Base32
       , encode
       , encodeNoPad
       , encodeLowercase
       , encodeLowercaseNoPad
       , decode
       , decodeLenient
       ) where

import Data.ByteString as BS
import Data.ByteString.Base32.Internal
import Data.List as L


-- | Base32 encoded bytestring.
type Base32 = ByteString

encW5 :: Word5 -> Word8
encW5 !x
  |  x <= 25  = 65 + x
  | otherwise = 24 + x
{-# INLINE encW5 #-}

encTable :: EncTable
encTable = BS.pack $ L.map encW5 [0..31]

-- | Encode an arbitrary bytestring into (upper case) base32 form.
encode :: ByteString -> Base32
encode = unpack5 True encTable

encodeNoPad :: ByteString -> Base32
encodeNoPad = unpack5 False encTable

encW5L :: Word5 -> Word8
encW5L !x
  | x <= 25   = 97 + x
  | otherwise = 24 + x

encTableLowercase :: EncTable
encTableLowercase = BS.pack $ L.map encW5L [0..31]

-- | Encode an arbitrary bytestring into (lower case) base32 form.
encodeLowercase :: ByteString -> Base32
encodeLowercase = unpack5 True encTableLowercase

encodeLowercaseNoPad :: ByteString -> Base32
encodeLowercaseNoPad = unpack5 False encTableLowercase

decW5 :: Word8 -> Word5
decW5 !x
  | x <  50  {- c2w '2' -} = invIx
  | x <= 55  {- c2w '7' -} = x - 24 {- c2w '2' - 26 -}
  | x <  65  {- c2w 'A' -} = invIx
  | x <= 90  {- c2w 'Z' -} = x - 65 {- c2w 'A' -}
  | x <  97  {- c2w 'a' -} = invIx
  | x <= 122 {- c2w 'z' -} = x - 97 {- c2w 'a' -}
  | otherwise = invIx
{-# INLINE decW5 #-}

decTable :: ByteString
decTable = BS.pack $ L.map decW5 [minBound .. maxBound]

-- | Decode a base32 encoded bytestring. This function is
-- case-insensitive and do not require correct padding.
decode :: Base32 -> Either String ByteString
decode = pack5 decTable

-- | The same as 'decode' but with additional leniency: decodeLenient
-- will skip non-alphabet characters.
decodeLenient :: Base32 -> Either String ByteString
decodeLenient = pack5Lenient decTable
