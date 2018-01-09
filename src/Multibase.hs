{-# LANGUAGE OverloadedStrings #-}
module Multibase
  ( Base(..)
  , MultibaseError(..)
  , addPrefix
  , removePrefix
  , getPrefix
  , encode
  , decode
  , identity
  , base2
  , base8
  , base16
  , base16'
  , base32
  , base32'
  , base32pad
  , base32pad'
  , base32z
  , base32hex
  , base32hex'
  , base32hexpad
  , base32hexpad'
  , base58btc
  , base58flickr
  , base64
  , base64pad
  , base64url
  , base64urlpad
  ) where

import Data.Char
import qualified Data.Word as W
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BStr
import qualified Data.Map as M
import MultibaseEncode

data Base = Base
  { basePrefix :: Char
  , baseEncode :: Encoder
  , baseDecode :: Decoder
  }

addPrefix :: Base -> ByteString -> ByteString
addPrefix base bytes = BStr.cons (basePrefix base) bytes

removePrefix :: ByteString -> Maybe ByteString
removePrefix "" = Nothing
removePrefix bytes = Just $ BStr.tail bytes

getPrefix :: ByteString -> Maybe Char
getPrefix "" = Nothing
getPrefix bytes = Just $ BStr.head bytes

encode :: Base -> ByteString -> ByteString
encode base bytes = addPrefix base $ baseEncode base bytes

decode :: ByteString -> Result ByteString
decode bytes =
  decode' prefix encoded
  where
    prefix = getPrefix bytes
    encoded = removePrefix bytes

decode' :: Maybe Char -> Maybe ByteString -> Result ByteString
decode' (Just prefix) (Just encoded) = do
  case M.lookup prefix prefixBases of
    Nothing -> Left UnknownBase
    Just base -> baseDecode base encoded
decode' _ _ = Left InputTooShort

identity, base2, base8, base16, base16' :: Base
base32, base32', base32pad, base32pad' :: Base
base32hex, base32hex', base32hexpad, base32hexpad' :: Base
base32z, base58btc, base58flickr :: Base
base64, base64pad, base64url, base64urlpad :: Base

-- | The identity base, does not change contents
identity = Base '\000'
  id
  Right

-- | Binary
base2 = Base '0'
  encodeBase2
  decodeBase2

-- | Octal
base8 = Base '7'
  encodeBase8
  decodeBase8

-- | Base 16 (lowercase)
base16 = Base 'f'
  encodeBase16
  decodeBase16

-- | Base 16 (uppercase)
base16' = Base 'F'
  (upperCase . encodeBase16)
  decodeBase16

-- | Base 32 (lowercase, unpadded)
base32 = Base 'b'
  encodeBase32LowercaseNoPad
  decodeBase32

-- | Base 32 (uppercase, unpadded)
base32' = Base 'B'
  encodeBase32NoPad
  decodeBase32

-- | Base 32 (lowercase, padded)
base32pad = Base 'c'
  encodeBase32Lowercase
  decodeBase32

-- | Base 32 (uppercase, padded)
base32pad' = Base 'C'
  encodeBase32
  decodeBase32

-- | Base 32hex (lowercase, unpadded)
base32hex = Base 'v'
  encodeBase32hexLowercaseNoPad
  decodeBase32hex

-- | Base 32hex (uppercase, unpadded)
base32hex' = Base 'V'
  encodeBase32hexNoPad
  decodeBase32hex

-- | Base 32hex (lowercase, padded)
base32hexpad = Base 't'
  encodeBase32hexLowercase
  decodeBase32hex

-- | Base 32hex (uppercase, padded)
base32hexpad' = Base 'T'
  encodeBase32hex
  decodeBase32hex

-- | z-base-32
base32z = Base 'h'
  encodeBase32z
  decodeBase32z

-- | Base 58 (Bitcoin encoding)
base58btc = Base 'z'
  encodeBase58btc
  decodeBase58btc

-- | Base 58 (Flickr encoding)
base58flickr = Base 'Z'
  encodeBase58flickr
  decodeBase58flickr

-- | Base 64 (unpadded)
base64 = Base 'm'
  (unpad . encodeBase64)
  (decodeBase64 . repad 4)

-- | Base 64 (padded)
base64pad = Base 'M'
  encodeBase64
  decodeBase64

-- | Base 64url (unpadded)
base64url = Base 'u'
  (unpad . encodeBase64URL)
  (decodeBase64URL . repad 4)

-- | Base 64url (padded)
base64urlpad = Base 'U'
  encodeBase64URL
  decodeBase64URL

bases =
  [ identity
  , base2
  , base8
  , base16
  , base16'
  , base32
  , base32'
  , base32pad
  , base32pad'
  , base32z
  , base32hex
  , base32hex'
  , base32hexpad
  , base32hexpad'
  , base58btc
  , base58flickr
  , base64
  , base64pad
  , base64url
  , base64urlpad
  ]

getBasePrefix :: Base -> (Char, Base)
getBasePrefix base = (basePrefix base, base)

prefixBases = M.fromList $ map getBasePrefix bases
