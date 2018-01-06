module Multibase ( Base(..),
                  appendPrefix,
                  removePrefix,
                  getPrefix,
                  multiEncode,
                  multiDecode
                  ) where

import qualified Data.Word as W
import qualified Data.ByteString as BStr
import qualified Data.Map as M
import MultibaseEncode

data Base
  = BaseID
  | Base2
  | Base8
  | Base10
  | Base16
  | Base32
  | Base32z
  | Base32hex
  | Base58btc
  | Base58flickr
  | Base64
  | Base64URL
  deriving (Eq, Ord, Show)

appendPrefix ::  Base -> BStr.ByteString -> BStr.ByteString
appendPrefix a b =  BStr.append (BStr.pack $ snd $  fst $ (multibases M.! a)) b

removePrefix :: BStr.ByteString -> BStr.ByteString
removePrefix a = BStr.pack $ tail (BStr.unpack a)

getPrefix :: BStr.ByteString -> [W.Word8]
getPrefix a = [head $ BStr.unpack a]

multiEncode :: Base -> BStr.ByteString -> Bool -> BStr.ByteString
multiEncode a b c =  appendPrefix a (encodeByteString b ( snd (multibases M.! a)) (fst $ fst (multibases M.! a)) c)

multiDecode :: BStr.ByteString -> BStr.ByteString
multiDecode a = decodeByteString (removePrefix a) (snd (multibases M.! (baseNumMap M.! (getPrefix a)))) (fst $ fst (multibases M.! (baseNumMap M.! (getPrefix a))))  0

multibases :: M.Map Base ((BStr.ByteString , [W.Word8]), Integer)
multibases = M.fromList [ (BaseID , (( word8Range ,   [0] ), 256 ))
                        , (Base2,(( base2   , [48]) , 2))
                        , (Base8,(( base8   , [55]) , 8))
                        , (Base10,(( base10   , [57]) , 10))
                        , (Base16,(( base16  , [102]) , 16))
                        , (Base32,(( base32  , [98]) , 32))
                        , (Base32z,(( base32z  , [104]) , 32))
                        , (Base32hex,(( base32hex  , [118]) , 32 ))
                        , (Base58btc,(( base58btc  , [122]) , 58))
                        , (Base58flickr,(( base58flickr  , [91]) , 58 ))
                        , (Base64,(( base64  , [109]) , 64))
                        , (Base64URL , (( base64URL  , [117]) , 64))
                        ]

baseNumMap = M.fromList [([0] , BaseID)
                        , ([48] , Base2)
                        , ([55] , Base8)
                        , ([57] , Base10)
                        ,( [102] , Base16)
                        , ([98] , Base32 )
                        , ([104] , Base32z)
                        ,( [118] , Base32hex)
                        ,( [122] , Base58btc )
                        ,( [91] , Base58flickr)
                        , ([109] , Base64)
                        ,( [117] , Base64URL)
                        ]
