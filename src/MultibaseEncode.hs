{-# LANGUAGE OverloadedStrings #-}
module MultibaseEncode where

import Data.Bits ((.|.), (.&.))
import qualified Data.Bits       as Bin
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Word       as W

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base32 as Base32
import qualified Data.ByteString.Base32.Hex as Base32H
import qualified Data.ByteString.Base32.Z as Base32Z
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64U

type Encoder = ByteString -> ByteString
type Decoder = ByteString -> Result ByteString
type Result a = Either MultibaseError a

data MultibaseError
  = UnknownBase
  | InvalidBaseString
  | InputTooShort
  deriving (Eq, Show)

unMaybe :: Maybe ByteString -> Either MultibaseError ByteString
unMaybe Nothing = Left InvalidBaseString
unMaybe (Just b) = Right b

eitherFix :: Either a ByteString -> Either MultibaseError ByteString
eitherFix (Right b) = Right b
eitherFix (Left _) = Left InvalidBaseString

-- Base 16

encodeBase16 :: Encoder
encodeBase16 = Base16.encode

decodeBase16 :: Decoder
decodeBase16 bytes
  | invalid == "" = Right valid
  | otherwise = Left InvalidBaseString
  where
    (valid, invalid) = Base16.decode bytes

-- Base 32

encodeBase32 :: Encoder
encodeBase32 = Base32.encode

encodeBase32NoPad :: Encoder
encodeBase32NoPad = Base32.encodeNoPad

encodeBase32Lowercase :: Encoder
encodeBase32Lowercase = Base32.encodeLowercase

encodeBase32LowercaseNoPad :: Encoder
encodeBase32LowercaseNoPad = Base32.encodeLowercaseNoPad

decodeBase32 :: Decoder
decodeBase32 = eitherFix . Base32.decode

encodeBase32hex :: Encoder
encodeBase32hex = Base32H.encode

encodeBase32hexNoPad :: Encoder
encodeBase32hexNoPad = Base32H.encodeNoPad

encodeBase32hexLowercase :: Encoder
encodeBase32hexLowercase = Base32H.encodeLowercase

encodeBase32hexLowercaseNoPad :: Encoder
encodeBase32hexLowercaseNoPad = Base32H.encodeLowercaseNoPad

decodeBase32hex :: Decoder
decodeBase32hex = eitherFix . Base32H.decode

encodeBase32z :: Encoder
encodeBase32z = Base32Z.encode

decodeBase32z :: Decoder
decodeBase32z = eitherFix . Base32Z.decode

-- Base 58

encodeBase58btc :: Encoder
encodeBase58btc = Base58.encodeBase58 Base58.bitcoinAlphabet

decodeBase58btc :: Decoder
decodeBase58btc = unMaybe . Base58.decodeBase58 Base58.bitcoinAlphabet

encodeBase58flickr :: Encoder
encodeBase58flickr = Base58.encodeBase58 Base58.flickrAlphabet

decodeBase58flickr :: Decoder
decodeBase58flickr = unMaybe . Base58.decodeBase58 Base58.flickrAlphabet

-- Base 64

encodeBase64 :: Encoder
encodeBase64 = Base64.encode

decodeBase64 :: Decoder
decodeBase64 = eitherFix . Base64.decode

encodeBase64URL :: Encoder
encodeBase64URL = Base64U.encode

decodeBase64URL :: Decoder
decodeBase64URL = eitherFix . Base64U.decode

--

-- decodeByteString :: ByteString -> Integer -> ByteString -> Either MultibaseError ByteString
-- decodeByteString bytes prevBase prevByteset =
--   Right $ decodeByteString' bytes prevBase prevByteset 0

-- decodeByteString' :: ByteString -> Integer -> ByteString -> Int ->  ByteString
-- decodeByteString' byteString prevBase prevByteset recurrCount
--   | Map.member prevBase bNumToBlO &&
--     mod (length (filter (/= 61) (BStr.unpack byteString))) (bNumToBlO Map.! prevBase) /= 0
--   = decodeByteString' ( BStr.pack ((filter (/= 61) (BStr.unpack byteString))
--                                   ++ [lookupTable prevBase prevByteset Map.! 0] ))
--     prevBase prevByteset (recurrCount + 1)
--   | otherwise
--   = BStr.pack $ reverse $ drop recurrCount (reverse $ encode (map (invertedTable prevBase prevByteset Map.!) (BStr.unpack byteString) ) 256 word8Range prevBase )


-- encodeByteString :: BStr.ByteString -> Integer -> BStr.ByteString -> Bool -> BStr.ByteString
-- encodeByteString byteString base byteset padFlag
--   | Map.member base bNumToBl &&
--     mod (BStr.length byteString) (bNumToBl Map.! base) /= 0
--   = BStr.pack $  padCatcher ( BStr.unpack byteString) base byteset padFlag (padHelper (BStr.unpack byteString) base)

--   | otherwise = BStr.pack $ encode (BStr.unpack byteString) base byteset 256



-- padHelper w8List base  =  (bNumToBl Map.! base) - (mod (length w8List) (bNumToBl Map.! base))

unpad :: ByteString -> ByteString
unpad = C.takeWhile (/= '=')

dropNulls :: ByteString -> ByteString
dropNulls = C.dropWhile (== '\000')

repad :: Int -> ByteString -> ByteString
repad mult bytes
  | mislen == 0 = bytes
  | otherwise   = BStr.append bytes (C.replicate padlen '=')
  where
    padlen = mult - mislen
    mislen = BStr.length bytes `mod` mult

upperCase :: ByteString -> ByteString
upperCase = C.map toUpper

lowerCase :: ByteString -> ByteString
lowerCase = C.map toLower


-- padCatcher :: [W.Word8] -> Integer -> BStr.ByteString -> Bool -> Int -> [W.Word8]
-- padCatcher w8List base byteSet padFlag pHn
--   | padFlag
--   = reverse ( (++) (replicate pHn (61 :: W.Word8) ) (drop  pHn (reverse (encodeWithPadding w8List base byteSet pHn))))

--   | otherwise
--   = reverse (drop   pHn (reverse (encodeWithPadding w8List base byteSet pHn)))


-- encodeWithPadding :: [W.Word8] -> Integer -> BStr.ByteString -> Int  -> [W.Word8]
-- encodeWithPadding  w8List base byteSet pHn =
--   encode (w8List ++ replicate pHn (0 :: W.Word8) ) base byteSet 256



-- encode :: [W.Word8] -> Integer -> BStr.ByteString -> Integer -> [W.Word8]
-- encode byteString base byteSet prevBase  =
--   map (lookupTable base byteSet Map.!)
--   $ integerListToWord8List $ convertInteger base ( convertIntegerList ( word8ListToIntegerList byteString) prevBase)



-- Integer base conversion ---------------------------------------------------------------------------------------------------------------------------------------------------

-- convertInteger :: Integer -> Integer -> [Integer]
-- convertInteger base int = convertInteger' base int []
--   where
--     convertInteger' _    0 acc = acc
--     convertInteger' base i acc =
--       let (q, r) = i `quotRem` base
--       in convertInteger' base q (r : acc)

-- convertIntegerList :: [Integer] -> Integer -> Integer
-- convertIntegerList ints base = List.foldl' (\acc x -> acc * base + x) 0 ints


toBase2 :: W.Word8 -> ByteString
toBase2 x = C.pack $ List.map (to01 . Bin.testBit x) $ List.reverse [0..7]

to01 :: Bool -> Char
to01 False = '0'
to01 True  = '1'

fromBase2 :: ByteString -> W.Word8
fromBase2 bs =
  List.foldr (\b acc -> acc `Bin.setBit` (7 - b)) 0 $ C.elemIndices '1' bs

encodeBase2 :: Encoder
encodeBase2 = BStr.concatMap toBase2

decodeBase2 :: Decoder
decodeBase2 bs
  | C.any (\c -> c /= '0' && c /= '1') bs = Left InvalidBaseString
  | modlen /= 0 = decodeBase2 $ C.append (C.replicate (8 - modlen) '0') bs
  | otherwise = Right $ BStr.pack $ decodeBase2' bs []
  where
    modlen = BStr.length bs `mod` 8
    decodeBase2' "" acc = acc
    decodeBase2' bs acc = fromBase2 inits : decodeBase2' tails acc
      where (inits, tails) = BStr.splitAt 8 bs

encodeBase8 :: Encoder
encodeBase8 bs = C.pack $ map fromNum8 $ binToOct $ map toBit $ prepad ++ C.unpack encoded
  where
    prepad = replicate (if modlen == 0 then 0 else 3 - modlen) '0'
    modlen = C.length encoded `mod` 3
    encoded = encodeBase2 bs

decodeBase8 :: Decoder
decodeBase8 bs
  | C.any (\c -> not $ C.elem c b8) bs = Left InvalidBaseString
  | modlen == 6 = decodeBase2 $ C.drop 2 asBin
  | modlen == 3 = decodeBase2 $ C.drop 1 asBin
  | modlen == 0 = decodeBase2 asBin
  where
    modlen = C.length bs `mod` 8
    asBin = octalToBin bs

octalToBin :: ByteString -> ByteString
octalToBin bs = C.pack $ map fromBit $ concatMap (fromOct . toNum8) $ C.unpack bs

binToOct :: [W.Word8] -> [W.Word8]
binToOct [] = []
binToOct cs = toOct c1 c2 c3 : binToOct ctail
  where
    [c1, c2, c3] = take 3 cs
    ctail = drop 3 cs

toBit :: Char -> W.Word8
toBit '0' = 0
toBit _ = 1

toOct :: W.Word8 -> W.Word8 -> W.Word8 -> W.Word8
toOct c1 c2 c3 = Bin.shiftL c1 2 .|. Bin.shiftL c2 1 .|. c3

toNum8 :: Char -> W.Word8
toNum8 c = fromIntegral . fromJust $ C.elemIndex c b8

fromNum8 :: W.Word8 -> Char
fromNum8 = C.index b8 . fromIntegral

fromOct :: W.Word8 -> [W.Word8]
fromOct o = [Bin.shiftR o 2, Bin.shiftR o 1 .&. 1, o .&. 1]

fromBit :: W.Word8 -> Char
fromBit 0 = '0'
fromBit _ = '1'

--- ByteString base encoding--------------------------------------------------------------------------------------------------------------------------------------------------

-- word8ListToIntegerList :: [W.Word8] -> [Integer]
-- word8ListToIntegerList  = map toInteger



-- integerListToWord8List :: [Integer] -> [W.Word8]
-- integerListToWord8List  = map fromInteger



--------useful maps and map builders -----------------------------------------------------------------------------------------------------------------------------------------
-- lookupTable base byteSet =  Map.fromList (BStr.zip (BStr.take (fromInteger base) word8Range) byteSet)

-- invertedTable base byteSet =  Map.fromList (map (\(a , b) -> (b , a)) (Map.toList (lookupTable base byteSet)) )

-- word8Range = BStr.pack [0..255]


-- bNumToBl  =  Map.fromList [(16,1),(32,5),(64,3),(128,7)]

-- bNumToBlO  =  Map.fromList [(16,2),(32,8),(64,4),(128,8)]

---- bytesets ----------------------------------------------------------------------------------------------------------------------------------------------------------------

-- bID = word8Range

-- b1 = "1"

-- b2 = "01"

b8  = C.pack ['0'..'7']

-- b10 = C.pack ['0'..'9']

-- b16 = C.pack $ ['0'..'9'] ++ ['a'..'f']
-- b16U = C.pack $ ['0'..'9'] ++ ['A'..'F']

-- b32 = C.pack $ ['a'..'z'] ++ ['2'..'7']
-- b32U = C.pack $ ['A'..'Z'] ++ ['2'..'7']

-- b32hex = C.pack $ ['0'..'9'] ++ ['a'..'v']
-- b32hexU = C.pack $ ['0'..'9'] ++ ['A'..'V']

-- b32z = "ybndrfg8ejkmcpqxot1uwisza345h769" :: ByteString
-- -- b32z   = BStr.pack $ [121,98,110,100,114,102,103,56,101,106,107,109,99,112,113,120,111,116,49,117,119,105,115,122,97,51,52,
-- --                         53,104,55,54,57]

-- b58btc = Base58.unAlphabet $ Base58.bitcoinAlphabet
-- -- b58btc = BStr.pack $ [49..57] ++ [65..72] ++ [74..78] ++ [80..90] ++ [97..107] ++ [109..122]

-- b58flickr = Base58.unAlphabet $ Base58.flickrAlphabet
-- -- b58flickr = BStr.pack $ [49..57] ++ [97..107] ++ [109..122] ++ [65..72] ++ [74..78] ++ [80..90]

-- b64alphanum = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

-- b64 = C.pack $ b64alphanum ++ "+/"

-- b64URL = C.pack $ b64alphanum ++ "-_"
