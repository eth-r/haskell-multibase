{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.ByteString.Multibase

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

encodes :: String -> Base -> ByteString -> ByteString -> SpecWith ()
encodes baseName base bin bout =
  it (baseName ++ " encodes " ++ show bin ++ " to " ++ show bout) $
  encode base bin `shouldBe` bout

decodes :: String -> ByteString -> ByteString -> SpecWith ()
decodes baseName bin bout =
  it (show bout ++ " decodes to " ++ show bin) $
  decode bout `shouldBe` Right bin

decodesTo :: ByteString -> ByteString -> SpecWith ()
decodesTo bin bout =
  it ("decodes " ++ show bin ++ " to " ++ show bout) $
  decode bin `shouldBe` Right bout

roundtrips :: Base -> String -> Spec
roundtrips base baseName =
  prop ("roundtrips on " ++ baseName) $ \bstr ->
  decode (encode base bstr) `shouldBe` Right bstr

main :: IO ()
main = hspec $ do

  describe "encode" $ do

    context "first test vector" $ do
      let test1 = "Decentralize everything!!"

      encodes "base2" base2 test1 "001000100011001010110001101100101011011100111010001110010011000010110110001101001011110100110010100100000011001010111011001100101011100100111100101110100011010000110100101101110011001110010000100100001"
      encodes "base8" base8 test1 "71043126154533472162302661513646244031273145344745643206455631620441"
      encodes "base16" base16 test1 "f446563656e7472616c697a652065766572797468696e672121"
      encodes "BASE16" base16' test1 "F446563656E7472616C697A652065766572797468696E672121"
      encodes "base32" base32 test1 "birswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb"
      encodes "BASE32" base32' test1 "BIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB"
      encodes "base32hex" base32hex test1 "v8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891"
      encodes "BASE32HEX" base32hex' test1 "V8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891"
      encodes "base32pad" base32pad test1 "cirswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb"
      encodes "BASE32PAD" base32pad' test1 "CIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB"
      encodes "base32hexpad" base32hexpad test1 "t8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891"
      encodes "BASE32HEXPAD" base32hexpad' test1 "T8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891"
      encodes "base32z" base32z test1 "het1sg3mqqt3gn5djxj11y3msci3817depfzgqejb"
      encodes "base58flickr" base58flickr test1 "Ztwe7gVTeK8wswS1gf8hrgAua9fcw9reboD"
      encodes "base58btc" base58btc test1 "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe"
      encodes "base64" base64 test1 "mRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ"
      encodes "base64pad" base64pad test1 "MRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ=="
      encodes "base64url" base64url test1 "uRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ"
      encodes "base64urlpad" base64urlpad test1 "URGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ=="

    context "second test vector" $ do
      let test2 = "yes mani !"

      encodes "base2" base2 test2 "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
      encodes "base8" base8 test2 "7171312714403326055632220041"
      encodes "base16" base16 test2 "f796573206d616e692021"
      encodes "BASE16" base16' test2 "F796573206D616E692021"
      encodes "base32" base32 test2 "bpfsxgidnmfxgsibb"
      encodes "BASE32" base32' test2 "BPFSXGIDNMFXGSIBB"
      encodes "base32hex" base32hex test2 "vf5in683dc5n6i811"
      encodes "BASE32HEX" base32hex' test2 "VF5IN683DC5N6I811"
      encodes "base32pad" base32pad test2 "cpfsxgidnmfxgsibb"
      encodes "BASE32PAD" base32pad' test2 "CPFSXGIDNMFXGSIBB"
      encodes "base32hexpad" base32hexpad test2 "tf5in683dc5n6i811"
      encodes "BASE32HEXPAD" base32hexpad' test2 "TF5IN683DC5N6I811"
      encodes "base32z" base32z test2 "hxf1zgedpcfzg1ebb"
      encodes "base58flickr" base58flickr test2 "Z7Pznk19XTTzBtx"
      encodes "base58btc" base58btc test2 "z7paNL19xttacUY"
      encodes "base64" base64 test2 "meWVzIG1hbmkgIQ"
      encodes "base64pad" base64pad test2 "MeWVzIG1hbmkgIQ=="
      encodes "base64url" base64url test2 "ueWVzIG1hbmkgIQ"
      encodes "base64urlpad" base64urlpad test2 "UeWVzIG1hbmkgIQ=="

    context "third test vector" $ do
      let test3 = "hello world"

      encodes "base2" base2 test3 "00110100001100101011011000110110001101111001000000111011101101111011100100110110001100100"
      encodes "base8" base8 test3 "7064145330661571007355734466144"
      encodes "base16" base16 test3 "f68656c6c6f20776f726c64"
      encodes "BASE16" base16' test3 "F68656C6C6F20776F726C64"
      encodes "base32" base32 test3 "bnbswy3dpeb3w64tmmq"
      encodes "BASE32" base32' test3 "BNBSWY3DPEB3W64TMMQ"
      encodes "base32hex" base32hex test3 "vd1imor3f41rmusjccg"
      encodes "BASE32HEX" base32hex' test3 "VD1IMOR3F41RMUSJCCG"
      encodes "base32pad" base32pad test3 "cnbswy3dpeb3w64tmmq======"
      encodes "BASE32PAD" base32pad' test3 "CNBSWY3DPEB3W64TMMQ======"
      encodes "base32hexpad" base32hexpad test3 "td1imor3f41rmusjccg======"
      encodes "BASE32HEXPAD" base32hexpad' test3 "TD1IMOR3F41RMUSJCCG======"
      encodes "base32z" base32z test3 "hpb1sa5dxrb5s6hucco"
      encodes "base58flickr" base58flickr test3 "ZrTu1dk6cWsRYjYu"
      encodes "base58btc" base58btc test3 "zStV1DL6CwTryKyV"
      encodes "base64" base64 test3 "maGVsbG8gd29ybGQ"
      encodes "base64pad" base64pad test3 "MaGVsbG8gd29ybGQ="
      encodes "base64url" base64url test3 "uaGVsbG8gd29ybGQ"
      encodes "base64urlpad" base64urlpad test3 "UaGVsbG8gd29ybGQ="

    context "first edge case" $ do
      let test4 = "\x00yes mani !"

      encodes "base2" base2 test4 "00000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"
      encodes "base8" base8 test4 "7000171312714403326055632220041"
      encodes "base16" base16 test4 "f00796573206d616e692021"
      encodes "BASE16" base16' test4 "F00796573206D616E692021"
      encodes "base32" base32 test4 "bab4wk4zanvqw42jaee"
      encodes "BASE32" base32' test4 "BAB4WK4ZANVQW42JAEE"
      encodes "base32hex" base32hex test4 "v01smasp0dlgmsq9044"
      encodes "BASE32HEX" base32hex' test4 "V01SMASP0DLGMSQ9044"
      encodes "base32pad" base32pad test4 "cab4wk4zanvqw42jaee======"
      encodes "BASE32PAD" base32pad' test4 "CAB4WK4ZANVQW42JAEE======"
      encodes "base32hexpad" base32hexpad test4 "t01smasp0dlgmsq9044======"
      encodes "BASE32HEXPAD" base32hexpad' test4 "T01SMASP0DLGMSQ9044======"
      encodes "base32z" base32z test4 "hybhskh3ypiosh4jyrr"
      encodes "base58flickr" base58flickr test4 "Z17Pznk19XTTzBtx"
      encodes "base58btc" base58btc test4 "z17paNL19xttacUY"
      encodes "base64" base64 test4 "mAHllcyBtYW5pICE"
      encodes "base64pad" base64pad test4 "MAHllcyBtYW5pICE="
      encodes "base64url" base64url test4 "uAHllcyBtYW5pICE"
      encodes "base64urlpad" base64urlpad test4 "UAHllcyBtYW5pICE="

    context "second edge case" $ do
      let test5 = "\x00\x00yes mani !"

      encodes "base2" base2 test5 "0000000000000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"
      encodes "base8" base8 test5 "700000171312714403326055632220041"
      encodes "base16" base16 test5 "f0000796573206d616e692021"
      encodes "BASE16" base16' test5 "F0000796573206D616E692021"
      encodes "base32" base32 test5 "baaahszltebwwc3tjeaqq"
      encodes "BASE32" base32' test5 "BAAAHSZLTEBWWC3TJEAQQ"
      encodes "base32hex" base32hex test5 "v0007ipbj41mm2rj940gg"
      encodes "BASE32HEX" base32hex' test5 "V0007IPBJ41MM2RJ940GG"
      encodes "base32pad" base32pad test5 "caaahszltebwwc3tjeaqq===="
      encodes "BASE32PAD" base32pad' test5 "CAAAHSZLTEBWWC3TJEAQQ===="
      encodes "base32hexpad" base32hexpad test5 "t0007ipbj41mm2rj940gg===="
      encodes "BASE32HEXPAD" base32hexpad' test5 "T0007IPBJ41MM2RJ940GG===="
      encodes "base32z" base32z test5 "hyyy813murbssn5ujryoo"
      encodes "base58flickr" base58flickr test5 "Z117Pznk19XTTzBtx"
      encodes "base58btc" base58btc test5 "z117paNL19xttacUY"
      encodes "base64" base64 test5 "mAAB5ZXMgbWFuaSAh"
      encodes "base64pad" base64pad test5 "MAAB5ZXMgbWFuaSAh"
      encodes "base64url" base64url test5 "uAAB5ZXMgbWFuaSAh"
      encodes "base64urlpad" base64urlpad test5 "UAAB5ZXMgbWFuaSAh"

  describe "decode" $ do

    context "first test vector" $ do
      let test1 = "Decentralize everything!!"

      "001000100011001010110001101100101011011100111010001110010011000010110110001101001011110100110010100100000011001010111011001100101011100100111100101110100011010000110100101101110011001110010000100100001"
        `decodesTo` test1
      "71043126154533472162302661513646244031273145344745643206455631620441"
        `decodesTo` test1
      "f446563656e7472616c697a652065766572797468696e672121" `decodesTo` test1
      "F446563656E7472616C697A652065766572797468696E672121" `decodesTo` test1
      "birswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb" `decodesTo` test1
      "BIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB" `decodesTo` test1
      "v8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891" `decodesTo` test1
      "V8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891" `decodesTo` test1
      "cirswgzloorzgc3djpjssazlwmvzhs5dinfxgoijb" `decodesTo` test1
      "CIRSWGZLOORZGC3DJPJSSAZLWMVZHS5DINFXGOIJB" `decodesTo` test1
      "t8him6pbeehp62r39f9ii0pbmclp7it38d5n6e891" `decodesTo` test1
      "T8HIM6PBEEHP62R39F9II0PBMCLP7IT38D5N6E891" `decodesTo` test1
      "het1sg3mqqt3gn5djxj11y3msci3817depfzgqejb" `decodesTo` test1
      "Ztwe7gVTeK8wswS1gf8hrgAua9fcw9reboD" `decodesTo` test1
      "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe" `decodesTo` test1
      "mRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ" `decodesTo` test1
      "MRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ==" `decodesTo` test1
      "uRGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ" `decodesTo` test1
      "URGVjZW50cmFsaXplIGV2ZXJ5dGhpbmchIQ==" `decodesTo` test1

    context "second test vector" $ do
      let test2 = "yes mani !"

      "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
        `decodesTo` test2
      "7171312714403326055632220041" `decodesTo` test2
      "f796573206d616e692021" `decodesTo` test2
      "F796573206D616E692021" `decodesTo` test2
      "bpfsxgidnmfxgsibb" `decodesTo` test2
      "BPFSXGIDNMFXGSIBB" `decodesTo` test2
      "vf5in683dc5n6i811" `decodesTo` test2
      "VF5IN683DC5N6I811" `decodesTo` test2
      "cpfsxgidnmfxgsibb" `decodesTo` test2
      "CPFSXGIDNMFXGSIBB" `decodesTo` test2
      "tf5in683dc5n6i811" `decodesTo` test2
      "TF5IN683DC5N6I811" `decodesTo` test2
      "hxf1zgedpcfzg1ebb" `decodesTo` test2
      "Z7Pznk19XTTzBtx" `decodesTo` test2
      "z7paNL19xttacUY" `decodesTo` test2
      "meWVzIG1hbmkgIQ" `decodesTo` test2
      "MeWVzIG1hbmkgIQ==" `decodesTo` test2
      "ueWVzIG1hbmkgIQ" `decodesTo` test2
      "UeWVzIG1hbmkgIQ==" `decodesTo` test2

    context "third test vector" $ do
      let test3 = "hello world"

      "00110100001100101011011000110110001101111001000000111011101101111011100100110110001100100"
        `decodesTo` test3
      "7064145330661571007355734466144" `decodesTo` test3
      "f68656c6c6f20776f726c64" `decodesTo` test3
      "F68656C6C6F20776F726C64" `decodesTo` test3
      "bnbswy3dpeb3w64tmmq" `decodesTo` test3
      "BNBSWY3DPEB3W64TMMQ" `decodesTo` test3
      "vd1imor3f41rmusjccg" `decodesTo` test3
      "VD1IMOR3F41RMUSJCCG" `decodesTo` test3
      "cnbswy3dpeb3w64tmmq======" `decodesTo` test3
      "CNBSWY3DPEB3W64TMMQ======" `decodesTo` test3
      "td1imor3f41rmusjccg======" `decodesTo` test3
      "TD1IMOR3F41RMUSJCCG======" `decodesTo` test3
      "hpb1sa5dxrb5s6hucco" `decodesTo` test3
      "ZrTu1dk6cWsRYjYu" `decodesTo` test3
      "zStV1DL6CwTryKyV" `decodesTo` test3
      "maGVsbG8gd29ybGQ" `decodesTo` test3
      "MaGVsbG8gd29ybGQ=" `decodesTo` test3
      "uaGVsbG8gd29ybGQ" `decodesTo` test3
      "UaGVsbG8gd29ybGQ=" `decodesTo` test3

    context "first edge case" $ do
      let test4 = "\x00yes mani !"

      "00000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"
        `decodesTo` test4
      "7000171312714403326055632220041" `decodesTo` test4
      "f00796573206d616e692021" `decodesTo` test4
      "F00796573206D616E692021" `decodesTo` test4
      "bab4wk4zanvqw42jaee" `decodesTo` test4
      "BAB4WK4ZANVQW42JAEE" `decodesTo` test4
      "v01smasp0dlgmsq9044" `decodesTo` test4
      "V01SMASP0DLGMSQ9044" `decodesTo` test4
      "cab4wk4zanvqw42jaee======" `decodesTo` test4
      "CAB4WK4ZANVQW42JAEE======" `decodesTo` test4
      "t01smasp0dlgmsq9044======" `decodesTo` test4
      "T01SMASP0DLGMSQ9044======" `decodesTo` test4
      "hybhskh3ypiosh4jyrr" `decodesTo` test4
      "Z17Pznk19XTTzBtx" `decodesTo` test4
      "z17paNL19xttacUY" `decodesTo` test4
      "mAHllcyBtYW5pICE" `decodesTo` test4
      "MAHllcyBtYW5pICE=" `decodesTo` test4
      "uAHllcyBtYW5pICE" `decodesTo` test4
      "UAHllcyBtYW5pICE=" `decodesTo` test4

    context "second edge case" $ do
      let test5 = "\x00\x00yes mani !"

      "0000000000000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"
        `decodesTo` test5
      "700000171312714403326055632220041" `decodesTo` test5
      "f0000796573206d616e692021" `decodesTo` test5
      "F0000796573206D616E692021" `decodesTo` test5
      "baaahszltebwwc3tjeaqq" `decodesTo` test5
      "BAAAHSZLTEBWWC3TJEAQQ" `decodesTo` test5
      "v0007ipbj41mm2rj940gg" `decodesTo` test5
      "V0007IPBJ41MM2RJ940GG" `decodesTo` test5
      "caaahszltebwwc3tjeaqq====" `decodesTo` test5
      "CAAAHSZLTEBWWC3TJEAQQ====" `decodesTo` test5
      "t0007ipbj41mm2rj940gg====" `decodesTo` test5
      "T0007IPBJ41MM2RJ940GG====" `decodesTo` test5
      "hyyy813murbssn5ujryoo" `decodesTo` test5
      "Z117Pznk19XTTzBtx" `decodesTo` test5
      "z117paNL19xttacUY" `decodesTo` test5
      "mAAB5ZXMgbWFuaSAh" `decodesTo` test5
      "MAAB5ZXMgbWFuaSAh" `decodesTo` test5
      "uAAB5ZXMgbWFuaSAh" `decodesTo` test5
      "UAAB5ZXMgbWFuaSAh" `decodesTo` test5

    context "mixed case" $ do
      let test3 = "hello world"

      "f68656c6c6f20776F726C64" `decodesTo` test3
      "F68656c6c6f20776F726C64" `decodesTo` test3
      "bnbswy3dpeB3W64TMMQ" `decodesTo` test3
      "Bnbswy3dpeB3W64TMMQ" `decodesTo` test3
      "vd1imor3f41RMUSJCCG" `decodesTo` test3
      "Vd1imor3f41RMUSJCCG" `decodesTo` test3
      "cnbswy3dpeB3W64TMMQ======" `decodesTo` test3
      "Cnbswy3dpeB3W64TMMQ======" `decodesTo` test3
      "td1imor3f41RMUSJCCG======" `decodesTo` test3
      "Td1imoR3f41RMUSJCCG======" `decodesTo` test3

    it "fails on unknown bases" $ do
      decode "L1111" `shouldBe` Left UnknownBase
      decode "X1111" `shouldBe` Left UnknownBase

    it "fails with invalid characters" $ do
      decode "z7pa_L19xttacUY" `shouldBe` Left InvalidBaseString

  describe "roundtrip" $ do

    roundtrips identity "baseId"
    roundtrips base2 "base2"
    roundtrips base8 "base8"
    roundtrips base16 "base16"
    roundtrips base16' "base16 uppercase"
    roundtrips base32 "base32"
    roundtrips base32' "base32 uppercase"
    roundtrips base32pad "base32pad"
    roundtrips base32pad' "base32pad uppercase"
    roundtrips base32z "base32z"
    roundtrips base32hex "base32hex"
    roundtrips base32hex' "base32hex uppercase"
    roundtrips base32hexpad "base32hexpad"
    roundtrips base32hexpad' "base32hexpad uppercase"
    roundtrips base58btc "base58btc"
    roundtrips base58flickr "base58flickr"
    roundtrips base64 "base64"
    roundtrips base64pad "base64pad"
    roundtrips base64url "base64url"
    roundtrips base64urlpad "base64urlpad"
