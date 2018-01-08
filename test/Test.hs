{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Multibase
import qualified MultibaseOld as Old

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

encodes :: Base -> ByteString -> ByteString -> SpecWith ()
encodes base bin bout =
  it ("encodes " ++ show bin ++ " to " ++ show bout) $
  encode base bin `shouldBe` bout

decodesTo :: ByteString -> ByteString -> SpecWith ()
decodesTo bin bout =
  it ("decodes " ++ show bin ++ " to " ++ show bout) $
  decode bin `shouldBe` Right bout

roundtrips :: Base -> String -> Spec
roundtrips base baseName =
  prop ("roundtrips on " ++ baseName) $ \bstr ->
  decode (encode base bstr) `shouldBe` Right bstr

main :: IO ()
main = hspec $ modifyMaxSuccess (const 10000) $ do

  describe "encode" $ do

    context "first test vector" $ do
      let test1 = "Decentralize everything!!"

      encodes base16 test1 "f446563656e7472616c697a652065766572797468696e672121"
      encodes base58btc test1 "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe"

    context "second test vector" $ do
      let test2 = "yes mani !"

      encodes base2 test2
        "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
      encodes base8 test2 "7171312714403326055632220041"
      encodes base16 test2 "f796573206d616e692021"
      encodes base16 test2 "f796573206d616e692021"
      encodes base32hex test2 "vf5in683dc5n6i811"
      encodes base32 test2 "bpfsxgidnmfxgsibb"
      encodes base32z test2 "hxf1zgedpcfzg1ebb"
      encodes base58flickr test2 "Z7Pznk19XTTzBtx"
      encodes base58btc test2 "z7paNL19xttacUY"

    context "base2 edge case" $ do
      encodes base2 "\x00yes mani !"
        "00000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"

      encodes base2 "\x00\x00yes mani !"
        "0000000000000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"

  describe "decode" $ do

    context "first test vector" $ do
      let test1 = "Decentralize everything!!"

      "f446563656e7472616c697a652065766572797468696e672121" `decodesTo` test1
      "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe" `decodesTo` test1

    context "second test vector" $ do
      let test2 = "yes mani !"

      "01111001011001010111001100100000011011010110000101101110011010010010000000100001" `decodesTo` test2
      "7171312714403326055632220041" `decodesTo` test2
      "f796573206d616e692021" `decodesTo` test2
      "vf5in683dc5n6i811" `decodesTo` test2
      "bpfsxgidnmfxgsibb" `decodesTo` test2
      "hxf1zgedpcfzg1ebb" `decodesTo` test2
      "Z7Pznk19XTTzBtx" `decodesTo` test2
      "z7paNL19xttacUY" `decodesTo` test2

    context "base2 edge case" $ do
       "0001111001011001010111001100100000011011010110000101101110011010010010000000100001"
        `decodesTo` "\x00yes mani !"

       "000000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"
        `decodesTo `"\x00\x00yes mani !"

    context "rfc4648 base 16" $ do
      "f" `decodesTo` ""
      "f66" `decodesTo` "f"
      "f666f" `decodesTo` "fo"
      "f666f6f" `decodesTo` "foo"
      "f666f6f62" `decodesTo` "foob"
      "f666f6f6261" `decodesTo` "fooba"
      "f666f6f626172" `decodesTo` "foobar"

    context "rfc4648 unpadded base32" $ do
      "b" `decodesTo` ""
      "bmy" `decodesTo` "f"
      "bmzxq" `decodesTo` "fo"
      "bmzxw6" `decodesTo` "foo"
      "bmzxw6yq" `decodesTo` "foob"
      "bmzxw6ytb" `decodesTo` "fooba"
      "bmzxw6ytboI" `decodesTo` "foobar"

    context "RFC4648 base32" $ do
      "c" `decodesTo` ""
      "cmy======" `decodesTo` "f"
      "cmzxq====" `decodesTo` "fo"
      "cmzxw6===" `decodesTo` "foo"
      "cmzxw6yq=" `decodesTo` "foob"
      "cmzxw6ytb" `decodesTo` "fooba"
      "cmzxw6ytboI======" `decodesTo` "foobar"

    context "RFC4648 unpadded base32hex" $ do
      "v" `decodesTo` ""
      "vco" `decodesTo` "f"
      "vcpng" `decodesTo` "fo"
      "vcpnmu" `decodesTo` "foo"
      "vcpnmuog" `decodesTo` "foob"
      "vcpnmuoj1" `decodesTo` "fooba"
      "vcpnmuoj1e8" `decodesTo` "foobar"

    context "RFC4648 base32hex" $ do
      "t" `decodesTo` ""
      "tco======" `decodesTo` "f"
      "tcpng====" `decodesTo` "fo"
      "tcpnmu===" `decodesTo` "foo"
      "tcpnmuog=" `decodesTo` "foob"
      "tcpnmuoj1" `decodesTo` "fooba"
      "tcpnmuoj1e8======" `decodesTo` "foobar"

    context "RFC4648 unpadded base64" $ do
      "M" `decodesTo` ""
      "mZg" `decodesTo` "f"
      "mZm8" `decodesTo` "fo"
      "mZm9v" `decodesTo` "foo"
      "mZm9vYg" `decodesTo` "foob"
      "mZm9vYmE" `decodesTo` "fooba"
      "mZm9vYmFy" `decodesTo` "foobar"

    context "RFC4648 base64" $ do
      "M" `decodesTo` ""
      "MZg==" `decodesTo` "f"
      "MZm8=" `decodesTo` "fo"
      "MZm9v" `decodesTo` "foo"
      "MZm9vYg==" `decodesTo` "foob"
      "MZm9vYmE=" `decodesTo` "fooba"
      "MZm9vYmFy" `decodesTo` "foobar"

    it "fails on unknown bases" $ do
      decode "L1111" `shouldBe` Left UnknownBase
      decode "X1111" `shouldBe` Left UnknownBase

    it "fails with invalid characters" $ do
      decode "z7pa_L19xttacUY" `shouldBe` Left InvalidBaseString

  describe "roundtrip" $ do

    roundtrips baseId "baseId"
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
    roundtrips base32hexPad "base32hexPad"
    roundtrips base32hexPad' "base32hexPad uppercase"
    roundtrips base58btc "base58btc"
    roundtrips base58flickr "base58flickr"
    roundtrips base64 "base64"
    roundtrips base64pad "base64pad"
    roundtrips base64url "base64url"
    roundtrips base64urlPad "base64urlPad"
