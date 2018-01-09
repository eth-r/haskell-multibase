module Main (main) where

import Criterion.Main
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Multibase

main :: IO ()
main = defaultMain
  -- Base2
  [ bench "base2/encode/1k" $ nf (encode base2)
      $ BS.replicate 1000 0x8e
  , bench "base2/encode/5k" $ nf (encode base2)
      $ BS.replicate 5000 0x8e

  , bench "base2/decode/1k" $ nf decode
      $ BC.cons '0' $ BC.replicate 1000 '1'
  , bench "base2/decode/5k" $ nf decode
      $ BC.cons '0' $ BC.replicate 5000 '1'

  -- Base8
  , bench "base8/encode/1k" $ nf (encode base8)
      $ BS.replicate 1000 0x8e
  , bench "base8/encode/5k" $ nf (encode base8)
      $ BS.replicate 5000 0x8e

  , bench "base8/decode/1k" $ nf decode
      $ BC.cons '7' $ BC.replicate 1000 '1'
  , bench "base8/decode/5k" $ nf decode
      $ BC.cons '7' $ BC.replicate 5000 '1'

  -- Base10
  , bench "base10/encode/1k" $ nf (encode base10)
      $ BS.replicate 1000 0x8e
  , bench "base10/encode/5k" $ nf (encode base10)
      $ BS.replicate 5000 0x8e

  , bench "base10/decode/1k" $ nf decode
      $ BC.cons '9' $ BC.replicate 1000 '1'
  , bench "base10/decode/5k" $ nf decode
      $ BC.cons '9' $ BC.replicate 5000 '1'

  -- Base16
  , bench "base16/encode/1M" $ nf (encode base16)
      $ BS.replicate 1000000 0x8e
  , bench "base16/encode/5M" $ nf (encode base16)
      $ BS.replicate 5000000 0x8e

  , bench "base16/decode/1M" $ nf decode
      $ BC.cons 'f' $ BC.replicate 1000000 'a'
  , bench "base16/decode/5M" $ nf decode
      $ BC.cons 'f' $ BC.replicate 5000000 'a'

  -- BASE16
  , bench "BASE16/encode/1M" $ nf (encode base16')
      $ BS.replicate 1000000 0x8e
  , bench "BASE16/encode/5M" $ nf (encode base16')
      $ BS.replicate 5000000 0x8e

  , bench "BASE16/decode/1M" $ nf decode
      $ BC.cons 'F' $ BC.replicate 1000000 'A'
  , bench "BASE16/decode/5M" $ nf decode
      $ BC.cons 'F' $ BC.replicate 5000000 'A'

  -- Base32
  , bench "base32/encode/1M" $ nf (encode base32)
      $ BS.replicate 1000000 0x8e
  , bench "base32/encode/5M" $ nf (encode base32)
      $ BS.replicate 5000000 0x8e

  , bench "base32/decode/1M" $ nf decode
      $ BC.cons 'b' $ BC.replicate 1000000 'a'
  , bench "base32/decode/5M" $ nf decode
      $ BC.cons 'b' $ BC.replicate 5000000 'a'

  -- BASE32
  , bench "BASE32/encode/1M" $ nf (encode base32')
      $ BS.replicate 1000000 0x8e
  , bench "BASE32/encode/5M" $ nf (encode base32')
      $ BS.replicate 5000000 0x8e

  , bench "BASE32/decode/1M" $ nf decode
      $ BC.cons 'B' $ BC.replicate 1000000 'A'
  , bench "BASE32/decode/5M" $ nf decode
      $ BC.cons 'B' $ BC.replicate 5000000 'A'

  -- Base32hex
  , bench "base32hex/encode/1M" $ nf (encode base32hex)
      $ BS.replicate 1000000 0x8e
  , bench "base32hex/encode/5M" $ nf (encode base32hex)
      $ BS.replicate 5000000 0x8e

  , bench "base32hex/decode/1M" $ nf decode
      $ BC.cons 'v' $ BC.replicate 1000000 'a'
  , bench "base32hex/decode/5M" $ nf decode
      $ BC.cons 'v' $ BC.replicate 5000000 'a'

  -- BASE32HEX
  , bench "BASE32HEX/encode/1M" $ nf (encode base32hex')
      $ BS.replicate 1000000 0x8e
  , bench "BASE32HEX/encode/5M" $ nf (encode base32hex')
      $ BS.replicate 5000000 0x8e

  , bench "BASE32HEX/decode/1M" $ nf decode
      $ BC.cons 'V' $ BC.replicate 1000000 'A'
  , bench "BASE32HEX/decode/5M" $ nf decode
      $ BC.cons 'V' $ BC.replicate 5000000 'A'

  -- Base32z
  , bench "z-base32/encode/1M" $ nf (encode base32z)
      $ BS.replicate 1000000 0x8e
  , bench "z-base32/encode/5M" $ nf (encode base32z)
      $ BS.replicate 5000000 0x8e

  , bench "z-base32/decode/1M" $ nf decode
      $ BC.cons 'h' $ BC.replicate 1000000 'y'
  , bench "z-base32/decode/5M" $ nf decode
      $ BC.cons 'h' $ BC.replicate 5000000 'y'

  -- Base58btc
  , bench "base58btc/encode/1k" $ nf (encode base58btc)
      $ BS.replicate 1000 0x8e
  , bench "base58btc/encode/5k" $ nf (encode base58btc)
      $ BS.replicate 5000 0x8e

  , bench "base58btc/decode/1k" $ nf decode
      $ BC.cons 'z' $ BC.replicate 1000 '2'
  , bench "base58btc/decode/5k" $ nf decode
      $ BC.cons 'z' $ BC.replicate 5000 '2'

  -- Base58flickr
  , bench "base58flickr/encode/1k" $ nf (encode base58flickr)
      $ BS.replicate 1000 0x8e
  , bench "base58flickr/encode/5k" $ nf (encode base58flickr)
      $ BS.replicate 5000 0x8e

  , bench "base58flickr/decode/1k" $ nf decode
      $ BC.cons 'Z' $ BC.replicate 1000 '2'
  , bench "base58flickr/decode/5k" $ nf decode
      $ BC.cons 'Z' $ BC.replicate 5000 '2'

  -- Base64
  , bench "base64/encode/1M" $ nf (encode base64)
      $ BS.replicate 1000000 0x8e
  , bench "base64/encode/5M" $ nf (encode base64)
      $ BS.replicate 5000000 0x8e

  , bench "base64/decode/1M" $ nf decode
      $ BC.cons 'm' $ BC.replicate 1000000 'a'
  , bench "base64/decode/5M" $ nf decode
      $ BC.cons 'm' $ BC.replicate 5000000 'a'

  -- Base64pad
  , bench "base64pad/encode/1M" $ nf (encode base64pad)
      $ BS.replicate 1000000 0x8e
  , bench "base64pad/encode/5M" $ nf (encode base64pad)
      $ BS.replicate 5000000 0x8e

  , bench "base64pad/decode/1M" $ nf decode
      $ BC.cons 'M' $ BC.replicate 1000000 'a'
  , bench "base64pad/decode/5M" $ nf decode
      $ BC.cons 'M' $ BC.replicate 5000000 'a'

  -- Base64url
  , bench "base64url/encode/1M" $ nf (encode base64url)
      $ BS.replicate 1000000 0x8e
  , bench "base64url/encode/5M" $ nf (encode base64url)
      $ BS.replicate 5000000 0x8e

  , bench "base64url/decode/1M" $ nf decode
      $ BC.cons 'u' $ BC.replicate 1000000 'a'
  , bench "base64url/decode/5M" $ nf decode
      $ BC.cons 'u' $ BC.replicate 5000000 'a'

  -- Base64urlpad
  , bench "base64urlpad/encode/1M" $ nf (encode base64urlpad)
      $ BS.replicate 1000000 0x8e
  , bench "base64urlpad/encode/5M" $ nf (encode base64urlpad)
      $ BS.replicate 5000000 0x8e

  , bench "base64urlpad/decode/1M" $ nf decode
      $ BC.cons 'U' $ BC.replicate 1000000 'a'
  , bench "base64urlpad/decode/5M" $ nf decode
      $ BC.cons 'U' $ BC.replicate 5000000 'a'
  ]
