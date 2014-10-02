-- |
-- Module      : Tests.Bench
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 Nicolas DI PRIMA
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Criterion.Main

import Data.ByteString.Pack
import Data.ByteString.Pack.Base32
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import Data.Word

import Data.Monoid
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze

import qualified Codec.Binary.Base32 as C32

main :: IO ()
main = defaultMain
    [ bgroup "just fill up with the same char" benchFillUpWith
--    , bgroup "write a bytestring" benchWriteByteString
--    , bgroup "write byte and bytestring" benchConcatBytes
    , bgroup "base32" benchBase32
    ]

benchFillUpWith =
    [ bench "bytestring: replicate" $ whnf (flip B.replicate 0x41) 2048
    , bench "bspack: fillUpWith" $ whnf (pack (fillUpWith (0x41 :: Word8))) 2048
    ]

exampleByteString :: B.ByteString
exampleByteString = B.replicate 1024 0x42

benchWriteByteString =
    [ bench "blaze" $ whnf (Blaze.toByteString) (Blaze.copyByteString exampleByteString)
    , bench "bytestring" $ nf (B.toLazyByteString) (B.byteString exampleByteString)
    , bench "bspack" $ whnf (flip pack 1024) (putByteString exampleByteString)
    ]

exampleShortByteString :: B.ByteString
exampleShortByteString = "Haskell rocks!"

concatPacker :: Packer ()
concatPacker = do
    putByteString exampleShortByteString
    putWord8 0x20

concatBlaze :: Blaze.Builder
concatBlaze =
      Blaze.copyByteString exampleShortByteString
   <> Blaze.fromChar ' '

concatBlazeShort :: Blaze.Builder
concatBlazeShort = mconcat $ replicate 5 concatBlaze

concatPackerShort :: Packer ()
concatPackerShort = sequence_ $ replicate 5 concatPacker

concatBlazeLong :: Blaze.Builder
concatBlazeLong = mconcat $ replicate 30 concatBlaze

concatPackerLong :: Packer ()
concatPackerLong = sequence_ $ replicate 30 concatPacker

benchConcatBytes =
    [ bench "blaze"       $ whnf (Blaze.toByteString) concatBlazeShort
    , bench "bspack"      $ whnf (flip pack 1024)     concatPackerShort
    , bench "blaze lazy"  $ nf (Blaze.toLazyByteString) concatBlazeLong
    , bench "blaze long"  $ whnf (Blaze.toByteString) concatBlazeLong
    , bench "bspack long" $ whnf (flip pack 2048)     concatPackerLong
    ]

benchBase32 =
    [ bench "bspack 512B" $ whnf (\bs -> pack (putByteStringBase32 True bs) 824)      $! B.replicate 512      0x42
    , bench "bspack 10KB" $ whnf (\bs -> pack (putByteStringBase32 True bs) 16000)    $! B.replicate 10000    0x42
    , bench "bspack 1MB"  $ whnf (\bs -> pack (putByteStringBase32 True bs) 1600000)  $! B.replicate 1000000  0x42
    , bench "bspack 10MB" $ whnf (\bs -> pack (putByteStringBase32 True bs) 16000000) $! B.replicate 10000000 0x42

    , bench "sandi 512B" $ whnf C32.encode $! B.replicate 512      0x42
    , bench "sandi 10KB" $ whnf C32.encode $! B.replicate 10000    0x42
    , bench "sandi 1MB"  $ whnf C32.encode $! B.replicate 1000000  0x42
    , bench "sandi 10MB" $ whnf C32.encode $! B.replicate 10000000 0x42
    ]
