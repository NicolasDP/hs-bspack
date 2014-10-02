-- |
-- Module      : Data.ByteString.Pack.Base32
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 Nicolas DI PRIMA
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
-- Base32 converstion (see RFC4648)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Pack.Base32
    ( putByteStringBase32
    , guessEncodedLength
    ) where

import           Control.Monad (void)
import           Data.Bits
import qualified Data.ByteString          as B
import           Data.ByteString.Internal as B
import           Data.ByteString.Pack.Internal
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Prim
import           GHC.Types
import           GHC.Word

putByteStringBase32 :: Bool -> ByteString -> Packer ()
putByteStringBase32 padding bs
    | neededLength == 0 = return ()
    | otherwise         = actionPackerWithRemain neededLength (actionPackerEncode32 bs)
  where
    sourceLength :: Int
    sourceLength = B.length bs
    neededLength :: Int
    neededLength = guessEncodedLength sourceLength

    actionPackerEncode32 :: ByteString -> Ptr Word8 -> Int -> IO (Int, ())
    actionPackerEncode32 (PS srcfptr off _) dstptr _ =
        withForeignPtr srcfptr $ \srcptr -> do
            (ndstptr, remain) <- encode32Ptr (srcptr `plusPtr` off) dstptr sourceLength neededLength (0x00, 0)
            if padding
                then B.memset ndstptr 0x3d (fromIntegral remain) >> return (0, ())
                else return (remain, ())

encode32Ptr :: Ptr Word8    -- ^ the source ptr
            -> Ptr Word8    -- ^ the destination ptr
            -> Int          -- ^ the source length
            -> Int          -- ^ the destination length
            -> (Word8, Int) -- ^ a cache to bufferize the bits before flushing them into dest
            -> IO (Ptr Word8, Int)
-- OK: all the bytes have been consumned, the Cache has been flushed
encode32Ptr _       !dstptr  0  0 (!bits, !size) = do
    _ <- flush dstptr (bits, size)
    return (dstptr, 0)
encode32Ptr _       !dstptr  0 !k (!bits, !size) = do
    _ <- flush dstptr (bits, size)
    return (dstptr `plusPtr` 1, k - 1)
-- OK: consume the byte and iterate
encode32Ptr !srcptr !dstptr !n !k (!bits, !size) = do
    w <- peek srcptr
    case size of
        0 -> flush dstptr (bufferize w 1 5 (bits, size)) >>= encodeNext 1 1 . bufferize w 6 8
        1 -> flush dstptr (bufferize w 1 4 (bits, size)) >>= encodeNext 1 1 . bufferize w 5 8
        2 -> flush dstptr (bufferize w 1 3 (bits, size)) >>= flush (dstptr `plusPtr` 1) . bufferize w 4 8 >>= encodeNext 1 2
        3 -> flush dstptr (bufferize w 1 2 (bits, size)) >>= flush (dstptr `plusPtr` 1) . bufferize w 3 7 >>= encodeNext 1 2 . bufferize w 8 8
        4 -> flush dstptr (bufferize w 1 1 (bits, size)) >>= flush (dstptr `plusPtr` 1) . bufferize w 2 6 >>= encodeNext 1 2 . bufferize w 7 8
        _ -> undefined
  where
    encodeNext :: Int -> Int -> (Word8, Int) -> IO (Ptr Word8, Int)
    encodeNext !srcoff !dstoff = encode32Ptr (srcptr `plusPtr` srcoff) (dstptr `plusPtr` dstoff) (n - srcoff) (k - dstoff)
    {-# INLINE encodeNext #-}
                 
flush :: Ptr Word8 -> (Word8, Int) -> IO (Word8, Int)
flush !ptr (!w, _) = do
    poke ptr (toBase32 $ fromIntegral w)
    return (0, 0)
{-# INLINE flush #-}

bufferize :: Word8
          -> Int
          -> Int
          -> (Word8, Int)
          -> (Word8, Int)
bufferize !w !from !to (!bits, !nbRead) = (newBits, newNbRead)
  where
    newBits :: Word8
    newBits = bits .|. (((w `shiftR` shifterR) .&. mask) `shiftL` shifterL)
    newNbRead :: Int
    newNbRead = nbRead + size

    shifterR :: Int
    shifterR = 8 - to
    shifterL :: Int
    shifterL = 5 - size - nbRead

    size :: Int
    size = to - from + 1
    mask :: Word8
    mask = getMask size
{-# INLINE bufferize #-}

-- | return the maximum needed length to convert in Base32
guessEncodedLength :: Int -- ^ the lenght of the Bytestring to convert into base32
                   -> Int
guessEncodedLength 0 = 0
guessEncodedLength l
  | modulo == 0 = 8 * l `div` 5
  | otherwise   = 8 * (l + 5 - modulo) `div` 5
  where
    modulo :: Int
    modulo = l `mod` 5

------------------------------------------------------------------------------
--                                Helpers                                   --
------------------------------------------------------------------------------

getMask :: Int -> Word8
getMask !n =
  case n of
    0 -> 0x00 -- 0000 0000
    1 -> 0x01 -- 0000 0001
    2 -> 0x03 -- 0000 0011
    3 -> 0x07 -- 0000 0111
    4 -> 0x0F -- 0000 1111
    5 -> 0x1F -- 0001 1111
    6 -> 0x3F -- 0011 1111
    7 -> 0x7F -- 0111 1111
    _ -> 0xFF -- 1111 1111

toBase32 :: Word8 -> Word8
toBase32 !w
  | index < 32 = W8# (indexWord8OffAddr# addr i)
  | otherwise  = error $ "toWord8: bad input: cannot convert '" ++ (show index) ++ "'"
  where
    index :: Int
    index = fromIntegral w

    !(I# i) = index
    !(Table addr) = alphabet

data Table = Table !Addr#

alphabet :: Table
alphabet = Table "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#
