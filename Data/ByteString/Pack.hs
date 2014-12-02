-- |
-- Module      : Data.ByteString.Pack
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 Nicolas DI PRIMA
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
-- Simple ByteString packer
--
-- > > either error id $ flip pack 20 $ putWord8 0x41 >> putByteString "BCD" >> putWord8 0x20 >> putStorable (42 :: Word32)
-- > ABCD *\NUL\NUL\NUL"
module Data.ByteString.Pack
    ( Packer
    , Result(..)
    , pack
      -- * Operations
      -- ** put
    , putWord8
    , putWord16
    , putWord32
    , putStorable
    , putByteString
    , fillList
    , fillUpWith
      -- ** skip
    , skip
    , skipStorable
    ) where

import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString.Pack.Internal

-- | pack the given packer into the given bytestring
pack :: Packer a -> Int -> Either String ByteString
pack p len =
    unsafePerformIO $ do
        fptr <- B.mallocByteString len
        val <- withForeignPtr fptr $ \ptr ->
                    runPacker_ p (Cache ptr len)
        return $ case val of
            PackerMore _ (Cache _ r) -> Right (PS fptr 0 (len - r))
            PackerFail err           -> Left err

fillUpWithWord8' :: Word8 -> Packer ()
fillUpWithWord8' w = Packer $ \(Cache ptr size) -> do
    _ <- B.memset ptr w (fromIntegral size)
    return $ PackerMore () (Cache (ptr `plusPtr` size) (0))

-- | put a storable from the current position in the stream
putStorable :: Storable storable => storable -> Packer ()
putStorable s = actionPacker (sizeOf s) (\ptr -> poke (castPtr ptr) s)

-- | put a Bytestring from the current position in the stream
--
-- If the ByteString ins null, then do nothing
putByteString :: ByteString -> Packer ()
putByteString bs
    | neededLength == 0 = return ()
    | otherwise         = actionPacker neededLength (actionPackerByteString bs)
  where
    neededLength :: Int
    neededLength = B.length bs

    actionPackerByteString :: ByteString -> Ptr Word8 -> IO ()
    actionPackerByteString (PS fptr off _) ptr =
        withForeignPtr fptr $ \srcptr ->
            B.memcpy ptr (srcptr `plusPtr` off) neededLength

-- | skip some bytes from the current position in the stream
skip :: Int -> Packer ()
skip n = actionPacker n (\_ -> return ())

-- | skip the size of a storable from the current position in the stream
skipStorable :: Storable storable => storable -> Packer ()
skipStorable = skip . sizeOf

-- | fill up from the current position in the stream to the end
--
-- it is basically:
-- > fillUpWith s == fillList (repeat s)
fillUpWith :: Storable storable => storable -> Packer ()
fillUpWith s = fillList $ repeat s
{-# RULES "fillUpWithWord8" forall s . fillUpWith s = fillUpWithWord8' s #-}
{-# NOINLINE fillUpWith #-}

-- | Will put the given storable list from the current position in the stream
-- to the end.
--
-- This function will fail with not enough storage if the given storable can't
-- be written (not enough space)
--
-- example:
-- > pack (fillList $ [1..] :: Word8) 9    ==> "\1\2\3\4\5\6\7\8\9"
-- > pack (fillList $ [1..] :: Word32) 4   ==> "\1\0\0\0"
-- > pack (fillList $ [1..] :: Word32) 64  -- will work
-- > pack (fillList $ [1..] :: Word32) 1   -- will fail (not enough space)
-- > pack (fillList $ [1..] :: Word32) 131 -- will fail (not enough space)
fillList :: Storable storable => [storable] -> Packer ()
fillList []     = return ()
fillList (x:xs) = putStorable x >> fillList xs

------------------------------------------------------------------------------
-- Common packer                                                            --
------------------------------------------------------------------------------

-- | put Word8 in the current position in the stream
putWord8 :: Word8 -> Packer ()
putWord8 = putStorable
{-# INLINE putWord8 #-}

-- | put Word16 in the current position in the stream
-- /!\ use Host Endianness
putWord16 :: Word16 -> Packer ()
putWord16 = putStorable
{-# INLINE putWord16 #-}

-- | put Word32 in the current position in the stream
-- /!\ use Host Endianness
putWord32 :: Word32 -> Packer ()
putWord32 = putStorable
{-# INLINE putWord32 #-}
