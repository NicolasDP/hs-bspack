-- |
-- Module      : Data.ByteString.Pack
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 Nicolas DI PRIMA
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ByteString.Pack
    ( Packer
    , Result(..)
    , pack
      -- * Operations
      -- ** put
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
import Control.Applicative
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

-- A little cache to update the data
data Cache = Cache {-# UNPACK #-} !(Ptr Word8) -- pointer in the bytestring
                   {-# UNPACK #-} !Int         -- remaining size

instance Show Cache where
    show (Cache _ l) = show l

-- | Packing result:
--
-- * PackerOK a -> means the bytestring has been filled with the given data
-- * PackerMore a cache -> a temporary 
data Result a =
      PackerMore a Cache
    | PackerFail String
  deriving (Show)

-- | Simple Bytestring Packer
newtype Packer a = Packer { runPacker_ :: Cache -> IO (Result a) }

instance Functor Packer where
    fmap = fmapPacker

instance Applicative Packer where
    pure = returnPacker
    (<*>) = appendPacker

instance Monad Packer where
    return = returnPacker
    (>>=) = bindPacker

fmapPacker :: (a -> b) -> Packer a -> Packer b
fmapPacker f p = Packer $ \cache -> do
    rv <- runPacker_ p cache
    return $ case rv of
        PackerMore v cache' -> PackerMore (f v) cache'
        PackerFail err      -> PackerFail err

returnPacker :: a -> Packer a
returnPacker v = Packer $ \cache -> return $ PackerMore v cache

bindPacker :: Packer a -> (a -> Packer b) -> Packer b
bindPacker p fp = Packer $ \cache -> do
    rv <- runPacker_ p cache
    case rv of
        PackerMore v cache' -> runPacker_ (fp v) cache'
        PackerFail err      -> return $ PackerFail err

appendPacker :: Packer (a -> b) -> Packer a -> Packer b
appendPacker p1f p2 = p1f >>= \p1 -> p2 >>= \v -> return (p1 v)

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

-- run a sized action
actionPacker :: Int -> (Ptr Word8 -> IO a) -> Packer a
actionPacker s action = Packer $ \(Cache ptr size) ->
    case compare size s of
        LT -> return $ PackerFail "Not enough space in destination"
        _  -> do
            v <- action ptr
            return $ PackerMore v (Cache (ptr `plusPtr` s) (size - s))

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
