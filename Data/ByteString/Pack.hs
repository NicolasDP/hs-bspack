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
    , putListOfStorable
    , putByteString
    , fillUpWith
      -- ** skip
    , skip
    , skipStorable
    ) where

import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable

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
    | PackerFull
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

instance MonadIO Packer where
    liftIO = liftPacker

liftPacker :: IO a -> Packer a
liftPacker f = Packer $ \cache -> do
    value <- f
    return $ PackerMore value cache

fmapPacker :: (a -> b) -> Packer a -> Packer b
fmapPacker f p = Packer $ \cache -> do
    rv <- runPacker_ p cache
    return $ case rv of
        PackerMore v cache' -> PackerMore (f v) cache'
        PackerFull          -> PackerFull
        PackerFail err      -> PackerFail err

returnPacker :: a -> Packer a
returnPacker v = Packer $ \cache -> return $ PackerMore v cache

bindPacker :: Packer a -> (a -> Packer b) -> Packer b
bindPacker p fp = Packer $ \cache -> do
    rv <- runPacker_ p cache
    case rv of
        PackerMore v cache' -> runPacker_ (fp v) cache'
        PackerFull          -> return $ PackerFull
        PackerFail err      -> return $ PackerFail err

appendPacker :: Packer (a -> b) -> Packer a -> Packer b
appendPacker p1f p2 = p1f >>= \p1 -> p2 >>= \v -> return (p1 v)

pack :: Packer a -> ByteString -> IO (Result a)
pack p (PS fptr off max) =
    withForeignPtr fptr $ \ptr ->
        runPacker_ p (Cache (ptr `plusPtr` off) (max - off))

-- run a sized action
actionPacker :: Int -> (Ptr Word8 -> IO a) -> Packer a
actionPacker s action = Packer $ \(Cache ptr size) -> do
    case compare size s of
        LT -> return $ PackerFail "Not enough space in destination"
        EQ -> do
            v <- action ptr
            return PackerFull
        GT -> do
            v <- action ptr
            return $ PackerMore v (Cache (ptr `plusPtr` s) (size - s))

-- | put a storable from the current position in the stream
putStorable :: Storable storable => storable -> Packer ()
putStorable s = actionPacker (sizeOf s) (\ptr -> poke (castPtr ptr) s)

-- | put a Bytestring from the current position in the stream
--
-- If the ByteString ins null, then do nothing
putByteString :: ByteString -> Packer ()
putByteString = putListOfStorable . B.unpack

-- | put a list of Storable
putListOfStorable :: Storable a => [a] -> Packer ()
putListOfStorable []     = return ()
putListOfStorable (x:xs) = putStorable x >> putListOfStorable xs

-- | skip some bytes from the current position in the stream
skip :: Int -> Packer ()
skip n = actionPacker n (\_ -> return ())

-- | skip the size of a storable from the current position in the stream
skipStorable :: Storable storable => storable -> Packer ()
skipStorable = skip . sizeOf

-- | fill up from the current position in the stream to then end
fillUpWith :: Storable storable => storable -> Packer ()
fillUpWith s = putListOfStorable $ repeat s
