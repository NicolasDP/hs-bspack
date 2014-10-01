-- |
-- Module      : Data.ByteString.Pack.Internal
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 Nicolas DI PRIMA
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ByteString.Pack.Internal
    ( Result(..)
    , Packer(..)
    , Cache(..)
    , actionPacker
    ) where

import Control.Applicative
import Data.Word
import Foreign.Ptr

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
{-# INLINE fmapPacker #-}

returnPacker :: a -> Packer a
returnPacker v = Packer $ \cache -> return $ PackerMore v cache
{-# INLINE returnPacker #-}

bindPacker :: Packer a -> (a -> Packer b) -> Packer b
bindPacker p fp = Packer $ \cache -> do
    rv <- runPacker_ p cache
    case rv of
        PackerMore v cache' -> runPacker_ (fp v) cache'
        PackerFail err      -> return $ PackerFail err
{-# INLINE bindPacker #-}

appendPacker :: Packer (a -> b) -> Packer a -> Packer b
appendPacker p1f p2 = p1f >>= \p1 -> p2 >>= \v -> return (p1 v)
{-# INLINE appendPacker #-}

-- run a sized action
actionPacker :: Int -> (Ptr Word8 -> IO a) -> Packer a
actionPacker s action = Packer $ \(Cache ptr size) ->
    case compare size s of
        LT -> return $ PackerFail "Not enough space in destination"
        _  -> do
            v <- action ptr
            return $ PackerMore v (Cache (ptr `plusPtr` s) (size - s))
{-# INLINE actionPacker #-}
