-- |
-- Module      : Tests.Tests.hs
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 Nicolas DI PRIMA
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString (ByteString)
import Data.ByteString.Pack
import Data.Word
import Data.Char

------------------------------------------------------------------------------
-- Simple packers to test                                                   --
------------------------------------------------------------------------------

putWord8 :: Word8 -> Packer ()
putWord8 = putStorable

putWord16 :: Word16 -> Packer ()
putWord16 = putStorable

putWord32 :: Word32 -> Packer ()
putWord32 = putStorable

testPackerOk :: ByteString -> Int -> Packer () -> Assertion
testPackerOk expected size packer =
    case pack packer size of
        Left err     -> assertFailure err
        Right result -> assertEqual ("the two given value should be equal: Test(" ++ (show expected) ++ ")")
                                    expected result

testPackerFail :: Int -> Packer () -> Assertion
testPackerFail size packer =
    case pack packer size of
        Left   _     -> return ()
        Right result -> assertFailure $ "The given test should not pass: " ++ (show result)

testCaseOk :: String -> ByteString -> Int -> Packer () -> TestTree
testCaseOk msg expected size packer = testCase msg (testPackerOk expected size packer)

testCaseFail :: String -> Int -> Packer () -> TestTree
testCaseFail msg size packer = testCase msg (testPackerFail size packer)

------------------------------------------------------------------------------
-- Group of tests
------------------------------------------------------------------------------

fromChar :: (Num a) => Char -> a
fromChar = fromIntegral . ord

refTestsOk = testGroup "All these tests must always pass"
    [ testCaseOk "put a byte"    "B"  1  (putWord8 $ fromChar 'B')
    , testCaseOk "write string"  "Haskell rocks!" 42 (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    , testCaseOk "put a 2 bytes" "XY" 2 (putWord16 0x5958)
    ]

refTestsFail = testGroup "Try to see that pack fails properly"
    [ testCaseFail "not enough space" 1 (putWord32 42)
    , testCaseFail "3 actions, enough for 1"     7 (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    , testCaseFail "3 actions, enough for 2"     8 (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    , testCaseFail "3 actions, enough for 2 bis" 9 (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    ]

tests = testGroup "bspack test suit"
    [ refTestsOk
    , refTestsFail
    ]

main = defaultMain tests
