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
import qualified Data.ByteString as B
import Data.ByteString.Pack
import Data.Char

------------------------------------------------------------------------------
-- Simple packers to test                                                   --
------------------------------------------------------------------------------

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

-- Thank you wikipedia
longByteString :: ByteString
longByteString =
    "Evolution is the change in the inherited characteristics of biological populations over successive generations. Evolutionary processes give rise to diversity at every level of biological organisation, including species, individual organisms and molecules such as DNA and proteins.[1] All life on Earth is descended from a last universal ancestor that lived approximately 3.8 billion years ago. Repeated speciation and the divergence of life can be inferred from shared sets of biochemical and morphological traits, or by shared DNA sequences.[2] These homologous traits and sequences are more similar among species that share a more recent common ancestor, and can be used to reconstruct evolutionary histories, using both existing species and the fossil record. Existing patterns of biodiversity have been shaped both by speciation and by extinction.[3]"

refTestsOk = testGroup "All these tests must always pass"
    [ testCaseOk "put a byte"    "B"  1  (putWord8 $ fromChar 'B')
    , testCaseOk "write string"  "Haskell rocks!" 42 (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    , testCaseOk "put a 2 bytes" "XY" 2 (putWord16 0x5958)
    , testCaseOk "write long stuff" longByteString (B.length longByteString) (putByteString longByteString)
    ]

refTestsFail = testGroup "Try to see that pack fails properly"
    [ testCaseFail "not enough space"            1   (putWord32 42)
    , testCaseFail "3 actions, enough for 1"     7   (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    , testCaseFail "3 actions, enough for 2"     8   (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    , testCaseFail "3 actions, enough for 2 bis" 9   (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    , testCaseFail "write a too long bytestirng" 124 (putByteString longByteString)
    ]

tests = testGroup "bspack test suit"
    [ refTestsOk
    , refTestsFail
    ]

main = defaultMain tests
