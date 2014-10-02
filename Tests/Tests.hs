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

longBase32 :: ByteString
longBase32 = "JBQXG23FNRWCA2LTEBQW4IDBMR3GC3TDMVSCA4DVOJSWY6JNMZ2W4Y3UNFXW4YLMEBYHE33HOJQW23LJNZTSA3DBNZTXKYLHMUXCAQLOEBXXAZLOFVZW65LSMNSSA4DSN5SHKY3UEBXWMIDNN5ZGKIDUNBQW4IDUO5SW45DZEB4WKYLSOMQG6ZRAMN2XI5DJNZTS2ZLEM5SSA4TFONSWC4TDNAWCA2LUEBQWY3DPO5ZSA4TBOBUWIIDEMV3GK3DPOBWWK3TUEBXWMIDSN5RHK43UFQQGG33OMNUXGZJMEBRW64TSMVRXIIDTN5THI53BOJSS4ICXNF2GQIDTORZG63THEBZXK4DQN5ZHIIDGN5ZCA2LOORSWO4TBORUW63RAO5UXI2BAN52GQZLSEBWGC3THOVQWOZLTFQQGE5LJNR2C22LOEBRW63TDOVZHEZLOMN4SAYLOMQQHAYLSMFWGYZLMNFZW2LBAMRSWE5LHM5SXE4ZMEBYHE33GNFWGK4TTFQQHE2LDNAQGY2LCOJQXE2LFOMQGC3TEEBQW4IDBMN2GS5TFEBRW63LNOVXGS5DZFQQEQYLTNNSWY3BANVQWWZLTEBUXIIDFMFZWSZLSEB2G6IDQOJXWI5LDMUQGM3DFPBUWE3DFFQQG2YLJNZ2GC2LOMFRGYZJMEBUGSZ3IFVYXKYLMNF2HSIDTN5THI53BOJSS4==="

longBase32WithoutPadding :: ByteString
longBase32WithoutPadding = B.take (B.length longBase32 - 3) longBase32

longForBase32 :: ByteString
longForBase32 = "Haskell is an advanced purely-functional programming language. An open-source product of more than twenty years of cutting-edge research, it allows rapid development of robust, concise, correct software. With strong support for integration with other languages, built-in concurrency and parallelism, debuggers, profilers, rich libraries and an active community, Haskell makes it easier to produce flexible, maintainable, high-quality software."

refTestsOk = testGroup "All these tests must always pass"
    [ testCaseOk "put a byte"    "B"  1  (putWord8 $ fromChar 'B')
    , testCaseOk "write string"  "Haskell rocks!" 42 (putByteString "Haskell" >> putWord8 0x20 >> putByteString "rocks!")
    , testCaseOk "put a 2 bytes" "XY" 2 (putWord16 0x5958)
    , testCaseOk "write long stuff" longByteString (B.length longByteString) (putByteString longByteString)
    , testCaseOk "write base 32 short" "JBQXG23FNRWA====" 20 (putByteStringBase32 True "Haskell")
    , testCaseOk "write base 32 long" longBase32 (B.length longBase32) (putByteStringBase32 True longForBase32)

    , testCaseOk "write base 32 short without padding" "JBQXG23FNRWA" 16 (putByteStringBase32 False "Haskell")
    , testCaseOk "write base 32 long without padding" longBase32WithoutPadding
                                                      (guessEncodedLength $ B.length longForBase32)
                                                      (putByteStringBase32 False longForBase32)
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
