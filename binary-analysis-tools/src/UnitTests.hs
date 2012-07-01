
module UnitTests where

import Test.HUnit

import qualified BinaryTemplate.TestParse
import qualified BitStream.TestBitStream
import qualified BinaryTemplate.TestEvaluate
import qualified Disassembler.TestDisassembler

main :: IO Counts
main = runTestTT allTests
                                
allTests :: Test
allTests = TestList [ 
            BitStream.TestBitStream.tests,
            BinaryTemplate.TestParse.tests,
            BinaryTemplate.TestEvaluate.tests, 
            Disassembler.TestDisassembler.tests
 ]