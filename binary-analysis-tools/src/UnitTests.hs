
module UnitTests where

import Test.HUnit

import qualified BinaryTemplate.TestParse
import qualified BitStream.TestBitStream
import qualified BinaryTemplate.TestEvaluate
import qualified Disassembler.TestDisassembler

run = runTestTT allTests
                                
allTests = TestList [ 
                                BitStream.TestBitStream.tests,
                                BinaryTemplate.TestParse.tests,
                                BinaryTemplate.TestEvaluate.tests, 
                                Disassembler.TestDisassembler.tests
                        ]