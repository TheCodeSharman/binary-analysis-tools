module BitStream.TestBitStream (
    tests, run
) where

import BitStream.BitStream
import Data.ByteString.Lazy
import Test.HUnit

run :: IO Counts
run = runTestTT tests

-- 10011110 10111100 11110100 11000010 11110111
testBitStream::ByteString
testBitStream = pack [158,188,244,194,247]
testBitStream2 :: ByteString
testBitStream2 = pack [44,32,19,21]
testBitStream3 :: ByteString
testBitStream3 = pack [32,21,66]

altstest :: BitStream (Integer, Integer)
altstest = (do
                expect 32 (readUnsignedInteger bigEndian 8)
                x <- readUnsignedInteger bigEndian 8
                expect 66 (readUnsignedInteger bigEndian 8)
                return (x,14) )
            `alternate` 
            (do
                expect 44 (readUnsignedInteger bigEndian 8)
                x <- readUnsignedInteger bigEndian 8
                y <- readUnsignedInteger bigEndian 8
                expect 21 (readUnsignedInteger bigEndian 8)
                return (x,y) )
                
alternativesTest = [ 
                    do
                        expect 32 (readUnsignedInteger bigEndian 8)
                        x <- readUnsignedInteger bigEndian 8
                        expect 66 (readUnsignedInteger bigEndian 8)
                        return (x,14),
                        
                    do
                        expect 58 (readUnsignedInteger bigEndian 8)
                        x <- readUnsignedInteger bigEndian 8
                        expect 23 (readUnsignedInteger bigEndian 8)
                        return (x,14), 
        
                    do
                        expect 44 (readUnsignedInteger bigEndian 8)
                        x <- readUnsignedInteger bigEndian 8
                        y <- readUnsignedInteger bigEndian 8
                        expect 21 (readUnsignedInteger bigEndian 8)
                        return (x,y)
                ]
                        
runBS2::BitStream a -> ByteString -> Maybe (a, InputString)
runBS2 bs is
    = runBS bs (createInputString is) 
                
tests = TestList [
    "little endian wordbuilder" ~: 0x44321922 ~=? littleEndian [0x44,0x32, 0x19, 0x22],
    
    "big endian wordbuilder" ~: 0x22193244 ~=? bigEndian [0x44,0x32, 0x19, 0x22],

    "extract 4" ~: 9 ~=? getBits 0 4 158 0,
    
    "extract 6" ~: 39 ~=? getBits 0 6 158 188,
    
    "extract 2nd 6" ~: 43 ~=? getBits 6 6 158 188,
    
    "test 255 as signed" ~: -1 ~=? convertUnsignedToSigned 8 255,

    "simple read" ~: (9,14) ~=? 
        let 
            res = runBS2 ( 
                    readBits8 4 
                        >>= \x -> readBits8 4 
                        >>= \y -> return (x,y)
                    ) testBitStream
        in
            case (res) of
                    Just ( a, _ ) -> a
                    Nothing -> (-1,-1),
                    
    "simple read triplet in 8bits" ~: Just (3,7,7) ~=? 
            execBS (
                    do
                        x <- readBits8 2 
                        y <- readBits8 3 
                        z <- readBits8 3
                        return (x,y,z)
                    ) (pack [255]),

    "overlapping read" ~: (39,43) ~=? 
        let 
            res = runBS2 (
                    readBits8 6 
                        >>= \x -> readBits8 6 
                        >>= \y -> return (x,y)
                    ) testBitStream
        in
            case (res) of
                    Just ( a, _ ) -> a
                    Nothing -> (-1,-1),
                    
    "32bit unsigned read" ~: 0x44321921 ~=? 
         let 
            res = runBS2 (
                    readUnsignedInteger bigEndian 32
                        >>= \a -> return a
                    ) (pack [0x44, 0x32, 0x19, 0x21])
        in
            case (res) of
                    Just ( a, _ ) -> a
                    Nothing -> -1,
                    
    "large unsigned read" ~: 85221873758 ~=? 
        
        let 
            res = runBS2 (
                    readUnsignedInteger bigEndian 37 
                        >>= \a -> return a
                    ) testBitStream
        in
            case (res) of
                    Just ( a, _ ) -> a
                    Nothing -> -1,
                                    
    "large signed read" ~: -52217079714 ~=? 
        let 
            res = runBS2 (
                    readSignedInteger bigEndian 37 
                        >>= \a -> return a
                    ) testBitStream
        in
            case (res) of
                    Just ( a, _ ) -> a
                    Nothing -> -1,
        
    "test alternate 1" ~:
        Just (32,19) ~=? execBS altstest testBitStream2,
        
    "test alternate 2" ~:
        Just (21,14) ~=? execBS altstest testBitStream3,
        
     "test for expect" ~:
         Just () ~=? execBS (expect 158 (readUnsignedInteger bigEndian 8)) testBitStream,
         
     "test for expect fail" ~:
         Nothing ~=? execBS (expect 44 (readUnsignedInteger bigEndian 8)) testBitStream,
         
     "test for readString" ~:
         Just "TestString" ~=? execBS (readASCIIString 80) (pack (Prelude.map (fromIntegral . fromEnum) "TestString")),
     
     "test for alternatives" ~:
         Just (32,19) ~=? execBS (alternatives alternativesTest) testBitStream2,
         
     "read empty" ~:
         Just () ~=? execBS (readEmpty) testBitStream2,
         
     "empty string read" ~:
          Nothing ~=? execBS (readUnsignedInteger bigEndian 8) (pack []),
          
     "read 8 bits" ~:
         Just (255) ~=? execBS (readUnsignedInteger bigEndian 8) (pack [255]),
         
     "read 2,3,3 bits out of 8" ~:
         Just (3,7,7) ~=? execBS (do
                                 x <- readUnsignedInteger bigEndian 2
                                 y <- readUnsignedInteger bigEndian 3
                                 z <- readUnsignedInteger bigEndian 3
                                 return (x,y,z)
                                 ) (pack [255])
     
    ]
