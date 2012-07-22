module BinaryEditor.BinaryEditor where

import Data.Int
import qualified Data.ByteString as BS

import BitStream.HexDump

-- The general idea is have a class BinaryFile
-- that allows rendering of different types of
-- content. 
--
-- For instance UnkownLoader loads with hex sections
-- PELoader loads PE files
-- ELFLoader loads ELF files etc.
class BinaryFile a where
    loadBinary     ::  String -> IO a
    saveBinary     ::  String -> a -> IO ()
    getBinaryBytes  :: a -> BS.ByteString
    getBinarySections    ::  a -> [Section]

-- A comment reference
data Comment = MkComment
    Int64              -- Offset into file
    String             -- Text of comment

-- A section of a binary file
data Section =  MkSection 
    (Maybe String)   -- Optional name for section
    Int64             -- Byte offset into file
    Address           -- Start address
    Address           -- End address

sectionGetBytes ::BinaryFile a => Section -> a -> BS.ByteString
sectionGetBytes section binary =
    BS.take (sectionGetLength section) (BS.drop (sectionGetOffset section) bytes)
        where
            bytes = getBinaryBytes binary
            
sectionGetOffset :: Section -> Int
sectionGetOffset (MkSection _ offset _ _ ) = fromIntegral offset

sectionGetStartAddress :: Section -> Address
sectionGetStartAddress (MkSection _ _ start _ ) = start

sectionGetEndAddress :: Section -> Address
sectionGetEndAddress (MkSection _ _ end _ ) = end

sectionGetLength :: Section -> Int
sectionGetLength (MkSection _ _ start end ) = fromIntegral $ fromAddress end - fromAddress start
