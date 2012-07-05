-- HexVoyager is a hex dumping tool that (will) embed its own
-- domain specific language (implemented in Haskell) to
-- allow the pretty printing of binary files.

-- Consider it hexdump with extra bling.

-- Provides the command line interface to the hex dump
-- module.
module HexDump.HexDump ( 
    --dump, 
    --load, 
    --grep, 
    --search 
    dumpAsHex
    ) where

import Data.Char
import Data.Word
import Data.Int

import Numeric


import System.IO.MMap

import BitStream.BitStream

import Control.Monad

type Address = Int64

-- padds a string to fill a field of a given length with a given Char
padd::Char->Int->String->String
padd c n s = reverse $ take n $ reverse s ++ repeat c

-- prints an address
printAddress::Address->String
printAddress addr 
    =    "0x" ++ padd '0' 16 (showHex addr "")

-- Simple conversion of a single byte
word8ToHex :: Word8 -> String
word8ToHex w
    = padd '0' 2 (showHex w "")
            
word8ToAscii :: Word8 -> Char
word8ToAscii w 
    = if isPrint c && isAscii c then c else '.' where c = (chr . fromIntegral) w
                  
                  
readBytes::Int->BitStream [Word8]
readBytes i = replicateM i (do 
    e <- isInputEmpty 
    if e 
        then return 0 
        else readBits8 8)              
                     
-- Dumps 16 byte chunks as hex
hexDumpLine :: BitStream String
hexDumpLine = do
    -- addr <- readAddress
    n1 <- readBytes 8
    n2 <- readBytes 8
    let
            toHex w a = word8ToHex w ++ (' ' : a)
            toAscii w a = word8ToAscii w : a
            hex1 = foldr toHex "" n1
            hex2 = foldr toHex "" n2
            ascii = foldr toAscii "" n1 ++ foldr toAscii "" n2   
    return $ "0x????????" ++ "  " ++ hex1 ++ " " ++ hex2 ++ " |" ++ ascii ++ "|"

           

-- Dumps an entire file
dumpAsHex :: FilePath -> Maybe (Address, Int64) -> IO String
dumpAsHex filePath addr = do
            bytes <- mmapFileByteStringLazy filePath loadAddress
            runDump bytes
        where
            loadAddress = case addr of
                            Just (start, size ) -> Just (start, start+size)
                            Nothing -> Nothing
            hexDumpLine' = 
                isInputEmpty >>= 
                    (\e->if e 
                            then return [] 
                            else 
                                hexDumpLine >>= 
                                    (\l -> liftM (l:) hexDumpLine'))
           
            runDump bytes = case runBS hexDumpLine' (createInputString bytes) of
                                Just (r, _) -> return $ unlines r
                                Nothing -> return ""
    
    
  {-      (case (runByteMonad strm start bytes) of
            Just (r,_) -> r
            Nothing -> () ) -}   
            
-- Read a structure from an address in a file
--load :: FilePath -> Address -> ByteMonad a -> IO (Maybe a)
--load fp start strm =
--    let
--        doLoad bytes = 
--            case (runBS strm start bytes) of
--                Just (r,_) -> Just r
--                Nothing -> Nothing
--    in do
--        bytes <- readFileSegment fp start
--        return $ doLoad bytes
        
  
--search::FilePath -> Address -> Int64 -> Int64 -> ByteMonad a -> IO (Maybe (Address,a))
--search fp addr off limit stm =
--    do
--        bytes <- readFileSegment fp addr 
--        return $ searchByteMonad stm addr off limit bytes  
--          
---- Search a file for all occurances of a string
---- TODO: make it a regular expression??
--grep::[Word8]->FilePath->Address->Int64->IO ()
--grep s fp start len = 
--    let
--        runGrep bytes = case runByteMonad (findSubstrings $ BS.pack s) start (B.take len bytes) of
--                            Just (r,_) -> putStr $ unlines $ map printAddress r
--                            Nothing -> print "Unable to run grep"
--    in do 
--        bytes <- readFileSegment fp start
--        runGrep bytes
                      

        