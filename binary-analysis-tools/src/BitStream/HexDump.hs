module BitStream.HexDump where


import Data.Char
import Data.Word
import Data.Int

import Numeric

import BitStream.BitStream

import Control.Monad
import qualified Data.ByteString as BS

-- Address are intended to reference logical memory addresses
newtype Address = MkAddress Int64
toAddress :: Int -> Address
toAddress = MkAddress . fromIntegral

fromAddress :: Address -> Int64
fromAddress (MkAddress i) = i

instance Show Address where
    show = printAddress


-- padds a string to fill a field of a given length with a given Char
padd::Char->Int->String->String
padd c n s = reverse $ take n $ reverse s ++ repeat c

-- prints an address
printAddress::Address->String
printAddress addr 
    =    "0x" ++ padd '0' 16 ((showHex.fromAddress) addr "")



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
            --toAscii w a = word8ToAscii w : a
            hex1 = foldr toHex "" n1
            hex2 = foldr toHex "" n2
            --ascii = foldr toAscii "" n1 ++ foldr toAscii "" n2   
    return $ hex1 ++ " " ++ hex2

-- Dumps a byte string
hexRenderer :: BS.ByteString -> String
hexRenderer bytes = 
    case runBS hexDumpLine' (createInputString bytes) of
                        Just (r, _) ->  unlines r
                        Nothing -> ""
        where
            hexDumpLine' = 
                isInputEmpty >>= 
                    (\e->if e 
                            then return [] 
                            else 
                                hexDumpLine >>= 
                                    (\l -> liftM (l:) hexDumpLine'))
    
