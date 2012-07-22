
module BitStream.BitStream ( 
    BitStream, littleEndian, bigEndian, createInputString, InputString,
    runBS, execBS, getBits, convertUnsignedToSigned, readBits8,
    expect, readEmpty, alternatives, alternate, 
    readASCIIString, readUnsignedInteger, readSignedInteger, isInputEmpty
  )  where

import Data.ByteString
import Control.Monad.State
import Data.Bits
import Data.Word

import Prelude hiding ( head, drop, null )

type InputString = (Int, Word8, ByteString)
createInputString::ByteString->InputString
createInputString bs = (8,0,bs)

-- Define a state monad that allows reading of bits from a ByteStream
-- the Int is an internal pointer to the bit number in the Word8 to start
-- reading from. The Word8 is the next byte off the byte stream buffer.
type BitStream a = StateT InputString Maybe a
runBS::BitStream a -> InputString -> Maybe ( a, InputString )

-- different ways building a word
type WordBuilder = [Word8]->Integer
bigEndian::WordBuilder
bigEndian = Prelude.foldr (\ w8 w32 -> w32 * 256 + fromIntegral w8) 0

littleEndian::WordBuilder
littleEndian w8s = Prelude.foldr (\ w8 w32 -> w32 * 256 + fromIntegral w8) 0 $ Prelude.reverse w8s 

-- Note the initial state where the bit pointer points to the 9th bit
runBS = runStateT  

-- lifts out the value and ignores the state
execBS::BitStream a->ByteString->Maybe a
execBS m bs = case runBS m (createInputString bs) of
                    Just ( a, _ ) -> Just a
                    Nothing -> Nothing


-- Retrieves a slice of bits from to Word8 values
getBits::Int->Int->Word8->Word8->Word8
getBits start bits word1 word2
    =    let
            buffer::Word16 
            buffer =  (fromIntegral word1 * 256) + fromIntegral word2
            mask::Word16 
            mask = (shiftL 1 bits - 1)
        in
            fromIntegral (rotateL buffer (start + bits) .&. mask)
    
-- Converts an unsigned Integer to a 2's complement signed integer        
convertUnsignedToSigned::Int->Integer->Integer
convertUnsignedToSigned width i
    = if testBit i (width - 1)
         then -((i `xor` (bit width - 1)) + 1)
         else i -- it is positive

-- read less than 8 bits from a BitStream
readBits8::Int->BitStream Word8
readBits8 bits = do
    -- Retrieve the current state
    (bitOffset, bufferWord8, bstr ) <- get
    
    -- Fail if the BitStream doesn't have enough bits
    let needToReadBeyondBuffer = (bitOffset + bits) > 8
    let emptyBitstream = null bstr && needToReadBeyondBuffer
    when emptyBitstream $ fail "empty bitstream"     

    -- Return the requested bits, reading the next byte if needed
    let ( nBstr, nWord8, nOff ) = if needToReadBeyondBuffer
            then ( drop 1 bstr, head bstr, bitOffset + bits - 8 )
            else ( bstr, bufferWord8, bitOffset + bits )              
    put (nOff, nWord8, nBstr)
    return (getBits bitOffset bits bufferWord8 nWord8) 
                            
-- Reads an empty                 
readEmpty::BitStream ()                    
readEmpty = return ()
                    
-- Reads a specified number of bits returning a list of 8 bit words        
readWords::Int->BitStream [Word8]
readWords bits
        | bits <= 8 
            = readBits8 bits >>= \a -> return [a]
        | otherwise 
            = readWords (bits - 8) 
                    >>= \a -> readBits8 8 
                    >>= \b -> return (b:a)                
                    
-- read an arbitrary number of bits as an unsigned Integer        
readUnsignedInteger::WordBuilder->Int->BitStream Integer
readUnsignedInteger wb bits = liftM wb (readWords bits)
          
-- Read an arbitrary number of bits as a signed Integer
readSignedInteger::WordBuilder->Int->BitStream Integer
readSignedInteger wb bits
    = liftM (convertUnsignedToSigned bits) (readUnsignedInteger wb bits)

-- Read an ASCII string        
readASCIIString::Int->BitStream String
readASCIIString bits
    = if bits `mod` 8 == 0
        then liftM word8ToString $ replicateM (bits `div` 8) (readBits8 8)
        else fail "length of string must be divisble by 8"
      where
           word8ToString = Prelude.map ( toEnum . fromEnum )
           
-- Given a value and a bitstream, expect this value to
-- match what is read    
expect::Eq a=>a->BitStream a->BitStream ()
expect ev rdr = rdr >>= guard . (ev==) 
    
-- Given a list of alternatives tries them one by
-- one until a result is returned
alternatives::[BitStream a]->BitStream a
alternatives = msum

-- Choose between x or y depending on which is matchable
alternate::BitStream a->BitStream a->BitStream a
alternate = mplus

-- Is the stream empty?
isInputEmpty::BitStream Bool
isInputEmpty = do
            (_,_,bs)<-get
            return $ null bs