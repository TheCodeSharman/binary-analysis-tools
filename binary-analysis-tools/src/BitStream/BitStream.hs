
module BitStream.BitStream ( 
	BitStream, littleEndian, bigEndian, createInputString, InputString,
	runBS, execBS, getBits, convertUnsignedToSigned, readBits8,
	expect, readEmpty, alternatives, alternate, 
	readASCIIString, readUnsignedInteger, readSignedInteger
  )  where

import Data.ByteString.Lazy
import Control.Monad.State
import Data.Bits
import Data.Word
import Data.Int

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
bigEndian w8s = Prelude.foldr (\w8 -> \w32 -> w32*256 + fromIntegral w8) 0 w8s

littleEndian::WordBuilder
littleEndian w8s = Prelude.foldr (\w8 -> \w32 -> w32*256 + fromIntegral w8) 0 (Prelude.reverse w8s) 

-- Note the initial state where the bit pointer points to the 9th bit
runBS m is = runStateT m is  

-- lifts out the value and ignores the state
execBS::BitStream a->ByteString->Maybe a
execBS m bs = case runBS m (createInputString bs) of
					Just ( a, _ ) -> Just a
					Nothing -> Nothing


-- Retrieves a slice of bits from to Word8 values
getBits::Int->Int->Word8->Word8->Word8
getBits start bits word1 word2
	=	let
			buffer::Word16 
			buffer =  ((fromIntegral word1) * 256) + (fromIntegral word2)
			mask::Word16 
			mask = ((shiftL 1 bits) - 1)
		in
			fromIntegral ((rotateL buffer (start + bits) ) .&. mask)
	
-- Converts an unsigned Integer to a 2's complement signed integer		
convertUnsignedToSigned::Int->Integer->Integer
convertUnsignedToSigned width i
	= case (testBit i (width-1)) of
		True -> -((i `xor` ((bit width)-1))+1) -- 2's complement signed integer
		False -> i -- it is positive

-- read less than 8 bits from a BitStream
readBits8::Int->BitStream Word8
readBits8 bits = do
					(bitOffset, bufferWord8, bstr ) <- get
					
					case Data.ByteString.Lazy.null bstr of
						-- No more bytes left in the bytestring but we still need
						-- to keep returning bits until the bitOffset is out of range
						True ->	if (bitOffset + bits) > 8
								then fail "empty bitstream"
								else
									do
										put ( bitOffset + bits, bufferWord8, bstr )
										return (getBits bitOffset bits bufferWord8 0) 
							
						False -> let
										(newBufferWord, newBitOffset, newBstr) 
											= case ( (bitOffset + bits) > 8 ) of
												True -> ( Data.ByteString.Lazy.head bstr, 
														  bitOffset + bits - 8, 
														  Data.ByteString.Lazy.drop 1 bstr)
												False -> ( bufferWord8, bitOffset + bits, bstr )
								 in 
								 	do
										put (newBitOffset, newBufferWord, newBstr)
										return (getBits bitOffset bits bufferWord8 newBufferWord)
					
readEmpty::BitStream ()					
readEmpty = do
				return ()
				
		
-- reads a specified number of bits returning a list of 8 bit words		
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
readUnsignedInteger wb bits
	= 	do
	  		w8s <- readWords bits
	  		return (wb w8s)
	  	
-- read an arbitrary number of bits as a signed Integer
readSignedInteger::WordBuilder->Int->BitStream Integer
readSignedInteger wb bits
	= do
		i <- readUnsignedInteger wb bits
		return (convertUnsignedToSigned bits i)
		
readASCIIString::Int->BitStream String
readASCIIString bits
	= let
		word8ToString::[Word8]->String
		word8ToString = Prelude.map (\x->toEnum $ fromEnum x)
	  in
		case (bits `mod` 8) of
			0 -> do
					str <- sequence (Prelude.take (bits `div` 8) (Prelude.repeat (readBits8 8)))
					return (word8ToString str)
			_ -> fail "length of string must be divisble by 8"
	
-- given a value and a bitstream, expect this value to
-- match what is read	
expect::(Eq a,Show a)=>a->BitStream a->BitStream ()
expect ev rdr
	= do
		v <- rdr
		case v == ev of
			True -> return ()
			False -> fail "value not found"
	
-- given a list of alternatives tries them one by
-- one until a result is returned
alternatives::[BitStream a]->BitStream a
alternatives [x] = x
alternatives (x : xs) = alternate x (alternatives xs)

-- choose between x or y depending on which is matchable
alternate::BitStream a->BitStream a->BitStream a
alternate x y = x `mplus` y