module Disassembler.Disassembler (
                decode, decodeList,
                Disassembler,
                intelArchitecture32,
                module BitStream.BitStream
                )
where

import BinaryTemplate.ResultTree
import BinaryTemplate.Evaluate
import BinaryTemplate.Parse
import BinaryTemplate.AbstractSyntax

import BitStream.BitStream
import qualified Data.ByteString.Lazy

import System.IO.Unsafe( unsafePerformIO )

data Disassembler = Disassembler {
                                                template::BinaryTemplate,
                                                decoder::CompiledBinTemp
                                        }

{-# NOINLINE intelArchitecture32 #-}                
intelArchitecture32::Disassembler                                
intelArchitecture32 
        = unsafePerformIO $
                do
                        bt <- loadBinaryTemplateFromFile "/Users/msharman/Documents/workspace/machineDesc/IA-32_opcodes.xml"
                        return Disassembler { template = head bt, decoder = compile (head bt) }


-- decode a single instruction from the input                
decode::InputString->Disassembler->Maybe (ResultTree, InputString)
decode is ds
        = execute (decoder ds) is
        
-- decode as many instructions as possible from an input string
decodeList::InputString->Disassembler->([ResultTree], InputString)
decodeList is ds
        = case decode is ds of
                Just (r, os) ->
                        let
                                (rs, os2) = decodeList os ds
                        in
                                (r : rs, os2)
                Nothing -> ([], is)
                
-- decode a basic block
-- decode into many basic blocks
