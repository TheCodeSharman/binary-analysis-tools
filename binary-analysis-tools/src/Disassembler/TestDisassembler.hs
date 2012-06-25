module Disassembler.TestDisassembler (
        tests, run 
) where

import Test.HUnit

import Disassembler.Disassembler
import Data.ByteString.Lazy
import BinaryTemplate.ResultTree
import BitStream.BitStream

run = runTestTT tests

decodeInstruction::Data.ByteString.Lazy.ByteString->Disassembler->Maybe ResultTree
decodeInstruction bs d 
                = case decode (createInputString bs) d of
                        Just (a, _) -> Just a
                        Nothing -> Nothing

tests = TestList [
                "check reading empty list" ~:
                                Nothing ~=? decodeInstruction (pack []) intelArchitecture32,
                                                
                "check reading singleton list" ~:
                                Nothing ~=? decodeInstruction (pack [34]) intelArchitecture32,
                                                
                "check invalid instr list" ~:
                                Nothing ~=? decodeInstruction (pack [0xaa,0xaa,00,00]) intelArchitecture32,
                                                
                 tADD
                ]

        
tADD = TestList [
                "add al,-86" ~:
                        Just (
                                ResultTree "ADD" [
                                        ResultTree "imm8"
                                                [Leaf (Number (-86))]
                                        ]) ~=? decodeInstruction (pack [0x04,0xaa]) intelArchitecture32,
                                        
                                        
                "add eax,-86" ~:
                        Just (
                                ResultTree "ADD" [
                                        ResultTree  "imm32" 
                                                [Leaf (Number (-86))]
                                        ]) ~=? decodeInstruction (pack [0x05,0xaa,0xff,0xff,0xff]) intelArchitecture32,
                                        
                "add dword [edx],-86" ~: 
                        Just (
                                ResultTree"ADD" [
                                        ResultTree "mod" [Leaf (Number (0))],
                                        ResultTree  "rm"  [Leaf (Number (2))],
                                        ResultTree  "imm32" [Leaf (Number (-86))]
                                        ]) ~=? decodeInstruction (pack [0x81,0x2,0xaa,0xff,0xff,0xff]) intelArchitecture32,
                                        
                "add dword [ eax + 2*ebx + 0x12345678 ],-86" ~: 
                        Just (
                                ResultTree  "ADD" [
                                        ResultTree  "mod" [Leaf (Number (2))],
                                        ResultTree  "rm"  [Leaf (Number (4))],
                                        ResultTree  "scale"  [Leaf (Number (1))],
                                        ResultTree  "index"  [Leaf (Number (3))],
                                        ResultTree  "base"  [Leaf (Number (0))],
                                        ResultTree  "disp32"  [Leaf (Number (0x12345678))],
                                        ResultTree  "imm32" [Leaf (Number (-86))]
                                        ]) ~=? decodeInstruction (pack [0x81,0x84,0x58,0x78,0x56,0x34,0x12,0xaa,0xff,0xff,0xff]) intelArchitecture32
                                        
        ]