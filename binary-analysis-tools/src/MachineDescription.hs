
module MachineDescription ( 
                        MachineDesc ( MachineDesc ), 
                        name, registers,flags, instructions,
                        
                        Instructions ( Instructions ),
                        globals, opcodes,
                        
                        Register ( RegDef, RegAlias ), 
                        regName, regAliasName, regRef, regBits,
                        regFirstBit, regEndBit,
                        
                        Flag ( Flag ),
                        flagName, flagRegRef, flagFirstBit, flagEndBit,
                        
                        Opcode ( Opcode ),
                        opcodeName, decoder,
                        
                        BinaryReader ) where

import BinaryTemplate ( BinaryTemplate )
import CompileBinaryTemplate ( BinaryReader )

-- Characterises a machine specification in a parsed
-- form.
data MachineDesc
        = MachineDesc {
                name::String,
                registers::[Register],
                flags::[Flag],
                instructions::Instructions
          } 
         deriving (Eq, Show)
          
data Flag = Flag { 
                                flagName::String,
                                flagRegRef::String,
                                flagFirstBit::Int,
                                flagEndBit::Int 
                        }
                         deriving (Eq, Show)

data Register 
        = RegDef { regName::String, regBits::Int }
          | RegAlias { regAliasName::String, regRef::String, regFirstBit::Int, regEndBit::Int }
           deriving (Eq, Show)
          
data Instructions 
        = Instructions {
                globals::[BinaryTemplate],
                opcodes::[Opcode]
         }  deriving (Eq, Show)
         
data Opcode 
        = Opcode {
                opcodeName::String,
                decoder::BinaryTemplate
          }  deriving (Eq, Show)


                 
                        