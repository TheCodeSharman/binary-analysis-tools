
module ParseMachineDescription where

import MachineDescription 

import CompileBinaryTemplate hiding ( tests )
import ParseBinaryTemplate hiding ( tests )
import BinaryTemplate ( BinaryTemplate)
import qualified BinaryTemplate as BT

import Text.XML.HXT.Arrow
import Text.XML.HXT.Arrow.Pickle

instance XmlPickler Register where
        xpickle = xpAlt ( \r -> case r of
                                                                RegDef _ _ -> 0
                                                                RegAlias _ _ _ _ -> 1 )
                          [
                                  xpElem "md:register" $
                                  xpWrap ( \ (n, b) -> RegDef { regName = n, regBits = b },
                                                   \ r -> ( regName r, regBits r ) ) $
                                  xpPair (xpAttr "name" xpText) (xpAttr "size" xpPrim),
                                  
                                  xpElem "md:registerAlias" $
                                  xpWrap ( \ ( n, r, s, e) ->
                                                          RegAlias {
                                                                  regAliasName = n,
                                                                  regRef = r,
                                                                  regFirstBit = s,
                                                                  regEndBit = e },
                                                   \ r -> ( regAliasName r,
                                                                     regRef r,
                                                                     regFirstBit r,
                                                                     regEndBit r ) ) $
                                  xp4Tuple
                                          (xpAttr "name" xpText)
                                          (xpAttr "registerRef" xpText)
                                          (xpAttr "firstBit" xpPrim)
                                          (xpAttr "lastBit" xpPrim)
                          ]
                          
instance XmlPickler Flag where 
        xpickle = xpElem "md:flag" $
                          xpWrap ( \ (n, r, s, e) -> Flag { flagName = n, flagRegRef = r, flagFirstBit = s, flagEndBit = e },
                                             \ f -> ( flagName f, flagRegRef f, flagFirstBit f, flagEndBit f ) ) $
                          xp4Tuple (xpAttr "name" xpText) (xpAttr "registerRef" xpText)
                                             (xpAttr "firstBit" xpPrim) (xpAttr "lastBit" xpPrim)

instance XmlPickler Opcode where
        xpickle = 
                xpWrap ( \ (n, d) -> Opcode { opcodeName = n, decoder = d },
                         \ o -> ( opcodeName o, decoder o ) ) $
                xpPair (xpAttr "name" xpText) xpBinaryTemplate
                          
instance XmlPickler Instructions where
        xpickle = 
                xpWrap ( \ (g,o) -> Instructions { globals = g, opcodes = o },
                                 \ i -> (globals i, opcodes i) ) $
                xpPair (xpElem "md:globalTemplates" $ xpList xpBinaryTemplate) (xpList (xpElem "md:opcodeTemplate" $ xpickle))
                          
instance XmlPickler MachineDesc where
        xpickle = 
                xpElem "md:instructionSetArchitecture" $
                xpWrap ( \(n, r, f, o) -> MachineDesc { name = n, registers = r, flags =f, instructions = o },
                           \ md -> ( name md, registers md, flags md, instructions md ) ) $
                xp4Tuple (xpAttr "name" xpText) 
                                 (xpElem "md:registers" $ xpList xpickle ) 
                                 (xpElem "md:flags" $ xpList xpickle )
                                 (xpElem "md:instructions" $ xpickle )
                                 
xpMachineDesc::PU MachineDesc
xpMachineDesc = xpickle
                        
-- the namespaces used in the document                  
namespaces :: NsEnv
namespaces = [ ("md", "http://twistedchimney.net/CPU") ]        
                                 
loadMachineDesc :: String -> IO MachineDesc
loadMachineDesc filename
  = do
    [doc] <- runX ( 
                                    readDocument [ (a_remove_whitespace, v_1), (a_validate, v_0) ] filename
                                    >>> attachNsEnv namespaces
                                    >>> propagateNamespaces
                                    >>> uniqueNamespaces
                                    >>> xunpickleVal xpMachineDesc
                  )
    return doc


