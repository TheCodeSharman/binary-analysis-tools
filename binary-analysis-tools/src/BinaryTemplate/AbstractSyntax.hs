
module BinaryTemplate.AbstractSyntax where
                
data Endianess =   Little | Big deriving ( Show, Eq )
data Signedness =  Signed | Unsigned deriving ( Show, Eq )
type BitWidth = Int

-- A primitive type describes a sequence of bits with an interpretation
data Prim =   PrimInt BitWidth Endianess Signedness 
                        | PrimArr Int Prim
                        deriving ( Show, Eq )

-- A value is Prim combined with an appropraite value                        
data Value =  PrimIntValue Integer
            | PrimArrValue [Integer] 
            | NamedResult String 
            | Default
            deriving ( Show, Eq )
        
-- An Op represents a operation                 
data Op =   Read !Prim !String        -- read a Prim value from the stream 
                  |        Match Value                     -- match a primitive on the last value read
                  | Select Value [(Value,Op)] -- select an alternative 
                  | Call String [(String,Value)] Op String  -- call a Def with a list of parameters and a list of children and insert Result
                  | Rep Int Op                                 -- repeat n times
                  | Alt [Op]                                 -- return the first alternative that succeeds
                  | Seq [Op]                                 -- execute each Op in sequence
                  | Def String [String] Op  -- define a named template
                  deriving ( Show, Eq )
                  
isDef::Op->Bool
isDef (Def _ _ _) = True
isDef _ = False

returnDefs = filter isDef
stripDefs = filter (not . isDef)

-- A binary template is a synonym for an Op
type BinaryTemplate = Op                






                        
                                        
        
    


