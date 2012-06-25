module BinaryTemplate.Evaluate (
        evaluate,
        compile,
        execute,
        CompiledBinTemp
 ) where
 
import BitStream.BitStream
import qualified Data.ByteString.Lazy
import BinaryTemplate.AbstractSyntax
import BinaryTemplate.ResultTree

import qualified Data.Map

import Data.List
import Control.Monad

newtype CompiledBinTemp = CompiledBinTemp { bitStream :: BitStream ResultTree }

-- evaluate a self contained BinaryTemplate (no external calls)
evaluate::BinaryTemplate->Data.ByteString.Lazy.ByteString->Maybe ResultTree
evaluate bt s = case execute (compile bt) (createInputString s) of
                                        Just (a, _) -> Just a
                                        Nothing -> Nothing

-- pre compile a BinaryTemplate into a BitStream
compile::BinaryTemplate->CompiledBinTemp
compile bt = CompiledBinTemp $ compileBT NoEScope bt

-- run the provided bitstream
execute::CompiledBinTemp->InputString->Maybe ( ResultTree, InputString )
execute cbt = runBS (bitStream cbt)

{- 
   The EvaluationScope keeps track of defined functions and the current
   argument list during evaluation. The scoping rules are that parameters
   are only available in the immediate template NOT its grandchildren.
   
   When looking up a NamedResult use the following:
           1. Is there an argument in the current EScope? If so use that value.
           2. Look up the name in the current value (ResultTree) and use this.
           3. Fail with unknown variable error
   
   Note that Alt, Seqs and Defs create new scopes, the difference being that the
   arguments list inherits into the Alt and Seq scopes, whereas the Def body can
   not access the arguments of the caller.
   
   When processing a Call use the following:
           1. try to lookup the def in the current scope, if possible, call this def.
           2. resursive search the parent scope looking for a match.
           3. fail with unknown template error
-}
   
data CDef = CDef [String] Op 
type CDefMap = Data.Map.Map String CDef
type ArgMap = Data.Map.Map String Value
data EvaluationScope = EScope CDefMap ArgMap EvaluationScope | NoEScope 
                                           
{- The following helper functions allow access to the current scope -}
getArgument::EvaluationScope->String->Maybe Value
getArgument NoEScope _ = Nothing
getArgument (EScope _ vals _) name 
        = Data.Map.lookup name vals -- no recursion here by design
                
getArgs::EvaluationScope->ArgMap
getArgs (EScope _ args _) = args
getArgs NoEScope = Data.Map.empty

getCDef::EvaluationScope->String->Maybe CDef
getCDef NoEScope _ = Nothing
getCDef (EScope cdfs _ parent) name
        = case Data.Map.lookup name cdfs of
                Just cdef -> Just cdef
                Nothing -> getCDef parent name

{- Builds a new scope, this function is called whenever a new scope of
   Defs is entered. Eg. in an Alt, Seq or Call -}
newEScope::EvaluationScope->[Op]->ArgMap->EvaluationScope
newEScope parent bts args 
        = EScope (compileDefs bts) args parent         
                where
                    compileDefs::[BinaryTemplate]->CDefMap
                    compileDefs = foldr addDef Data.Map.empty
                    addDef (Def name params op) = Data.Map.insert name (CDef params op) 
                                                    
{- Lookup a NamedResult -}
getNamedResult::EvaluationScope->String->ResultTree->Maybe (Either Value BinaryField)
getNamedResult escope name rt
        = case getArgument escope name of
                Just v -> Just $ Left v 
                Nothing -> case findLastResult (subResultTrees name rt) of
                                         Just bf -> Just $ Right bf
                                         Nothing -> Nothing

getNamedResultList::EvaluationScope->String->[ResultTree]->Maybe (Either Value BinaryField)
getNamedResultList escope name [] 
    = getNamedResult escope name Empty
    
getNamedResultList escope name (r:rs)
        = case getNamedResult escope name r of
                Just v -> Just v
                Nothing -> getNamedResultList escope name rs
                           
{- The compileBT function defines how the BinaryTemplate is mapped to a
   BitStream. -}
compileBT::EvaluationScope->BinaryTemplate->BitStream ResultTree

-- Top level Def (internal defs should be filtered out)
compileBT escope (Def name params op)
        = {-# SCC "compileBT-Def" #-} if params == [] then
                compileBT escope op
          else 
                  fail "top level defs currently don't support paramters"
                  
-- Calling a def with no context
compileBT escope op@(Call _ _ _ _)
        = {-# SCC "compileBT-Call" #-} callTemplate escope [Empty] op

-- Reading values from the bit stream
compileBT _ (Read pr lbl)
        = {-# SCC "compileBT-Read" #-}  case pr of 
                -- Simple read of an integer
                PrimInt w e sgn 
                        -> labelResultTreeM lbl $ liftM (\x->Leaf $ Number x) $ readPrimInt w e sgn
                        
                -- An array of integers; this is a separate case to the nested version
                -- so that we can attach a label to the result (which readNestedArr 
                -- doesn't do).
                PrimArr c pr@(PrimInt _ _ _)
                        -> labelResultTreeM lbl $ readPrimArr c pr
                        
                -- The general case recurses over readNestedArr
                PrimArr c pr
                        -> readNestedArr c pr lbl
                        
                where
                        -- read an integer via BitStream
                        readPrimInt w e sgn
                                =        case sgn of
                                                Signed -> readSignedInteger (selectBuilder e) w
                                                Unsigned ->readUnsignedInteger (selectBuilder e) w
                                        where
                                                selectBuilder e
                                                        = case e of
                                                                Little -> littleEndian
                                                                Big -> bigEndian
                                        
                        -- the base case for an array of integers
                        readPrimArr::Int->Prim->BitStream ResultTree                        
                        readPrimArr c (PrimInt w e sgn) 
                                = liftM (\x->Leaf (Array x)) $ sequence $ replicate c (readPrimInt w e sgn)
                        
                        -- the nested case is more complex
                        readNestedArr::Int->Prim->String->BitStream ResultTree
                        readNestedArr c pr lbl
                                = case pr of
                                        PrimArr c2 pr2
                                                -> labelResultTreesM lbl $ sequence $ replicate c (readNestedArr c2 pr2 "")
                                        PrimInt _ _ _
                                                -> readPrimArr c pr
                                                                          
-- Sequencing in general
-- this function needs to build a list of namedresults 
compileBT escope (Seq lst)
        = {-# SCC "compileBT-doop" #-} 
                  labelResultTreesM "" $ doOp (return []) (stripDefs lst)
                    where
                        escope2 = newEScope escope (returnDefs lst) (getArgs escope)
                
                        -- doOp needs to scan the list of Ops looking for
                        -- context sensitive Match and Call statements and
                        -- process them with the appropriate context
                        doOp::BitStream [ResultTree]->[Op]->BitStream [ResultTree]
                                   
                        doOp rs (Match v:ops)
                                = {-# SCC "compileBT-doop-match" #-}doOp
                                        (do
                                                 lst <- rs
                                                 compileMatch escope2 lst v
                                                 return lst) 
                                          ops
                                        
                        doOp rs (cop@(Call _ _ _ _):ops)
                                = {-# SCC "compileBT-doop-call" #-} doOp
                                        (do
                                                lst <- rs
                                                r <- callTemplate escope2 lst cop
                                                return $ lst++[r]) 
                                                ops
                                                  
                        doOp rs (op:ops) 
                                = {-# SCC "compileBT-doop-list" #-} doOp 
                                        (do
                                                lst <- rs 
                                                r <- compileBT escope2 op
                                                return $ lst++[r])
                                        ops
                        
                        doOp rs []
                                = rs

        
-- Alternation
compileBT escope (Alt lst)
        = {-# SCC "compileBT-Alt" #-}  alternatives $ compileMatches escope2 (stripDefs lst)
                where
                        escope2 = newEScope escope (returnDefs lst) (getArgs escope)
                        -- Scan through a list of ops processing match statements
                        -- then return a list of bitstreams
                        compileMatches::EvaluationScope->[Op]->[BitStream ResultTree]
                        compileMatches es (Match _:ops)
                                = error "invalid Match; no operation to match on!"
                         
                        compileMatches es (rd:Match v:ops)
                                 = (do
                                         r <- compileBT es rd
                                         compileMatch es [r] v
                                         return r) : (compileMatches es ops)
                                                 
                                                                                        
                        compileMatches es (op:ops)
                                = (compileBT es op) : (compileMatches es ops)
                        
                        compileMatches es []
                                = []

-- Select
compileBT escope (Select val lst)
        = {-# SCC "compileBT-Select" #-}  compileBT escope (lookUpVal (resolveSelect val) lst)
                where
                        
                        resolveSelect val 
                                = {-# SCC "resolveSelect" #-} case val of
                                        NamedResult nm 
                                                -> case getArgument escope nm of
                                                        Just a -> a
                                                        Nothing -> error ("unable to find parameter '" ++ nm ++ "'")
                                        _ -> val
                
                        lookUpVal::Value->[(Value,Op)]->Op
                        lookUpVal val [] = error ("no select option for value: " ++ show val)
                        lookUpVal val ((vc,code):lss)
                                =  {-# SCC "lookUpVal" #-} case vc of
                                          Default -> code
                                          _ -> if (val == vc) then code else lookUpVal val lss
-- Rep
compileBT escope (Rep n op)
        = {-# SCC "compileBT-Rep" #-}  compileBT escope (Seq (replicate n op))
        
compileBT _ op = error $ "unimplemented BinaryTemplate " ++ (show op)
                                        

-- compile a Match statement        
compileMatch::EvaluationScope->[ResultTree]->Value->BitStream ()
compileMatch escope rd v
         = let         
                        compareValues::BinaryField->Value->Bool
                        compareValues (Number n) (PrimIntValue v2) = (n == v2)
                        compareValues (Array ns) (PrimArrValue vs) = (ns == vs) 
                        compareValues _ _ = False
                        
                        checkMatch::[ResultTree]->BinaryField->Value->Bool
                        checkMatch rs bf v2
                                = case v2 of
                                                  NamedResult nm 
                                                          -> case getNamedResultList escope nm rs of
                                                                  Just (Left v3) -> compareValues bf v3 
                                                                  Just (Right bf2) -> bf == bf2
                                                  _ -> compareValues bf v
                  in
                                case findLastResult rd of
                                        Just bf 
                                          -> if checkMatch rd bf v then
                                                          return ()
                                                   else
                                                           fail "no match"
                                                           
                                        Nothing -> fail "no match"
        
-- Call a template, checking parameters and looking up arguments
-- the caller needs to pass the current result tree so that this 
-- can be used to look up arguments.
callTemplate::EvaluationScope->[ResultTree]->Op->BitStream ResultTree
callTemplate escope results (Call def args fallOps lbl)
         = case getCDef escope def of
                Just (CDef params op) 
                        -> let
                            toArgMap' mp [] = mp
                            toArgMap' mp ((n,v):as) = toArgMap' (Data.Map.insert n v mp) as
                            
                            toArgMap = toArgMap' Data.Map.empty
                            
                            -- errors on any missing formal parameters
                            checkParams1 [] as = toArgMap as
                            checkParams1 (p:ps) as 
                                    = let
                                            args = toArgMap as
                                      in case Data.Map.lookup p args of
                                            Just _ -> checkParams1 ps as
                                            Nothing -> error ("Missing parameter '" ++ p ++ "'")
                            
                            -- errors on any unknown arguments
                            checkParams2 ps as 
                                    = case Data.Map.keys as \\ ps of
                                            [] -> as
                                            ms -> error ("Unknown arguments: " ++ concat( intersperse ", " ms ))        
                            
                            -- combine both checks
                            checkParams ps as 
                                    = checkParams2 ps (checkParams1 ps as)
    
                            -- Any NamedResult arguments need to be retrieved now
                            checkedArgs 
                                    = let
                                            validArgs = checkParams params args
                                            
                                            convertToValue (Left v) = v
                                            convertToValue (Right bf) 
                                                    = case bf of
                                                            Number i -> PrimIntValue i
                                                            Array a -> PrimArrValue a
                                                    
                                            lookupArg v
                                                    = case v of
                                                            NamedResult n 
                                                                    -> case getNamedResultList escope n results of
                                                                                    Just v ->  convertToValue v
                                                                                    Nothing -> error ("Unable to find named result: '" ++ n ++ "'") 
                                                            _ -> v
                                      in
                                               Data.Map.map lookupArg validArgs
                            
                            doTemplate = liftM (:[]) (compileBT escope2 op)
                                               where
                                                 escope2 = newEScope escope [] checkedArgs
                    in
                            {- when there are fallthrough operations these need to be executed in the
                               parent scope after the template has executed -}
                            case fallOps of
                                    Seq [] -> labelResultTreesM lbl $ doTemplate
                                    _  -> labelResultTreesM lbl $ 
                                                    doTemplate 
                                                            >>= \rs -> compileBT escope fallOps        
                                                            >>= \r -> return (rs++[r])                                         
                                        
                Nothing 
                        -> error ("Unknown template '" ++ def ++ "'")
                                
-- construction helpers
labelResultTreeM::(Monad m)=>String->m ResultTree->m ResultTree
labelResultTreeM lbl = labelResultTreesM lbl . (liftM (:[]) )

labelResultTreesM::(Monad m)=>String->m [ResultTree]->m ResultTree
labelResultTreesM lbl mrs 
        = liftM (removeEmptyLbl . processResults) mrs
                where
                        addLabel = ResultTree lbl
                        
                        -- strip out superfluous top level labels
                        removeEmptyLbl rs
                                = if lbl == "" then
                                        case rs of
                                                [] -> Empty
                                                (r:[]) -> r
                                                _ -> addLabel rs
                                  else
                                          addLabel rs
                                          
                        -- remove unlabelled results
                        stripUnlabelled 
                                = filter (\rt->case rt of
                                                                Leaf _ -> False
                                                                --ResultTree "" _ -> False
                                                                _ -> True)
                                                                
                        -- remove empty results
                        stripEmpty 
                                = filter (not . isEmpty)
                                
                        -- combine empty label trees
                        liftEmptyTrees::[ResultTree]->[ResultTree]
                        liftEmptyTrees [] = []
                        liftEmptyTrees ((ResultTree "" irs):rs) = irs ++ (liftEmptyTrees rs)
                        liftEmptyTrees (r:rs) = r : (liftEmptyTrees rs)
                                                                
                        -- we need to keep unlabelled results if there is nothing
                        -- else labelled
                        processResults rs
                                = let
                                        lifted = liftEmptyTrees rs
                                        estripped = stripEmpty lifted
                                        lstripped = stripUnlabelled estripped
                                  in
                                          case lstripped of
                                                  [] -> estripped
                                                  _ -> lstripped