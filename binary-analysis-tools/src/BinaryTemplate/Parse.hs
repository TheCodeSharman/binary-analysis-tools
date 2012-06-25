{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module BinaryTemplate.Parse ( 
        loadBinaryTemplateFromFile, 
        loadBinaryTemplateFromString,
        parseBinaryTemplate
) where

import BinaryTemplate.AbstractSyntax

import Text.XML.HXT.Core
import Data.Char (isDigit)

{- 
    Some helper functions to load and parses the binary template 
-}
loadBinaryTemplateFromFile :: String -> IO [BinaryTemplate]
loadBinaryTemplateFromFile filename
  = loadBinaryTemplateX (readDocument parserOptions filename)
  
loadBinaryTemplateFromString :: String -> IO [BinaryTemplate]
loadBinaryTemplateFromString xml
  = loadBinaryTemplateX (readString parserOptions xml)
                                            
loadBinaryTemplateX :: IOSLA (XIOState ()) XmlTree XmlTree -> IO [BinaryTemplate]
loadBinaryTemplateX loader
        = runX $ loader >>> getChildren >>> isElem >>> binaryTemplateNS >>> parseBinaryTemplate
      
{-  
    The parser proper:
-}

-- Parser options
parserOptions :: [SysConfig]
parserOptions = [ withValidate yes, withCheckNamespaces yes, withRemoveWS yes ]       

-- Namespace used into the BinaryTemplate files
binaryTemplateNS::ArrowXml a => a XmlTree XmlTree
binaryTemplateNS = hasNamespaceUri "http://twistedchimney.net/BinaryTemplate"

-- Break on commas, removing leading spaces
splitByComma :: String -> [String]
splitByComma "" = []
splitByComma ss 
   = case break (==',') (stripWhite ss) of
        (s, _:ss2) -> s : splitByComma ss2
        (s, _) -> [s]
        where 
           stripWhite = dropWhile (`elem` " \n\t")
           
-- A very simplistic parser for the params attribute
parseParams :: String -> [String]
parseParams ps 
   = map validateParam (splitByComma ps)
        where
          isValidChar c = c `elem` "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-."
          validateParam "" = error "illegal parameter"
          validateParam p  = if all isValidChar p then p else error "illegal parameter"
     
-- A value is either a number or a symbolic name     
parseValue :: String -> Value
parseValue s
  | s == "" = Default
  | isDigit $ head s = PrimIntValue (read s)
  | otherwise = NamedResult s
        
        
parseInt :: String -> IOSArrow XmlTree Int
parseInt n = proc int -> do
        s <- getAttrValue n -< int
        returnA -< (read s) 
        
parseMatch :: ArrowXml a => a XmlTree Op
parseMatch = proc mth -> do 
        match <- getAttrValue "match" -< mth
        returnA -< Match (parseValue match)        

-- parse {http://twistedchimney.net/BinaryTemplate}def nodes   
parseDef :: IOSArrow XmlTree Op
parseDef = proc def -> do
        name <- getAttrValue "name" -< def
        params <- getAttrValue "params" -< def
        ops <- listA (parseBinaryTemplate <<< getChildren) -< def
        returnA -< Def name (parseParams params) (Seq ops)
    
-- parse {http://twistedchimney.net/BinaryTemplate}select nodes                    
parseSelect :: IOSArrow XmlTree Op
parseSelect = proc sel -> do
        select <- getAttrValue "select" -< sel
        ops <- listA ( getChildren >>> proc valop -> do
                                        op <- parseBinaryTemplate -< valop
                                        value <- getAttrValue "case" -< valop
                                        returnA -< (parseValue value,op) ) -< sel
        returnA -< Select (parseValue select) ops
  
-- parse {http://twistedchimney.net/BinaryTemplate}alt nodes        
parseAlt :: IOSArrow XmlTree Op
parseAlt = proc alt -> do
        ops <- listA (parseBinaryTemplate <<< getChildren) -< alt
        returnA -< Alt ops
  
-- parse {http://twistedchimney.net/BinaryTemplate}bits nodes                             
parseBits :: IOSArrow XmlTree Op
parseBits = proc bits -> do
        type_ <- getAttrValue "type" -< bits
        width <- parseInt "width" -< bits
        sign <- getAttrValue "sign" -< bits
        endian <- getAttrValue "endian" -< bits
        label <- getAttrValue "label" -< bits
        returnA -< Read (parsePrimitive type_ width sign endian) label
  where
            parseEndian "little" = Little
            parseEndian "big" = Big
            parseEndian _ = error "illegal endianess"
            
            parseSign "unsigned" = Unsigned
            parseSign "signed" = Signed
            parseSign _ = error "illegal signedness"
            
            parsePrimitive t w s e 
                    = case t of
                            "int" -> PrimInt w (parseEndian e) (parseSign s) 
                            _ -> error "unimplemented primitive type" 
                            
-- parse {http://twistedchimney.net/BinaryTemplate}rep nodes
parseRep :: IOSArrow XmlTree Op      
parseRep = proc rep -> do
        count <- parseInt "n" -< rep
        ops <- listA (parseBinaryTemplate <<< getChildren) -< rep
        returnA -< Rep count (Seq ops)
      
-- parse {http://twistedchimney.net/BinaryTemplate}call nodes
parseCall :: IOSArrow XmlTree Op     
parseCall = proc call -> do
        name <- getLocalPart -< call
        label <- getAttrValue "label" -< call
        ops <- listA (parseBinaryTemplate <<< getChildren) -< call
        args <- listA ( parseArg <<< getAttrl ) -< call
        returnA -< Call name args (Seq ops) label
            where 
                parseArg 
                    = ifA (hasName "label" <+> hasName "match" <+> hasName "case" <+> hasName "xmlns") 
                      none
                      (proc arg -> do
                                name <- getLocalPart -< arg
                                val <- getText <<< getChildren -< arg
                                returnA -< ( name, parseValue val ) )  
                              
-- parse any top level {http://twistedchimney.net/BinaryTemplate} nodes
parseBinaryTemplate :: IOSArrow XmlTree BinaryTemplate
parseBinaryTemplate = choiceA [
        (isElem >>> hasName "def") :-> parseDef,
        (isElem >>> hasName "alt") :-> parseAltOrSelect,
        (isElem >>> hasName "rep") :-> parseRep,
        (isElem >>> hasName "bits") :-> (parseBits <+> maybeParseMatch),
        isElem :-> (parseCall <+> maybeParseMatch),
        this :-> error "Unkown Node"
    ] where
        maybeParseMatch = ifA (getAttrValue "match" >>> isA (=="")) none parseMatch
        parseAltOrSelect = ifA (getAttrValue "select" >>> isA (=="")) parseAlt parseSelect
       