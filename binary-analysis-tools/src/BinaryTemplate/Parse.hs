{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module BinaryTemplate.Parse ( 
        loadBinaryTemplateFromFile, 
        loadBinaryTemplateFromString,
        parseBinaryTemplate
) where


import BinaryTemplate.AbstractSyntax

import Text.XML.HXT.Core
import Data.Char (isDigit)

options :: [SysConfig]
options = [ withValidate False ]
 
-- this function loads and parses the binary template and returns a list
-- of BinaryTemplate's found in the xml file
loadBinaryTemplateFromFile :: String -> IO [BinaryTemplate]
loadBinaryTemplateFromFile filename
  = loadBinaryTemplateX (readDocument options filename)
  
loadBinaryTemplateFromString :: String -> IO [BinaryTemplate]
loadBinaryTemplateFromString xml
  = loadBinaryTemplateX (readString options xml)
                                            
loadBinaryTemplateX :: IOSLA (XIOState ()) XmlTree XmlTree
                                  -> IO [BinaryTemplate]
loadBinaryTemplateX loader
        = runX ( 
                loader 
                 >>> propagateNamespaces
                >>> uniqueNamespaces
                >>> getChildren             -- find the document elements
                >>> isElem                                        
                >>> parseBinaryTemplate )
                
                     
-- using the Arrow abstraction parse the XML BinaryTemplate
parseBinaryTemplate        :: IOSArrow XmlTree BinaryTemplate
parseBinaryTemplate 
        = choiceA [
                (isElem >>> btNameSpace >>> hasName "def") :-> parseDef,
                (isElem >>> btNameSpace >>> hasName "alt") :-> parseAltOrSelect,
                (isElem >>> btNameSpace >>> hasName "rep") :-> parseRep,
                (isElem >>> btNameSpace >>> hasName "bits") :-> (parseBits <+> maybeParseMatch),
                (isElem >>> btNameSpace) :-> (parseCall <+> maybeParseMatch),
                this :-> error "embedded foreign XML currently not implemented"
          ]
        where
                btNameSpace =  hasNamespaceUri "http://twistedchimney.net/BinaryTemplate"
        
                -- Break on commas, removing leading spaces
                splitByComma :: String -> [String]
                splitByComma "" = []
                splitByComma ss 
                        = let
                                stripWhite = dropWhile (flip elem " \n\t")
                          in
                                case break (==',') (stripWhite ss) of
                                        (s, _:ss2) -> s : splitByComma ss2
                                        (s, _) -> [s]
                
                -- A very simplistic parser for the params attribute
                parseParams :: String -> [String]
                parseParams ps 
                        = let
                                isValidChar c = c `elem` "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-."
                                validateParam "" = error "illegal parameter"
                                validateParam p  = if (all isValidChar p) then p else error "illegal parameter"
                          in
                                  map validateParam (splitByComma ps)
                
                maybeParseMatch
                    = ifA
                            (getAttrValue "match" >>> isA (==""))
                            none
                            parseMatch
                        
                parseMatch = proc mth -> do 
                        match <- getAttrValue "match" -< mth
                        returnA -< Match (parseValue match)        
                          
                parseDef = proc def -> do
                        name <- getAttrValue "name" -< def
                        params <- getAttrValue "params" -< def
                        ops <- listA (parseBinaryTemplate <<< getChildren) -< def
                        returnA -< Def name (parseParams params) (Seq ops)
                
                parseAltOrSelect 
                    = ifA
                        (getAttrValue "select" >>> isA (==""))
                        parseAlt
                        parseSelect
                        
                parseValue :: String -> Value
                parseValue s 
                  = if s == "" then
                                  Default
                          else if isDigit $ head s then
                                  PrimIntValue (read s)
                          else
                                  NamedResult s
                        
                parseSelect = proc sel -> do
                        select <- getAttrValue "select" -< sel
                        ops <- listA ( getChildren >>> proc valop -> do
                                                        op <- parseBinaryTemplate -< valop
                                                        value <- getAttrValue "case" -< valop
                                                        returnA -< (parseValue value,op) ) -< sel
                        returnA -< Select (parseValue select) ops
                        
                
                parseAlt = proc alt -> do
                        ops <- listA (parseBinaryTemplate <<< getChildren) -< alt
                        returnA -< Alt ops
                        
                parseInt :: String -> IOSArrow XmlTree Int
                parseInt n = proc int -> do
                        s <- getAttrValue n -< int
                        returnA -< (read s)                        
                
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
                        
                parseRep = proc rep -> do
                        count <- parseInt "n" -< rep
                        ops <- listA (parseBinaryTemplate <<< getChildren) -< rep
                        returnA -< Rep count (Seq ops)
                        
                parseCall = proc call -> do
                        name <- getLocalPart -< call
                        label <- getAttrValue "label" -< call
                        ops <- listA (parseBinaryTemplate <<< getChildren) -< call
                        args <- listA ( parseArg <<< getAttrl ) -< call
                        returnA -< Call name args (Seq ops) label
                        
                parseArg 
                    = ifA (hasName "label" <+> hasName "match" <+> hasName "case") 
                      none
                      (proc arg -> do
                                name <- getLocalPart -< arg
                                val <- getText <<< getChildren -< arg
                                returnA -< ( name, parseValue val ) )