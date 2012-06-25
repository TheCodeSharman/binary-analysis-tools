module BinaryTemplate.TestParse (
        tests, run 
) where

import Test.HUnit

import BinaryTemplate.AbstractSyntax
import BinaryTemplate.Parse

run :: IO Counts
run = runTestTT tests

parseTest :: String -> [BinaryTemplate] -> IO ()
parseTest xml expect
 = do
                res <- loadBinaryTemplateFromString xml
                assertEqual "Result not equal" expect res
         
tests :: Test
tests = TestList [

        "Test bits parse" ~: parseTest 
                 "<bits xmlns=\"http://twistedchimney.net/BinaryTemplate\" type=\"int\" width=\"8\" sign=\"unsigned\" endian=\"little\"/>"
                 [Read (PrimInt 8 Little Unsigned) ""],
                 
        "Test bits parse with label" ~: parseTest 
                 "<bits xmlns=\"http://twistedchimney.net/BinaryTemplate\" type=\"int\" width=\"8\" sign=\"unsigned\" endian=\"little\" label=\"bt1\"/>"
                 [Read (PrimInt 8 Little Unsigned) "bt1"],
                 
        "Test bits parse with match" ~: parseTest 
                 "<bits xmlns=\"http://twistedchimney.net/BinaryTemplate\" type=\"int\" width=\"8\" sign=\"unsigned\" endian=\"little\" match=\"0x23\"/>"
                 [Read (PrimInt 8 Little Unsigned) "", Match (PrimIntValue 0x23)],
                 
        "Test def parse" ~: parseTest 
                 "<def xmlns=\"http://twistedchimney.net/BinaryTemplate\" name=\"t1\" params=\"p1,p2,p3\" />"
                 [Def "t1" ["p1","p2","p3"] (Seq [])],
                 
        "Test def parse with children" ~: parseTest 
                 "<def xmlns=\"http://twistedchimney.net/BinaryTemplate\" name=\"t1\" params=\"p1,p2,p3\"> \
                 \  <t1/> \
                 \  <t2/> \
                 \</def>"
                 [Def "t1" ["p1","p2","p3"] (Seq [Call "t1" [] (Seq []) "",Call "t2" [] (Seq []) ""])],
        
        "Test call parse" ~: parseTest 
                 "<bt1 xmlns=\"http://twistedchimney.net/BinaryTemplate\" arg1=\"a1\" arg2=\"a2\" label=\"test\"/>"
                 [Call "bt1" [("arg1",NamedResult "a1"),("arg2",NamedResult "a2")] (Seq []) "test"],
                 
        "Test call parse with match" ~: parseTest 
                 "<bt1 xmlns=\"http://twistedchimney.net/BinaryTemplate\" match=\"100\"/>"
                 [Call "bt1" [] (Seq []) "",Match (PrimIntValue 100)],
                 
        "Test call parse with children" ~: parseTest 
                 "<bt1 xmlns=\"http://twistedchimney.net/BinaryTemplate\"> \
                 \ <t1/> \
                 \ <t2/> \
                 \</bt1>"
                 [Call "bt1" [] (Seq [Call "t1" [] (Seq []) "",Call "t2" [] (Seq []) ""]) ""],
                 
        "Test alt parse" ~: parseTest 
                 "<alt xmlns=\"http://twistedchimney.net/BinaryTemplate\"> \
                 \  <t1/> \
                 \  <t2/> \
                 \</alt>"
                 [Alt [Call "t1" [] (Seq []) "",Call "t2" [] (Seq []) ""]],
                 
        "Test rep parse" ~: parseTest 
                 "<rep xmlns=\"http://twistedchimney.net/BinaryTemplate\" n=\"45\"> \
                 \  <t1/> \
                 \  <t2/> \
                 \</rep>"
                 [Rep 45 (Seq [Call "t1" [] (Seq []) "",Call "t2" [] (Seq []) ""]) ],
                 
        "Test select parse" ~: parseTest 
                 "<alt xmlns=\"http://twistedchimney.net/BinaryTemplate\" select=\"test1\"> \
                 \  <t1 case=\"1\"/> \
                 \  <t2/> \
                 \</alt>"
                 [Select (NamedResult "test1") [(PrimIntValue 1,Call "t1" [] (Seq []) ""),(Default,Call "t2" [] (Seq []) "")]]

        ]
