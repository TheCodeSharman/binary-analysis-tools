module BinaryTemplate.TestEvaluate (
        tests, run 
) where

import Test.HUnit

import BitStream.BitStream
import BinaryTemplate.AbstractSyntax
import BinaryTemplate.ResultTree
import BinaryTemplate.Evaluate

import Data.ByteString.Lazy
 
run = runTestTT tests 
                                        
tests = TestList [
-- reading primitive values
        "Read primitive values - integer unsigned" ~:
                Just (ResultTree "test" [Leaf (Number 0x04321921)]) ~=? evaluate 
                                 (Read (PrimInt 32 Big Unsigned) "test")
                                 (pack [0x04, 0x32, 0x19, 0x21]),
                                 
        "Read primitive values - integer unsigned little" ~:
                Just (ResultTree "test" [Leaf (Number 0x21193204)]) ~=? evaluate 
                                 (Read (PrimInt 32 Little Unsigned) "test")
                                 (pack [0x04, 0x32, 0x19, 0x21]),
                                 
        "Read primitive values - integer signed" ~:
                Just (ResultTree "test" [Leaf (Number (-0x04321921))]) ~=? evaluate 
                                 (Read (PrimInt 32 Big Signed) "test")
                                 (pack [0xfb, 0xcd, 0xe6, 0xdf]),
                                 
        "Read primitive values - arrays" ~:
                Just (ResultTree "test" [Leaf (Array [0x04, 0x32, 0x19, 0x21])])
                        ~=? evaluate
                                        (Read (PrimArr 4 (PrimInt 8 Little Unsigned)) "test")
                                        (pack [0x04, 0x32, 0x19, 0x21]),
                                        
        "Read primitive values - arrays nested" ~:
                Just (ResultTree "test" [
                                        Leaf (Array [0x04, 0x32, 0x19, 0x21]),
                                        Leaf (Array [0x04, 0x32, 0x19, 0x21])
                                        ])
                        ~=? evaluate
                                        (Read (PrimArr 2 (PrimArr 4 (PrimInt 8 Little Unsigned))) "test")
                                        (pack [0x04, 0x32, 0x19, 0x21,0x04, 0x32, 0x19, 0x21]),
                                        
-- match
        "fail a match" ~:
                Nothing 
                        ~=? evaluate
                                        (Seq [Read (PrimInt 32 Big Signed) "test",
                                                  Match $ PrimIntValue 0x04321921])
                                        (pack [0xfb, 0xcd, 0xe6, 0xdf]),
                                        
        "pass a match" ~:
                Just (ResultTree "test" [Leaf (Number 0x04321921)]) 
                        ~=? evaluate
                                        (Seq [Read (PrimInt 32 Big Signed) "test",
                                                  Match $ PrimIntValue 0x04321921])
                                        (pack [0x04, 0x32, 0x19, 0x21]),
        
        "match on named result" ~:        
                Just (ResultTree "" [
                                        ResultTree "1" [Leaf (Number 0x04321921)],
                                        ResultTree "2" [Leaf (Number 0x04321921)]
                                ]) 
                        ~=? evaluate
                                        (Seq [
                                                    Seq [Read (PrimInt 32 Big Signed) "1",
                                                               Read (PrimInt 32 Big Signed) "2"],
                                                    Match $ NamedResult "1"
                                                 ])
                                        (pack [0x04, 0x32, 0x19, 0x21, 0x04, 0x32, 0x19, 0x21]),        
                                        
        "match after call" ~:
                Just (ResultTree "" [
                                        Leaf (Number 0x04321921),
                                        Leaf (Number 0x04321921)
                                ]) 
                        ~=? evaluate
                                        (Seq [
                                                Def "a" [] (Read (PrimInt 32 Big Signed) ""),
                                                Call "a" [] (Seq []) "",
                                                Match $ PrimIntValue 0x04321921,
                                                Call "a" [] (Seq []) ""
                                                 ])
                                        (pack [0x04, 0x32, 0x19, 0x21, 0x04, 0x32, 0x19, 0x21]),
                                        
        "match after call as an alt" ~:
                Just (Leaf (Number 0x04331921)) 
                        ~=? evaluate
                                        (Alt [
                                                Def "a" [] (Read (PrimInt 32 Big Signed) ""),
                                                Call "a" [] (Seq []) "",
                                                Match $ PrimIntValue 0x04321921,
                                                Call "a" [] (Seq []) "",
                                                Match $ PrimIntValue 0x04331921
                                                 ])
                                        (pack [0x04, 0x33, 0x19, 0x21, 0x04, 0x32, 0x19, 0x21]),
-- control flow
        "Sequence primitive values" ~:
                Just (ResultTree "" [
                                ResultTree "test1" [Leaf $ Number 0x04321921],
                                ResultTree "test2" [Leaf $ Number (-0x04321921)]
                                ])
                        ~=? evaluate
                                        (Seq [Read (PrimInt 32 Big Unsigned) "test1",
                                                  Read (PrimInt 32 Big Signed) "test2"])
                                        (pack [0x04, 0x32, 0x19, 0x21, 0xfb, 0xcd, 0xe6, 0xdf]),

        "Alternation - first value" ~:
                Just (ResultTree "test1" [Leaf $ Number 0x04321921])
                        ~=? evaluate
                                        (Alt [Read (PrimInt 32 Big Unsigned) "test1",
                                                  Read (PrimInt 32 Big Signed) "test2"])
                                        (pack [0x04, 0x32, 0x19, 0x21]),
        
        "Alternation - second value" ~:
                Just (ResultTree "test2" [Leaf $ Number (-0x04321921)])
                        ~=? evaluate
                                        (Alt [
                                                   Seq [Read (PrimInt 32 Big Signed) "test",
                                                              Match $ PrimIntValue 0x04321921],
                                                   Seq [Read (PrimInt 32 Big Signed) "test2",
                                                             Match $ PrimIntValue (-0x04321921)]
                                                 ])
                                        (pack [0xfb, 0xcd, 0xe6, 0xdf]),

-- select on primitive values
        "Select - on hardcoded int 2nd branch" ~:
                Just (ResultTree "t2" [Leaf $ Number (0xfb)])
                        ~=? evaluate 
                                        (Select (PrimIntValue 2)
                                                [ (PrimIntValue 1, Read (PrimInt 32 Little Unsigned) "t1"),
                                                  (PrimIntValue 2, Read (PrimInt 8 Little Unsigned) "t2") ])
                                        (pack [0xfb, 0xcd, 0xe6, 0xdf]),
                                        
        "Select - on hardcoded int 1st branch" ~:
                Just (ResultTree "t1" [Leaf $ Number (-0x04321921)])
                        ~=? evaluate 
                                        (Select (PrimIntValue 1)
                                                [ (PrimIntValue 1, Read (PrimInt 32 Big Signed) "t1"),
                                                  (PrimIntValue 2, Read (PrimInt 8 Little Unsigned) "t2") ])
                                        (pack [0xfb, 0xcd, 0xe6, 0xdf]),
                                        
-- repetition
        "Repetition - simple" ~:
                Just (ResultTree "" [Leaf $ Number 1, Leaf $ Number 2, Leaf $ Number 3, Leaf $ Number 4]) 
                        ~=? evaluate
                                        (Rep 4 (Read (PrimInt 8 Little Unsigned) ""))
                                        (pack [1,2,3,4]),
                                        
-- call a template
        "Call template" ~:
                Just (Leaf $ Number 1)
                        ~=? evaluate
                                        (Seq [
                                                Def "a" [] (Read (PrimInt 8 Little Unsigned) ""),
                                                Call "a" [] (Seq []) "" 
                                        ])
                                        (pack [1]),
                
        {- This needs some explanation, the third argument in a Call is the
        sequence of Ops that is nested in the call, the nested ops are
        called after the template executes -}                        
        "Call template - fallthrough" ~:
                Just (ResultTree "" [Leaf $ Number 1, Leaf $ Number 2])
                  ~=? evaluate
                                        (Seq [
                                                Def "a" [] (Read (PrimInt 8 Little Unsigned) ""),
                                                Call "a" [] (Read (PrimInt 8 Little Unsigned) "") "" 
                                        ])
                                        (pack [1,2]),
                                        
        "Call template - with arguments, match variant" ~:
                Just (Leaf $ Number 4)
                        ~=? evaluate
                                        (Seq [
                                                Def "a" ["switch"] (Seq [
                                                                Read (PrimInt 8 Little Unsigned) "",
                                                                Match (NamedResult "switch")
                                                        ]),
                                                Call "a" [("switch",PrimIntValue 0x4)] (Seq []) ""
                                        ])
                                        (pack [4]),
                                        
        {- When calling a template any NamedResult arguments must be looked up
           before passing the parameters -}
        "Call template passing namedresult argument - parameter name " ~:
                Just (Leaf $ Number 4)
                        ~=? evaluate
                                        (Seq [
                                                Def "b" ["arg1"] 
                                                        (Seq [
                                                                Def "a" ["switch"] (Seq [
                                                                                Read (PrimInt 8 Little Unsigned) "",
                                                                                Match (NamedResult "switch")
                                                                        ]),
                                                                Call "a" [("switch",NamedResult "arg1")] (Seq []) ""
                                                        ]),
                                                Call "b" [("arg1",PrimIntValue 0x4)] (Seq []) ""
                                        ])
                                        (pack [4]),
                                        
        "Call template passing namedresult argument - result name " ~:
                Just (ResultTree "v1" [Leaf (Number 4)])
                        ~=? evaluate
                                        (Seq [
                                                Def "a" ["switch"] (Seq [
                                                                Read (PrimInt 8 Little Unsigned) "",
                                                                Match (NamedResult "switch")
                                                        ]),
                                                Read (PrimInt 8 Little Unsigned) "v1",
                                                Call "a" [("switch",NamedResult "v1")] (Seq []) ""
                                        ])
                                        (pack [4,4]),
                                        
        "Call template - with arguments, match variant, fail" ~:
                Nothing
                        ~=? evaluate
                                        (Seq [
                                                Def "a" ["switch"] (Seq [
                                                                Read (PrimInt 8 Little Unsigned) "",
                                                                Match (NamedResult "switch")
                                                        ]),
                                                Call "a" [("switch",PrimIntValue 0x1)] (Seq []) ""
                                        ])
                                        (pack [4]),
                                        
        "Call template - with arguments, select variant" ~:
                Just (Leaf $ Number 0xf)
                        ~=? evaluate
                                        (Seq [
                                                Def "a" ["switch"] 
                                                        (Select (NamedResult "switch")
                                                                [(PrimIntValue 0x1, Read (PrimInt 32 Big Signed) ""),
                                                             (PrimIntValue 0x4, Read (PrimInt 8 Little Unsigned) "")]
                                                         ),
                                                Call "a" [("switch",PrimIntValue 0x4)] (Seq []) ""
                                        ])
                                        (pack [0xf]),
                                        
        "Call template - with arguments, select variant, 2" ~:
                Just (Leaf $ Number (-0x04321921))
                        ~=? evaluate
                                        (Seq [
                                                Def "a" ["switch"] 
                                                        (Select (NamedResult "switch")
                                                                [(PrimIntValue 0x1, Read (PrimInt 32 Big Signed) ""),
                                                             (PrimIntValue 0x4, Read (PrimInt 8 Little Unsigned) "")]
                                                         ),
                                                Call "a" [("switch",PrimIntValue 0x1)] (Seq []) ""
                                        ])
                                        (pack [0xfb, 0xcd, 0xe6, 0xdf]),
                                        
        "Call template with alt calls failing to match" ~:
                Just (Leaf $ Number 1)
                        ~=? evaluate
                                        (Alt [
                                                Def "a" [] (Seq [
                                                                                Read (PrimInt 32 Big Signed) "matched",
                                                                                Match $ PrimIntValue 0x04321921 
                                                                        ]),
                                                Call "a" [] (Seq []) "",
                                                Read (PrimInt 8 Big Signed) ""
                                        ])
                                        (pack [1]),
                                        
        "Call template with nested alt calls failing to match" ~:
                Just (Leaf $ Number 1)
                        ~=? evaluate
                                        (Alt [
                                                Def "a" [] (Alt [
                                                                                Read (PrimInt 32 Big Signed) "matched",
                                                                                Match $ PrimIntValue 0x04321921,
                                                                                Read (PrimInt 32 Big Signed) "matched2",
                                                                                Match $ PrimIntValue 0x04351921
                                                                        ]),
                                                Call "a" [] (Seq []) "",
                                                Read (PrimInt 8 Big Signed) ""
                                        ])
                                        (pack [1]),
                                        
        "Nested Alts" ~:
                Just (Leaf $ Number 1)
                        ~=? evaluate
                                        (Alt [
                                                Alt [
                                                                Read (PrimInt 32 Big Signed) "matched",
                                                                Match $ PrimIntValue 0x04321921,
                                                                Read (PrimInt 32 Big Signed) "matched2",
                                                                Match $ PrimIntValue 0x04351921
                                                        ],
                                                Read (PrimInt 8 Big Signed) ""
                                        ])
                                        (pack [1]),
                                        
        "Empty Sequence Succeeds" ~:
                Just (Leaf (Number 1))
                        ~=? evaluate
                                        (Seq [
                                                Seq [],
                                                Read (PrimInt 8 Big Unsigned) ""
                                        ])
                                        (pack [1]),
                                        
        "Using an empty sequence to make an optional template" ~:
                Just (Leaf (Number 1))
                        ~=? evaluate
                                        (Seq [
                                                Alt [
                                                        Read (PrimInt 32 Big Signed) "matched",
                                                        Match $ PrimIntValue 0x04321921,
                                                        Read (PrimInt 32 Big Signed) "matched2",
                                                        Match $ PrimIntValue 0x04351921,
                                                        Seq []
                                                ]
                                                ,Read (PrimInt 8 Big Unsigned) ""
                                        ])
                                        (pack [1]),
        
                                        
        "Make sure a failed alt in a seq fails the seq" ~:
                Nothing
                        ~=? evaluate
                                        (Seq [
                                                Alt [
                                                        Read (PrimInt 32 Big Signed) "matched",
                                                        Match $ PrimIntValue 0x04321921,
                                                        Read (PrimInt 32 Big Signed) "matched2",
                                                        Match $ PrimIntValue 0x04351921
                                                ]
                                                ,Read (PrimInt 8 Big Unsigned) ""
                                        ])
                                        (pack [1]),
                                        
        "Strip out unlabelled results in preference to labelled" ~:
                Just (ResultTree "" [
                                ResultTree "label2" [Leaf (Number 3)],
                                ResultTree "label" [Leaf (Number 1)]
                                ])
                        ~=? evaluate
                                        (Seq [
                                                Seq [
                                                        Read (PrimInt 8 Big Signed) "",
                                                        Read (PrimInt 8 Big Signed) ""
                                                ],
                                                Read (PrimInt 8 Big Signed) "label2",
                                                Read (PrimInt 8 Big Signed) "",
                                                Read (PrimInt 8 Big Signed) "label"
                                         ])
                                         (pack [5,4,3,2,1]),
                                                
                "Select statement with default" ~:
                        Just (ResultTree "t1" [Leaf $ Number (-0x04321921)])
                                ~=? evaluate 
                                                (Select (PrimIntValue 1)
                                                        [ (PrimIntValue 2, Read (PrimInt 8 Little Unsigned) "t2"),
                                                          (Default, Read (PrimInt 32 Big Signed) "t1")
                                                           ])
                                                (pack [0xfb, 0xcd, 0xe6, 0xdf]),
                                                
                "Test resulttree pruning with result lifting" ~:
                Just (ResultTree "" [
                                ResultTree "label3" [Leaf (Number 5)],
                                ResultTree "label2" [Leaf (Number 3)],
                                ResultTree "label" [Leaf (Number 1)]
                                ])
                        ~=? evaluate
                                        (Seq [
                                                Seq [
                                                        Read (PrimInt 8 Big Signed) "label3",
                                                        Read (PrimInt 8 Big Signed) ""
                                                ],
                                                Read (PrimInt 8 Big Signed) "label2",
                                                Read (PrimInt 8 Big Signed) "",
                                                Read (PrimInt 8 Big Signed) "label"
                                         ])
                                         (pack [5,4,3,2,1])                                        
        ] 
