
module BinaryTemplate.ResultTree (
                ResultTree( .. ),
                BinaryField( .. ),
                isEmpty,
                subResultTrees,
                findLastResult
        ) where

-- Represents a dissected binary stream with named elements
-- stored in a hierarchy.
data ResultTree = ResultTree String [ResultTree] | Leaf BinaryField | Empty
        deriving ( Show, Eq )
        
data BinaryField = 
        Number Integer | 
        Array [Integer] 
        deriving ( Show, Eq )
        
isEmpty::ResultTree->Bool
isEmpty Empty = True
isEmpty _ = False

subResultTrees::String->ResultTree->[ResultTree]
subResultTrees s rt@(ResultTree lbl rs)
        = if lbl == s then [rt] else  concatMap (subResultTrees s) rs
subResultTrees _ _ = []

findLastResult::[ResultTree]->Maybe BinaryField
findLastResult [] = Nothing
findLastResult rts 
        = case last rts of
                 Leaf bf -> Just bf
                 Empty -> Nothing
                 ResultTree _ lst -> findLastResult lst

                                