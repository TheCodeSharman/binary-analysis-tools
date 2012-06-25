module Main where

import UnitTests
import Disassembler.Disassembler
import qualified Data.ByteString.Lazy

main :: IO ()
main 
        = do
                putStrLn "Running unit tests..."
                _ <- run
                putStrLn "Running performance tests..."
                putStrLn $
                        let 
                                (rs, _) = decodeList 
                                                (createInputString (Data.ByteString.Lazy.pack (concat (replicate 10000 [0x81,0x84,0x58,0x78,0x56,0x34,0x12,0xaa,0xff,0xff,0xff]))))
                                                intelArchitecture32
                        in
                                show $ length rs

                        
                        
                        
                                
                
