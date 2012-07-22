
module BinaryEditor.UnknownBinaryFile where

import BinaryEditor.BinaryEditor
import BitStream.HexDump

import qualified Data.ByteString as BS

-- A binary file
data UnknownBinaryFile = MkUnknownBinaryFile 
    BS.ByteString      -- Bytes in file
    [Section]           -- List of sections that are mapped

instance BinaryFile UnknownBinaryFile where
    loadBinary filePath = do
         bytes <- BS.readFile filePath
         let 
            section = 
                MkSection 
                Nothing 0 (toAddress 0) 
                  (toAddress $ BS.length bytes) 
         return $ MkUnknownBinaryFile bytes [section]
    saveBinary _ _ = error "Unimplemented!"
    getBinaryBytes (MkUnknownBinaryFile bytes _) = bytes
    getBinarySections (MkUnknownBinaryFile _ sections) = sections