{-
    Allows the editor to display hex blocks and
    edit them.
-}
module GUI.HexEditor where

import BinaryEditor.BinaryFile
import BitStream.HexDump
import GUI.RegionEditor

import Graphics.UI.Gtk

data HexEditor = HexEditor

instance RegionEditor HexEditor where

     -- This is here just for display purposes
     editorName _ = return "Hex"

     -- Adds the "Hex" tag that provides custom
     -- editting behaviour for Hex regions
     editorInit _ buffer = do
        tagTable <- textBufferGetTagTable buffer
        hexTag <- textTagNew (Just "Hex")
        set hexTag 
          [ textTagEditable := False,
            textTagEditableSet := True,
            textTagFamily := "monospace",
            textTagFamilySet := True,
            textTagForeground := "blue",
            textTagForegroundSet := True ]
        textTagTableAdd tagTable hexTag

     editorInsertAt _ binary section =
         insertAndApplyTag (hexRenderer $ sectionGetBytes section binary) "Hex" 

