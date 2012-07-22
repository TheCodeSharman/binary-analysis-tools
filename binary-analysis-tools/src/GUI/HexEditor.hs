module GUI.HexEditor where

import BinaryEditor.BinaryEditor
import BitStream.HexDump

import GUI.SectionEditor

import Graphics.UI.Gtk

data HexEditor = HexEditor
instance SectionEditor HexEditor where
     editorName _ = return "Hex"

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

     editorInsertAt _ binary section iter buffer = do
         markStart <- textBufferCreateMark buffer Nothing iter True
         markEnd <- textBufferCreateMark buffer Nothing iter False
         textBufferInsert buffer iter hexString
         iterStart <- textBufferGetIterAtMark buffer markStart
         iterEnd <- textBufferGetIterAtMark buffer markEnd
         textBufferApplyTagByName buffer "Hex" iterStart iterEnd
         textBufferDeleteMark buffer markStart
         textBufferDeleteMark buffer markEnd
            where
                hexString = hexRenderer $ sectionGetBytes section binary
