module GUI.SectionEditor where

import BinaryEditor.BinaryEditor
import Graphics.UI.Gtk

-- A section editor maps events between the 
-- TextBuffer and the related binary section
-- since a section can be editted in multiple
-- different ways; e.g. Hex, Assembly, Structured Data
-- then each Section has a list of SectionEditors
class SectionEditor a where
    editorName::a->IO String
    editorInit::a->TextBuffer-> IO ()
    editorInsertAt::(BinaryFile b)=>a->b->Section->TextIter->TextBuffer->IO ()
