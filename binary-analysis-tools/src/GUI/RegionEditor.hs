module GUI.RegionEditor where

import BinaryEditor.BinaryFile
import Graphics.UI.Gtk

-- A RegionEditor maps events between a TextBuffer and the related
-- underlying BinaryFile
class RegionEditor a where

    -- A name to show in the context menus for this region type
    editorName::a->IO String
    editorName _ = return "Text"

    -- Do any special initialisation on the TextBuffer
    editorInit::a->TextBuffer-> IO ()
    editorInit _ _ = return ()
    
    -- Insert the complete text for this region
    -- the default method just inserts it as plain text
    editorInsertAt::(BinaryFile b)=>a->b->Section->TextBuffer->TextIter->IO ()
    editorInsertAt _ binary section buffer iter =
        textBufferInsertByteString buffer iter (sectionGetBytes section binary)

-- Some utilities for region implementations
insertAndApplyTag :: String -> TagName -> TextBuffer -> TextIter -> IO ()
insertAndApplyTag text tag buffer iter = do
         markStart <- textBufferCreateMark buffer Nothing iter True
         markEnd <- textBufferCreateMark buffer Nothing iter False
         textBufferInsert buffer iter text
         iterStart <- textBufferGetIterAtMark buffer markStart
         iterEnd <- textBufferGetIterAtMark buffer markEnd
         textBufferApplyTagByName buffer tag iterStart iterEnd
         textBufferDeleteMark buffer markStart
         textBufferDeleteMark buffer markEnd
