{-
The BinaryEditorView constructs a complex weave of
RegionEditors, TextBuffers and TextViews that implement
the editor GUI.
-}
module GUI.BinaryEditorView where

import Graphics.UI.Gtk

import BinaryEditor.BinaryFile

import GUI.RegionEditor
import GUI.HexEditor 
import Control.Monad

type AttrBinaryView box bf= Attr box (Maybe (BinaryEditorView bf))
data BinaryEditorView bf = BinaryEditorView (Maybe bf) [HPaned] 

-- Constructs a binary view and stores it in a new attribute
-- attached to the VBox supplied.
binaryViewConstruct :: BoxClass bx=>BinaryFile bf=>bx -> IO (AttrBinaryView bx bf)
binaryViewConstruct box = do
    attrBinView <- objectCreateAttribute :: BoxClass bx=>BinaryFile bf=>IO (AttrBinaryView bx bf)
    let binView = BinaryEditorView Nothing []
    set box [ attrBinView := Just binView ]
    return attrBinView
    
binaryViewOpenFile :: BoxClass bx=>BinaryFile bf=> bx -> AttrBinaryView bx bf -> String -> IO ()
binaryViewOpenFile vbox attr fileName = do
    Just (BinaryEditorView _ hpanes) <- get vbox attr
    textView <- textViewNew
    set textView [ textViewEditable := False ]
    
    -- Destroy previous widgets if they exist
    unless (null hpanes) $ do 
        containerRemove vbox (last hpanes)
        mapM_ widgetDestroy hpanes

    binFile <- loadBinary fileName
    
    -- Create a new buffer and use the HexEditor to display
    -- the first section of the file.
    buffer <- textBufferNew Nothing
    editorInit HexEditor buffer
    start <- textBufferGetStartIter buffer
    editorInsertAt HexEditor 
        binFile (head $ getBinarySections binFile) 
        buffer start 
        
    -- Attach the buffer to the view
    textViewSetBuffer textView buffer
    boxPackEnd vbox textView PackGrow 0
    widgetShow textView
    set vbox [ attr := Just (BinaryEditorView (Just binFile) []) ]
    return ()
    
binaryViewAddView::BoxClass bx=>BinaryFile bf=>RegionEditor r=>r->bx->AttrBinaryView bx bf->IO ()
binaryViewAddView editor box attr = do 
    Just (BinaryEditorView (Just binFile) hpanes) <- get box attr
    hpane <- hPanedNew
    panedSetPosition hpane 300
    oldview<-if hpanes == [] 
        then do
            children <- containerGetChildren box
            let oldview = last children
            containerRemove box oldview
            boxPackEnd box hpane PackGrow 0
            return oldview
        else do
            let lastPane = head hpanes
            Just oldview <- panedGetChild2 lastPane
            panedPack2 lastPane hpane True False
            return oldview
            
    textView <- textViewNew
    set textView [ textViewEditable := False ]

    buffer <- textBufferNew Nothing
    editorInit editor buffer
    start <- textBufferGetStartIter buffer
    editorInsertAt editor 
        binFile (head $ getBinarySections binFile) 
        buffer start 
    textViewSetBuffer textView buffer
    panedPack1 hpane oldview True False
    panedPack2 hpane textView True False
    set hpane [ panedChildShrink oldview := True ]
    set hpane [ panedChildShrink textView := True ]
    widgetShowAll hpane
    set box [ attr := Just (BinaryEditorView (Just binFile) (hpane:hpanes)) ]
    return ()