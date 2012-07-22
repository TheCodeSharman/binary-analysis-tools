module GUI.Main where 

import Graphics.UI.Gtk

import BinaryEditor.BinaryEditor
import BinaryEditor.UnknownBinaryFile

import GUI.SectionEditor
import GUI.HexEditor 
  
-- Create A View
--buildBinaryView :: Builder -> 
  
-- Load a new file
loadFile :: Builder -> String -> IO ()
loadFile builder fileName = do
    -- First text view
    view <- builderGetObject builder castToTextView "textview1"
    set view [ textViewEditable := False ]
    
    -- Load the binary file using the UnkownBinaryFile loader
    binFile <- loadBinary fileName :: IO UnknownBinaryFile
    
    -- Create a new buffer and use the HexEditor to display
    -- the first section of the file.
    buffer <- textBufferNew Nothing
    editorInit HexEditor buffer
    start <- textBufferGetStartIter buffer
    editorInsertAt HexEditor 
        binFile 
        (head $ getBinarySections binFile) 
        start 
        buffer 
        
    -- Attach the buffer to the view
    textViewSetBuffer view buffer

    -- Second text view: editable version of first
    view2 <- builderGetObject builder castToTextView "textview2"
    buffer2 <- textBufferNew Nothing
    bytes <- readFile fileName
    textBufferSetText buffer2 bytes
    textViewSetBuffer view2 buffer2

-- Configure the actions in the GUI      
actionsSetup:: Builder -> IO Window
actionsSetup builder = do
    -- Open a file
    openFileAction <- builderGetObject builder castToAction "openFileAction"
    on openFileAction actionActivated $ do
        fileChooserDialog <- builderGetObject builder castToFileChooserDialog "fileChooser"
        resp <- dialogRun fileChooserDialog
        if resp == ResponseOk
            then do
                widgetHide fileChooserDialog
                Just fileName <- fileChooserGetFilename fileChooserDialog
                loadFile builder fileName
            else
                widgetHide fileChooserDialog

    -- Quit
    quitAction <- builderGetObject builder castToAction "quitAction"
    on quitAction actionActivated mainQuit

    -- About box
    aboutMenuItem <- builderGetObject builder castToMenuItem "aboutMenuItem"
    on aboutMenuItem menuItemActivate $ do
        aboutBox <- builderGetObject builder castToAboutDialog "aboutDialog"
        dialogRun aboutBox
        widgetHide aboutBox

    -- Configure to quit application on main window close
    mainWindow <- builderGetObject builder castToWindow "mainWindow"
    on mainWindow objectDestroy $ actionActivate quitAction
    return mainWindow
        
-- Load the user interface glade XML file                
loadUserInterface :: IO Builder
loadUserInterface = do
    builder <- builderNew
    builderAddFromFile builder "rsrc/bintools.ui"
    return builder

-- Construct user interface and enter main loop
runMainLoop:: IO ()
runMainLoop = do
    -- Initialise and load user interface
    initGUI
    builder <- loadUserInterface
    mainWindow <- actionsSetup builder
    -- Open the main window
    widgetShowAll mainWindow
    mainGUI