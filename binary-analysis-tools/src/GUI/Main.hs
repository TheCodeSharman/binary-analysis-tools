
module GUI.Main where 

import Graphics.UI.Gtk
import Debug.Trace
  
-- Load a new file
loadFile :: String -> IO ()
loadFile fileName = traceIO fileName
  
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
                loadFile fileName
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