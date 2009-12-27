import Control.Applicative

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import qualified ListBox as LB

type GameID = Int

data GUI = GUI {
      mainWindow :: Window,
      btnQuit :: Button,
      btnConnect :: Button,
      gamesListBox :: LB.ListBox GameID,
      connectDialog :: Dialog,
      cdServerEntry :: Entry,
      cdBtnOk :: Button,
      cdBtnCancel :: Button
    }

loadGUI :: IO GUI
loadGUI = do
  Just xml <- xmlNew "client-main-window.glade"
  GUI <$> xmlGetWidget xml castToWindow "mainWindow"
      <*> xmlGetWidget xml castToButton "btnQuit"
      <*> xmlGetWidget xml castToButton "btnConnect"
      <*> (flip LB.newListBox [] =<< xmlGetWidget xml castToTreeView "treeView")
      <*> xmlGetWidget xml castToDialog "connectDialog"
      <*> xmlGetWidget xml castToEntry  "cdServerEntry"
      <*> xmlGetWidget xml castToButton "cdBtnOk"
      <*> xmlGetWidget xml castToButton "cdBtnCancel"

main :: IO ()
main = do
  initGUI
  gui <- loadGUI
  widgetShowAll (mainWindow gui)
  onDestroy (mainWindow gui) mainQuit
  onClicked (btnQuit gui) mainQuit
  onClicked (btnConnect gui) $ runConnectDialog gui
  mainGUI

runConnectDialog :: GUI -> IO ()
runConnectDialog gui = do
  onEntryActivate (cdServerEntry gui) ok
  onClicked (cdBtnOk gui) ok
  onClicked (cdBtnCancel gui) cancel
  windowPresent (connectDialog gui)
      where ok = do putStrLn =<< get (cdServerEntry gui) entryText 
                    widgetHide (connectDialog gui)
            cancel = widgetHide (connectDialog gui)
            
