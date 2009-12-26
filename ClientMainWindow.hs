import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import qualified ListBox as LB

dummyGameList = [ (n, "Game" ++ show n) | n <- [1..10] ]

main :: IO ()
main = do
  initGUI
  Just xml <- xmlNew "client-main-window.glade"
  window <- xmlGetWidget xml castToWindow "window"
  btnQuit <- xmlGetWidget xml castToButton "btnQuit"
  treeView <- xmlGetWidget xml castToTreeView "treeView"
  LB.newListBox treeView dummyGameList
  widgetShowAll window
  onDestroy window mainQuit
  onClicked btnQuit mainQuit
  mainGUI
