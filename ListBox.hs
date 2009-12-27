-- |
-- This module contains utility functions to display a simple list of
-- elements which can be selected, using TreeView and ListStore
-- from the GTK Model/View framework.

module ListBox where

import Control.Applicative
import Control.Monad
import Data.IORef

import qualified Graphics.UI.Gtk as Gtk

-- | This type is a record containing the information
-- needed for the functions in this module.
-- The fields are the TreeView and the list to display
data ListBox a = ListBox Gtk.TreeView (Gtk.ListStore String) (IORef [(a, String)])

-- | This function takes a TreeView and a list, and sets up everything
-- so that the TreeView displays the content of the list
newListBox :: Gtk.TreeView -- ^ a previously created widget
           -> [(a, String)] -- ^ a list of values/labels tuples (the labels are shown to the user)
           -> IO (ListBox a) -- ^ the returned record, manipulated by other functions in this module
newListBox treeView assocList = do
  ref <- newIORef assocList
  listStore <- Gtk.listStoreNew (map snd assocList)
  Gtk.set treeView [ Gtk.treeViewModel Gtk.:= listStore, Gtk.treeViewHeadersVisible Gtk.:= False ]
  cell <- Gtk.cellRendererTextNew
  col <- Gtk.treeViewColumnNew
  Gtk.treeViewColumnPackStart col cell False
  Gtk.cellLayoutSetAttributes col cell listStore $ \ind -> [ Gtk.cellText Gtk.:= ind ]
  Gtk.treeViewAppendColumn treeView col
  return $ ListBox treeView listStore ref

onSelectionChanged :: ListBox a -> IO () -> IO ()
onSelectionChanged (ListBox treeView _ _) act = do
  treeSelection <- Gtk.treeViewGetSelection treeView
  Gtk.onSelectionChanged treeSelection act
  return ()

getSelection :: ListBox a -> IO [a]
getSelection (ListBox treeView _ ref) = do
  assocList <- readIORef ref
  treeSelection <- Gtk.treeViewGetSelection treeView
  selectionIndices <- map head <$> Gtk.treeSelectionGetSelectedRows treeSelection
  return $ map fst (getIndices selectionIndices assocList)

-- | Utility function
-- does the same thing as [ (list !! i) | i <- indices ], but faster (O(n) vs. O(n^2))
getIndices :: (Integral i) => [i] -> [a] -> [a]
getIndices is xs = loop is (zip [0..] xs)
    where loop [] _ = []
          loop is@(i:is') ((i',x):ixs')
              | i == i' = x : loop is' ixs'
              | otherwise = loop is ixs'

setSelectionMode :: ListBox a -> Gtk.SelectionMode -> IO ()
setSelectionMode (ListBox treeView _ _) selectionMode =
    flip Gtk.treeSelectionSetMode selectionMode =<< Gtk.treeViewGetSelection treeView

setList :: ListBox a -> [(a, String)] -> IO ()
setList (ListBox treeView listStore ref) assocList = do
  Gtk.listStoreClear listStore
  mapM_ (Gtk.listStoreAppend listStore) (map snd assocList)
  writeIORef ref assocList
