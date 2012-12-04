module OkCancelDialog where

import Control.Monad

import Graphics.UI.Gtk

runOkCancelDialog :: Dialog -> Button -> Button -> IO () -> IO ()
runOkCancelDialog dialog okBtn cancelBtn act = do
  -- All the established signal connections are registered and disconnected at the end,
  -- to prevent a signal being connected twice after runConnectDialog runs twice.
  idOk <- onClicked okBtn $ dialogResponse dialog ResponseOk
  idCancel <- onClicked cancelBtn $ dialogResponse dialog ResponseCancel
  response <- dialogRun dialog
  when (response == ResponseOk) act
  signalDisconnect idOk
  signalDisconnect idCancel
  widgetHide dialog
