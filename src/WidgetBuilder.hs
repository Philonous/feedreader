{-# LANGUAGE RankNTypes, ScopedTypeVariables, PackageImports, NoMonomorphismRestriction #-}

module WidgetBuilder where

import "mtl"  Control.Monad.Reader

import Data.IORef

import Graphics.UI.Gtk

newtype WidgetAdder =  WA (forall w . WidgetClass w => w -> IO ())

withContainer new action = do
  WA cont <- ask
  c <- liftIO $ new
  res <- lift $ runReaderT action c
  liftIO $ cont c
  return (res,c)

addCont action = do
  container <- ask
  runReaderT action (WA $ containerAdd container)

boxPackS padding style  action = do
  container <- ask
  runReaderT action (WA $ \w -> boxPackStart container w style padding)

boxPackS' style action = boxPackS 0 style action

packGrow = boxPackS' PackGrow
packNatural = boxPackS' PackNatural

addPage name action = do
  container <- ask
  runReaderT action (WA $ \w -> notebookAppendPage container w name >> return () )

withVBoxNew = withContainer (vBoxNew False 0)
withHBoxNew = withContainer (hBoxNew False 0)
withHButtonBoxNew = withContainer (vButtonBoxNew)
withScrolledWindow = withContainer (scrolledWindowNew Nothing Nothing) . addCont

withNotebook = withContainer notebookNew

withAlignment xa ya xs ys = withContainer $ alignmentNew xa ya xs ys

withMainWindow action = do
  w <- windowNew
  a <- runReaderT action (WA $ containerAdd w)
  return (a,w)

addNewWidget new = do
  WA cont <- ask
  w <- liftIO $ new
  liftIO $ cont w
  return w

addLabel = addNewWidget $ labelNew Nothing
addEntry = addNewWidget entryNew

addButton name action = do
  b <- liftIO $ buttonNew
  liftIO $ buttonSetLabel b name
  liftIO $ on b buttonActivated action
  addNewWidget $ return b

addProgressBar = addNewWidget $ progressBarNew

-- add a column to a tree view
addColumn name renderers= do
  (view, model, wrapper) <- ask
  col <- liftIO $ treeViewColumnNew
  liftIO $ set col [treeViewColumnTitle := name]
  forM_ renderers $ \i -> do
    (rend, attr) <- i
    liftIO $ do
      treeViewColumnPackStart col rend True
      attr col model
  liftIO $ treeViewAppendColumn view col
  return col

withNewTreeView wrapper model action = do
  WA cont <- ask
  treeView <- liftIO $ treeViewNewWithModel wrapper
  lift $ runReaderT action (treeView, model, wrapper)
  liftIO $ cont treeView
  return treeView

cLSA rend attr col model = cellLayoutSetAttributes col rend model attr

textRenderer selector = liftIO $ do
  rend <- cellRendererTextNew
  set rend [cellTextEllipsize := EllipsizeEnd]
  return (toCellRenderer rend, cLSA rend $ \row -> [cellText :=> selector row])

textBufferAppendLine buffer entry = do
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end (entry ++ "\n")

createLog = do
  text <- liftIO $ textViewNew
  buffer <- liftIO $ textViewGetBuffer text
  addNewWidget (return text)
  return (text, textBufferAppendLine buffer)

withTreeModelFilter ref createModel = do
  model <- createModel
  treeModelFilter <- treeModelFilterNew model []
  let filter iter = {-# SCC "filter" #-}do
--        iter <- treeModelFilterConvertIterToChildIter treeModelFilter iter'
--        print iter
        path <- treeModelGetPath model iter
        case path of
          [] -> error "empty path"
                             -- not catching this case or returning True will
                             -- crash the program with seg fault.
                             -- This problem only seems to appear
                             -- when adding the first element to an initially
                             -- empty child store
          _  -> do
--                 print path
                 row <- customStoreGetRow model iter
                 filter <- readIORef ref
                 res <- filter row
--                 putStrLn "done filtering this"
                 return res
  treeModelFilterSetVisibleFunc treeModelFilter filter
  return (model, treeModelFilter)



