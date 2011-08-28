{-# LANGUAGE RankNTypes, ScopedTypeVariables, PackageImports, NoMonomorphismRestriction #-}

module WidgetBuilder where

import "mtl"  Control.Monad.Reader

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
withHBoxNew = withContainer (vBoxNew False 0)
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

addButton name action = do
  b <- liftIO $ buttonNew
  liftIO $ buttonSetLabel b name
  liftIO $ on b buttonActivated action
  addNewWidget $ return b

addProgressBar = do
  b <- liftIO $ progressBarNew
  addNewWidget $ return b

-- add a column to a tree view
addColumn name renderers= do
  (view, model) <- ask
  col <- liftIO $ treeViewColumnNew
  liftIO $ set col [treeViewColumnTitle := name]
  forM_ renderers $ \i -> do
    (rend, attr) <- i
    liftIO $ do
      treeViewColumnPackStart col rend True
      --cellLayoutSetAttributes col rend model attr
      attr col model
  liftIO $ treeViewAppendColumn view col
  return col

withNewTreeView model action = do
  WA cont <- ask
  treeView <- liftIO $ treeViewNewWithModel model
  lift $ runReaderT action (treeView, model)
  liftIO $ cont treeView
  return treeView

cLSA rend attr col model = cellLayoutSetAttributes col rend model attr

textRenderer selector = liftIO $ do
  rend <- cellRendererTextNew
  set rend [cellTextEllipsize := EllipsizeEnd]
  return (toCellRenderer rend, cLSA rend $ \row -> [cellText :=> selector row])

