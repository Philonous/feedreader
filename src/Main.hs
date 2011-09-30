{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, PackageImports, TypeFamilies, DeriveDataTypeable #-}
module Main where

import           Prelude              hiding (log, catch)

import           Control.Applicative  ((<$>))
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import "mtl"     Control.Monad.Reader

import           Data.Accessor
import qualified Data.Foldable        as F
import           Data.IORef
import           Data.List            (sortBy)
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.Ord             (comparing)
import           Data.SafeCopy
import qualified Data.Sequence        as S
import           Data.Time
import qualified Data.Traversable     as T
import           Data.Tree

import           Graphics.UI.Gtk      as GTK

import           System.Cmd
import           System.Directory

import           Feed
import           UI
import qualified Backend              as Store
import           Backend.Acid
import           WidgetBuilder
import           Keymap
import           GTKFeedStore

data ReaderState store = RS
  { feedStore     :: store
  , treeView      :: TreeView
  , progress      :: ProgressBar
  , logStr        :: String -> IO ()
  , logWindow     :: ScrolledWindow
  , filterFuncRef :: IORef ( DisplayFeed  -> IO Bool )
  , updateView'   :: ReaderMonad store ()
  , inputBox      :: HBox
  , inputLabel    :: Label
  , inputEntry    :: Entry
  , inputAction   :: IORef (String -> ReaderMonad store ())
  }


type ReaderMonad store a = ReaderT (ReaderState store) IO a

updateView = asks updateView' >>= id

log message = asks logStr >>= \l -> liftIO $ l message
catchReader a h = do
  r <- ask
  liftIO $ catch (runReaderT a r) (\e -> runReaderT (h e) r)

getFeeds = asks feedStore >>= liftIO . Store.getFeeds

updateFeeds = do
  store <- asks feedStore
  feeds <- getFeeds
  bar <- asks progress
  logs <- asks logStr
  s <- ask
  liftIO . forkIO $ do
    let numFeeds = fromIntegral $ Map.size feeds
    postGUIAsync $ widgetShow bar
    updates <- liftM catMaybes . forM (zip [1..] $ Map.elems feeds) $ \(i,f) -> handle
      ( \ (e :: FeedFetchError) -> ( logs $ "Error while updating: \n"  ++ (show e) )
        >> return Nothing) $ do
      postGUIAsync $ progressBarSetText bar ("Updating " ++ name f)
      nStories <- newStories f
      now <- getCurrentTime
      let latest = S.length nStories
      postGUIAsync $ do
        Store.appendStories store (feed f) nStories
        progressBarSetFraction bar (i / numFeeds)
      return $ Just latest
    postGUISync $ widgetHide bar >> logs ("New stories: " ++ show (sum updates)) >> return ()
    return ()
  liftIO $ Store.milestone store

addFeed url = do
  store <- asks feedStore
  feeds <- getFeeds
  handleError . unless (url `Map.member` feeds) . liftIO $ do
    newFeed <- feedWithMetadataFromURL url
    Store.addFeed store newFeed
    return ()
  updateView
  where
    handleError = flip catchReader (\ (e :: FeedFetchError) -> log ("error: " ++ show e))


getCurrentPath = do
  view <- asks treeView
  liftIO $ treeViewGetCursor view

getCurrentRow = do
  store <- asks feedStore
  (path, _) <- getCurrentPath
  case path of
    [] -> return Nothing
    ps -> liftIO $ getRowFromPath store ps

data Current = CurFeed FeedMetadata | CurStory FeedID StoryMetadata | NoCur
  deriving (Show)

getCurrent = do
  row <- getCurrentRow
  feeds <- getFeeds
  case row of
    Nothing -> return NoCur
    Just (FeedFeed f) -> return . CurFeed $ f
    Just (FeedStory f s) -> return $ CurStory (feed f) (S.index (stories $ feeds Map.! (feed f)) s )

openCurrentInBrowser = do
  row <- getCurrent
  case row of
    CurStory _ s -> do
      case link $ story s of
        Nothing -> return ()
        Just l -> void . liftIO . system $ "firefox -new-tab " ++ l
      setRead True
    CurFeed f -> case home f of
      Nothing -> return ()
      Just h -> void . liftIO . system $ "firefox -new-tab " ++ h
    _ -> return ()

readerT = flip runReaderT

main = do
  putStrLn "feedreader v1"
  home <- getHomeDirectory
  store <- createAcidFeedStore (home ++ "/.feedreader")
  Store.milestone store
  uiMain store
  return ()

-- modify a columnAccessor so that it does something when the user interacts
-- with the model
onChange (read, write) feedAction storyAction =
  (read, \display new -> fromGTKStore (feedAction new) (storyAction new)  display
                         >> write display new)

readLens store = onChange (toRead (Store.getFeeds store) (const $ return ()) ) ( Store.setFeedRead store) ( Store.setRead store)

markdLens store = onChange (toMarkd (Store.getFeeds store) (const $ return ()) ) ( Store.setFeedMarked store) ( Store.setMarked store)

getL = fst
setL = snd

setRead p = do
  store <- asks feedStore
  row <- getCurrentRow
  case row of
    Just r -> liftIO $ (setL $ readLens store) r p
    _      -> return ()
  lift $ updateView

toggleVisible w = do
  widget <- asks w
  liftIO $ set widget [widgetVisible :~ not]

noFilter = lift $ do
  ref <- asks filterFuncRef
  liftIO $ writeIORef ref (\_ -> return True)
  updateView

filterRead = lift $ do
  ref <- asks filterFuncRef
  store <- asks feedStore
  logs <- asks logStr
  liftIO $ writeIORef ref (\row -> do
--                logs (show row)
                not <$> (getL $ (readLens store)) row )
  updateView

filterMarkd = lift $ do
  ref <- asks filterFuncRef
  store <- asks feedStore
  logs <- asks logStr
  liftIO $ writeIORef ref (\row ->
--                logs (show row)
                  (getL $ (markdLens store)) row )
  updateView

filterReadNotMarkd = lift $ do
  ref <- asks filterFuncRef
  store <- asks feedStore
  logs <- asks logStr
  liftIO $ writeIORef ref (\row -> do
                  read <- (getL $ (readLens store)) row
                  markd <- (getL $ (markdLens store)) row
                  return $ not read || markd
                  )
  updateView


showCurrent = getCurrent >>= liftIO . print

myKeymap = mkKeymap
         [ ((control, "u") , updateFeeds)
         , ((0, "g" ) , openCurrentInBrowser)
         , ((shift, "g" ) , showCurrent )
         , ((0, "l" ) , toggleVisible logWindow )
         , ((0, "r" ) , setRead True)
         , ((0, "u" ) , setRead False)
         , ((alt, "n" ) , noFilter)
         , ((alt, "space" ) , filterReadNotMarkd)
         , ((alt, "backspace" ) , noFilter) -- TODO: find key name
         , ((alt, "m" ) , filterMarkd)
         , ((alt, "r" ) , filterRead)
         , ((0, "h") , lift $ withInput "log" "" log )
         , ((shift, "a") , lift $ withInput "Add Feed" "" addFeed )
         , ((0, "space") , toggleExpandCurrent)
         , ((control, "a") , lift $ withInput "Add from file"
                                              "/home/uart14/feeds"
                              (\filename -> do
                                  feeds <- liftIO $ filter (not.null) . lines <$> readFile filename
                                  liftIO $ print feeds
                                  forM feeds $ \f -> do
                                     liftIO $ putStrLn f
                                     catchReader (addFeed f)
                                        ( \(e :: SomeException) ->
                                            liftIO $ print e)
                                  return () ))
         ]


---------------------------
-- Input ------------------
---------------------------

activateInput prefill= do
  bar <- asks inputEntry
  container <- asks inputBox
  liftIO $ do
    GTK.widgetShow container
  -- grabbing focus seems to select the whole content of the entry
  -- so we do it first and remove the selection afterwards
    GTK.widgetGrabFocus bar
    set bar [ GTK.entryText := prefill
            , GTK.editablePosition := (-1)
            ]

deactivateInput = do
  widget <- asks inputBox
  view <- asks treeView
  actionRef <- asks inputAction
  liftIO $ do
    GTK.widgetHide widget
    GTK.widgetGrabFocus view
    writeIORef actionRef (const $ return ())

withInput labelText prefill action =  do
  label <- asks inputLabel
  actionRef <- asks inputAction
  liftIO $ writeIORef actionRef action
  liftIO $ GTK.labelSetText label labelText
  activateInput prefill

withInput' t p a = do
  r <- ask
  withInput t p (runReaderT a r)

toggleExpandCurrent = do
  (path , _) <- getCurrentPath
  view <- asks treeView
  expandet <- liftIO $ treeViewRowExpanded view path
  liftIO $ case expandet of
    True -> treeViewCollapseRow view path
    False -> treeViewExpandRow view path False
  return ()

-- showInfoLabel text = do
--   label <- asks infoLabel
--   liftIO $ do
--     GTK.labelSetText label text
--     GTK.widgetShow label

-- hideInfoLabel = do
--   asks infoLabel >>= liftIO . GTK.widgetHide

-- withInfoLabelInput info label prefill action = do
--   showInfoLabel info
--   withInput label prefill $ \x ->  action x >> hideInfoLabel

------------------------
-- UI Main -------------
------------------------

uiMain :: AcidFeedStore -> IO ()
uiMain store' = do
  initGUI
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- keep threads alive
  filterFuncRef <- newIORef $ \_ -> return True
  (model, filterModel) <- withTreeModelFilter filterFuncRef $ createGTKFeedStore store'
  let store = GTKStore store' model
  actionRef <- newIORef (const $ return ())
  (((view,s,bar, logWindow, logStr, label, entry, inputBox),_),window) <- withMainWindow $ do
    withVBoxNew $ do
      (v,s) <- packGrow . withScrolledWindow . withNewTreeView filterModel model $ do
        nameC <- addColumn "name" [ textRenderer $ toName (Store.getFeeds store)]
        liftIO $ set nameC [treeViewColumnExpand := True]
        lastC <- addColumn "last" [ textRenderer $ toDate (Store.getFeeds store)]
        liftIO $ set lastC [treeViewColumnExpand := True]
        addColumn "read"   [ toggleRenderer  $ readLens store  ]
        addColumn "marked" [ toggleRenderer  $ markdLens store ]
      ((logWindow, logStr),logScroll) <- packGrow $ withScrolledWindow createLog
      liftIO $ scrolledWindowSetPolicy logScroll PolicyNever PolicyAutomatic
      bar <- packNatural $ addProgressBar
      liftIO $ scrolledWindowSetPolicy s PolicyNever PolicyAutomatic
      ((label, entry),inputBox) <- packNatural . withHBoxNew $ do
        label <- packNatural addLabel
        entry <- packGrow    addEntry
        return (label, entry)
      return (v,s,bar, logScroll, logStr, label, entry, inputBox)
  let updateViews = liftIO $ do
        treeModelFilterRefilter filterModel
        widgetQueueDraw view
  let globalState = RS
       { feedStore    =  store
       , treeView     =  view
       , progress     =  bar
       , logStr       =  logStr
       , logWindow    =  logWindow
       , filterFuncRef=  filterFuncRef
       , updateView'  =  updateViews
       , inputBox     =  inputBox
       , inputLabel   =  label
       , inputEntry   =  entry
       , inputAction  =  actionRef
       }
  on entry entryActivate $ do
    text <- entryGetText entry
    action <- readIORef actionRef
    readerT globalState $ do
      deactivateInput
      action text
  addKeymap view True (readerT globalState )  myKeymap
  windowSetDefaultSize window 800 600
  widgetShowAll window
  widgetHide bar
  widgetHide logWindow
  widgetHide inputBox
  onDestroy window (Store.milestone store >> mainQuit)
  mainGUI
