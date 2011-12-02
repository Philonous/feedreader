{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, PackageImports, TypeFamilies, DeriveDataTypeable #-}
module Main where

import           Prelude                   hiding (log, catch)

import           Control.Applicative       ((<$>))
import           Control.Concurrent        (yield)
import           Control.Concurrent.Thread
import           Control.Exception
import           Control.Monad
import "mtl"     Control.Monad.Reader

import           Data.Accessor
import qualified Data.Foldable             as F
import           Data.IORef
import           Data.List                 (sortBy)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Ord                  (comparing)
import           Data.SafeCopy
import qualified Data.Sequence             as S
import qualified Data.Strict.Maybe         as Strict
import qualified Data.Text                 as Text
import           Data.Time
import qualified Data.Traversable          as T
import           Data.Tree

import           Graphics.UI.Gtk           as GTK

import           System.Mem
import           System.Cmd
import           System.Directory

import qualified Backend                   as Store
import           Backend.Acid
import           Backend.Memory
import           Backend.MemoryBuffered
import           Feed
import           GTKFeedStore
import           Keymap
import           UI
import           WidgetBuilder

data ReaderState store = RS
  { feedStore     :: store
  , treeView      :: TreeView
  , filterModel   :: TreeModelFilter
  , statusbar     :: Label
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

forkM xs action = do
  threads <- forM xs $ forkIO . action
  forM threads $ \(_,res) -> res >>= result

updateFeeds = do
  store <- asks feedStore
  feeds <- getFeeds
  bar <- asks statusbar
  logs <- asks logStr
  s <- ask
  liftIO . forkIO $ do
    updates' <- forkM (zip [1..] $ Map.elems feeds) $ \(i,f) -> handle
      ( \ (e :: FeedFetchError) -> ( logs $ "Error while updating: \n"  ++ (show e) )
        >> return Nothing) $ do
      nStories <- newStories f -- DEBUG
      now <- getCurrentTime
      let latest = S.length nStories
      postGUISync . void $ Store.appendStories store (feed f) nStories
      logs $ Text.unpack (name f) ++ " " ++ show latest ++ " new stories."
      return $ Just latest
    let updates = sum $ catMaybes updates'
    postGUISync $ logs ("New stories: " ++ show updates) >> return ()

    {-# SCC "milestone" #-} liftIO $ Store.milestone store
  return ()

addFeed url = {-# SCC "add_feed_func" #-} do
  store <- asks feedStore
  feeds <- getFeeds
  handleError . liftIO . void . forkIO . unless (url `Map.member` feeds) $ do
    newFeed <- {-# SCC "get_feed" #-} feedWithMetadataFromURL url
    {-# SCC "add_to_Store" #-} postGUISync $ Store.addFeed store newFeed
    return ()
  {-# SCC "update_view" #-} updateView
  where
    handleError = flip catchReader (\ (e :: FeedFetchError) -> log ("error: " ++ show e))


getCurrentPath = do
  view <- asks treeView
  (path, _) <- liftIO $ treeViewGetCursor view
  return path

getCurrentRow = do
  store <- asks feedStore
  filter <- asks filterModel
  path' <- getCurrentPath
  path <-  liftIO $ treeModelFilterConvertPathToChildPath filter path'
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
        Strict.Nothing -> return ()
        Strict.Just l -> void . liftIO . system $ "firefox -new-tab " ++ (Text.unpack l)
      setRead True
    CurFeed f -> case home f of
      Strict.Nothing -> return ()
      Strict.Just h -> void . liftIO . system $ "firefox -new-tab " ++ (Text.unpack h)
    _ -> return ()

readerT = flip runReaderT

main = do
  putStrLn "feedreader v1"
  home <- getHomeDirectory
  store <- createAcidFeedStore (home ++ "/.feedreader")
  Store.milestone store
--  buf <- newIORef =<< Store.getFeeds store
  uiMain store -- =<< newIORef (Map.empty :: Store.FeedStorage)
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
         , ((0, "z" ) , liftIO $ performGC )
         , ((0, "l" ) , toggleVisible logWindow )
         , ((0, "r" ) , setRead True)
         , ((0, "u" ) , setRead False)
         , ((alt, "n" ) , noFilter)
         , ((alt, "space" ) , filterReadNotMarkd)
         , ((alt, "backspace" ) , noFilter) -- TODO: find key name
         , ((alt, "m" ) , filterMarkd)
         , ((alt, "r" ) , filterRead)
         , ((0, "h") , lift $ withInput "log" "" log )
         , ((shift, "a") , lift $ withInput "Add Feed" "" (addFeed . Text.pack) )
         , ((0, "space") , toggleExpandCurrent)
         , ((control, "a") , lift $ withInput "Add from file"
                                              "/home/uart14/feeds"
                              (\filename -> do
                                  feeds <- liftIO $ filter (not.null) . lines <$> readFile filename
                                  -- liftIO $ print feeds
                                  forM feeds $ \f -> do
                                     -- liftIO $ putStrLn f
                                     catchReader (addFeed $ Text.pack f)
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
  path <- getCurrentPath
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

uiMain store' = do
  initGUI
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- keep threads alive
  filterFuncRef <- newIORef $ \_ -> return True
  (model, filterModel) <- withTreeModelFilter filterFuncRef $ createGTKFeedStore store'
  let store = GTKStore store' model
  actionRef <- newIORef (const $ return ())
  (((view,s,bar, logWindow, logToWindow, label, entry, inputBox),_),window) <- withMainWindow $ do
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
      bar <- packNatural $ addLabel
      liftIO $ scrolledWindowSetPolicy s PolicyNever PolicyAutomatic
      ((label, entry),inputBox) <- packNatural . withHBoxNew $ do
        label <- packNatural addLabel
        entry <- packGrow    addEntry
        return (label, entry)
      liftIO $ treeViewSetRulesHint v True
      return (v,s,bar, logScroll, logStr, label, entry, inputBox)
  let updateViews = liftIO $ do
        treeModelFilterRefilter filterModel
        widgetQueueDraw view
  let globalState = RS
       { feedStore    =  store
       , treeView     =  view
       , filterModel  =  toTreeModelFilter filterModel
       , statusbar    =  bar
       , logStr       =  \l -> logToWindow l >> GTK.labelSetText bar l
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
  widgetHide logWindow
  widgetHide inputBox
  onDestroy window (Store.milestone store >> mainQuit)
  mainGUI
