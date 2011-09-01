{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, PackageImports, TypeFamilies, DeriveDataTypeable #-}
module Main where

import Prelude hiding (log)

import Control.Applicative((<$>))
import Control.Concurrent
import Control.Monad
import "mtl" Control.Monad.Reader

import Data.Accessor
import Data.Acid
import qualified Data.Foldable as F
import Data.List(sortBy)
import Data.Ord(comparing)
import qualified Data.Map as Map
import Data.IORef
import Data.SafeCopy
import qualified Data.Sequence as S
import qualified Data.Traversable as T

import Graphics.UI.Gtk

import System.Cmd
import System.Directory

import Feed
import UI
import Backend
import WidgetBuilder
import Keymap

data ReaderState = RS
  { gtkStore  :: GTKStore
  , treeView  :: TreeView
  , filterModel :: TreeModelFilter
  , progress  :: ProgressBar
  , logStr    :: String -> IO ()
  , logWindow :: ScrolledWindow
  , acidStore :: AcidState FeedStore
  , filterFuncRef :: IORef ( DisplayFeed  -> IO Bool )
  , updateView' :: ReaderMonad ()
  }


type ReaderMonad a = ReaderT ReaderState IO a

updateView = asks updateView' >>= id

log message = asks logStr >>= \l -> liftIO $ l message

withFeeds action = ask >>= \acid -> liftIO $ do
  feeds <- acidAskFeedStore acid
  result <- action feeds
  acidPutFeedStore acid result

updateFeeds = do
  acid <- asks acidStore
  store <- asks gtkStore
  feeds <- acidAskFeedStore acid
  bar <- asks progress
  logs <- asks logStr
  liftIO . forkIO $ do
    let numStories = fromIntegral $ Map.size feeds
    postGUISync $ widgetShow bar
    forM (zip [1..] $ Map.elems feeds) $ \(i,f) -> do
      postGUISync $ progressBarSetText bar ("Updating " ++ name f)
      nStories <- newStories f
      logs $ name f ++ " : " ++ show (S.length nStories) ++ " new Stories"
      acidAddStories acid (feed f) nStories
      let latest = S.length . stories $ f
      postGUISync $ do
        addStoriesToGTKStore (feed f) (indicesFrom latest nStories) store
        progressBarSetFraction bar (i / numStories)
    postGUISync $ widgetHide bar
    return ()
  liftIO $ createCheckpoint acid
  liftIO $ putStrLn "Update Done."

addFeed url name = do
  acid <- ask
  feeds <- acidAskFeedStore acid
  unless (url `Map.member` feeds) $ liftIO $ do
    newFeed' <- feedWithMetadataFromURL url
    let newFeed = setVal storiesF S.empty newFeed'
    acidAddFeed acid url newFeed

getCurrentRow = do
  view <- asks treeView
  store <- asks gtkStore
  filter <- asks filterModel
  (path, _) <- liftIO $ treeViewGetCursor view
  case path of
    [] -> return Nothing
    ps -> liftIO $ Just <$> treeFilterModelGetValue store filter ps

data Current = CurFeed FeedMetadata | CurStory FeedID StoryMetadata | NoCur

getStore = asks acidStore >>= acidAskFeedStore

getCurrent = do
  row <- getCurrentRow
  store <- getStore
  case row of
    Nothing -> return NoCur
    Just (FeedFeed f) -> return . CurFeed $ store Map.! f
    Just (FeedStory f s) -> return $ CurStory f (S.index (stories $ store Map.! f) s )

openCurrentInBrowser = do
  row <- getCurrent
  case row of
    CurStory _ s -> do
      liftIO . system $ "firefox -new-tab " ++ (ident $ story s)
      setRead True
    CurFeed f -> case home f of
      Nothing -> return ()
      Just h -> void . liftIO . system $ "firefox -new-tab " ++ h
    _ -> return ()

-- updateView =

readerT = flip runReaderT

main = do
  home <- getHomeDirectory
  acid <- openAcidStateFrom (home ++ "/.feedreader") (FeedStore Map.empty)
  createCheckpoint acid
  readerT acid $ do
    addFeed "http://www.lawblog.de/index.php/feed/" "lawlblog"
    addFeed "http://www.teamfortress.com/rss.xml" "tf2"
    addFeed "http://alternativlos.org/alternativlos.rss" "alternativlos"
    addFeed "http://chaosradio.ccc.de/chaosradio-latest.rss" "chaosradio"
  feeds <- acidAskFeedStore acid
  uiMain acid feeds
  return ()

-- modify a columnAccessor so that it does something when the user interacts
-- with the model
onChange (read, write) feedAction storyAction =
  (read, \display new -> fromGTKStore (feedAction new) (storyAction new)  display
                         >> write display new)

readLens acid = onChange (toRead (acidAskFeedStore acid) (const $ return ()) ) (acidFeedRead acid) (acidStoryRead acid)

markdLens acid = onChange (toMarkd (acidAskFeedStore acid) (const $ return ()) ) (acidFeedMarkd acid) (acidStoryMarkd acid)

getL = fst
setL = snd

setRead p = do
  acid <- asks acidStore
  row <- getCurrentRow
  case row of
    Just r -> liftIO $ (setL $ readLens acid) r p
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
  acid <- asks acidStore
  logs <- asks logStr
  liftIO $ writeIORef ref (\row -> do
                logs (show row)
                not <$> (getL $ (readLens acid)) row )
  updateView


myKeymap = mkKeymap
	 [ ((0, "a") , updateFeeds)
	 , ((0, "g" ) , openCurrentInBrowser)
	 , ((0, "l" ) , toggleVisible logWindow )
	 , ((0, "r" ) , setRead True)
	 , ((0, "u" ) , setRead False)
	 , ((0, "n" ) , noFilter)
	 , ((0, "e" ) , lift (log "u") >> filterRead)
	 ]

uiMain acid feedMap = do
  initGUI
  let feedForest = feedsToForest feedMap
  filterFuncRef <- newIORef $ \_ -> return True
  (model, filterModel) <- liftIO . withTreeModelFilter filterFuncRef $ treeStoreNew feedForest
  feedRef <- liftIO $ newIORef $ feedMap
  (((view,s,bar, logWindow, logStr),_),window) <- withMainWindow $ do
    withHBoxNew $ do
      (v,s) <- packGrow . withScrolledWindow . withNewTreeView filterModel model $ do
        nameC <- addColumn "name" [ textRenderer $ toName (acidAskFeedStore acid)]
	liftIO $ set nameC [treeViewColumnExpand := True]
        lastC <- addColumn "last" [ textRenderer $ toDate (acidAskFeedStore acid)]
 	liftIO $ set lastC [treeViewColumnExpand := True]
        addColumn "read"   [ toggleRenderer  $ readLens acid  ]
        addColumn "marked" [ toggleRenderer  $ markdLens acid ]
      ((logWindow, logStr),logScroll) <- packGrow $ withScrolledWindow createLog
      liftIO $ scrolledWindowSetPolicy logScroll PolicyNever PolicyAutomatic
      bar <- packNatural $ addProgressBar
      liftIO $ scrolledWindowSetPolicy s PolicyNever PolicyAutomatic
      return (v,s,bar, logScroll, logStr)
  let updateViews = liftIO $ do
        treeModelFilterRefilter filterModel
        widgetQueueDraw view
  addKeymap window True (readerT $ RS model view (toTreeModelFilter filterModel) bar logStr logWindow acid filterFuncRef updateViews) myKeymap
  windowSetDefaultSize window 800 600
  widgetShowAll window
  widgetHide bar
  widgetHide logWindow
  onDestroy window (createCheckpoint acid >> mainQuit)
  mainGUI

