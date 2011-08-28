{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, PackageImports, TypeFamilies, DeriveDataTypeable #-}
module Main where

import Control.Applicative((<$>))
import Control.Concurrent
import Control.Monad
import "mtl" Control.Monad.Reader
-- import "mtl" Control.Monad.Reader.Class
-- import "mtl" Control.Monad.State.Class
-- import "mtl" Control.Monad.Trans

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
  { gtkStore :: GTKStore
  , treeView :: TreeView
  , progress :: ProgressBar
  , acidStore :: AcidState FeedStore
  }

withFeeds action = ask >>= \acid -> liftIO $ do
  feeds <- acidAskFeedStore acid
  result <- action feeds
  acidPutFeedStore acid result

updateFeeds = do
  acid <- asks acidStore
  store <- asks gtkStore
  feeds <- acidAskFeedStore acid
  bar <- asks progress
  liftIO . forkIO $ do
    let numStories = fromIntegral $ Map.size feeds
    postGUISync $ widgetShow bar
    forM (zip [1..] $ Map.elems feeds) $ \(i,f) -> do
      postGUISync $ progressBarSetText bar ("Updating " ++ name f)
      nStories <- newStories f
      putStrLn $ name f ++ " : " ++ show (S.length nStories) ++ " new Stories"
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
  (path, _) <- liftIO $ treeViewGetCursor view
  case path of
    [] -> return Nothing
    ps -> liftIO $ Just <$> treeStoreGetValue store ps

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
    CurStory _ s -> void . liftIO . system $ "firefox -new-tab " ++ (ident $ story s)
    CurFeed f -> case home f of
      Nothing -> return ()
      Just h -> void . liftIO . system $ "firefox -new-tab " ++ h
    _ -> return ()


readerT = flip runReaderT

main = do
  home <- getHomeDirectory
  state <- openAcidStateFrom (home ++ "/.feedreader") (FeedStore Map.empty)
  createCheckpoint state
  readerT state $ do
    addFeed "http://www.lawblog.de/index.php/feed/" "lawlblog"
    addFeed "http://www.teamfortress.com/rss.xml" "tf2"
    addFeed "http://alternativlos.org/alternativlos.rss" "alternativlos"
    addFeed "http://chaosradio.ccc.de/chaosradio-latest.rss" "chaosradio"
--    deleteNewest
  feeds <- acidAskFeedStore state
  uiMain state feeds
  return ()

-- modify a columnAccessor so that it does something when the user interacts
-- with the model
onChange (read, write) feedAction storyAction =
  (read, \display new -> fromGTKStore (feedAction new) (storyAction new)  display
                         >> write display new)

readCol acid = onChange (toRead (acidAskFeedStore acid) (const $ return ()) ) (acidFeedRead acid) (acidStoryRead acid)

markdCol acid = onChange (toMarkd (acidAskFeedStore acid) (const $ return ()) ) (acidFeedMarkd acid) (acidStoryMarkd acid)

myKeymap = mkKeymap
	 [ ((0, "u") , lift $ updateFeeds)
	 , ((0, "g" ) , lift . void $ openCurrentInBrowser)
	 ]

uiMain acid feedMap = do
  initGUI
  let feedForest = feedsToForest feedMap
  model <- liftIO $ treeStoreNew feedForest
  feedRef <- liftIO $ newIORef $ feedMap
  (((view,s,bar),_),window) <- withMainWindow $ do
    withHBoxNew $ do
      (v,s) <- packGrow . withScrolledWindow . withNewTreeView model $ do
        nameC <- addColumn "name" [ textRenderer $ toName (acidAskFeedStore acid)]
        liftIO $ set nameC [treeViewColumnExpand := True]
        lastC <- addColumn "last" [ textRenderer $ toDate (acidAskFeedStore acid)]
 	liftIO $ set lastC [treeViewColumnExpand := True]
        addColumn "read"   [ toggleRenderer  $ readCol acid  ]
        addColumn "marked" [ toggleRenderer  $ markdCol acid ]
      bar <- packNatural $ addProgressBar
      liftIO $ scrolledWindowSetPolicy s PolicyNever PolicyAutomatic
      return (v,s,bar)

  addKeymap window True (readerT $ RS model view bar acid) myKeymap

  windowSetDefaultSize window 800 600
  widgetShowAll window
  widgetHide bar
  onDestroy window (createCheckpoint acid >> mainQuit)
  mainGUI

