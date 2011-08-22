{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, PackageImports, TypeFamilies, DeriveDataTypeable #-}
module Main where

import Control.Applicative((<$>))
import Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.State.Class
import "mtl" Control.Monad.Trans

import Data.Accessor
import Data.Acid
import Control.Concurrent
import qualified Data.Foldable as F
import Data.List(sortBy)
import Data.Ord(comparing)
import qualified Data.Map as Map
import Data.IORef
import Data.SafeCopy
import qualified Data.Sequence as S
import qualified Data.Traversable as T

import Graphics.UI.Gtk

import System.Directory

import Feed
import UI
import Backend

withFeeds action = ask >>= \acid -> liftIO $ do
  feeds <- acidAskFeedStore acid
  result <- action feeds
  acidPutFeedStore acid result

updateFeeds store = do
  acid <- ask
  feeds <- acidAskFeedStore acid
  T.forM feeds $ \f -> liftIO . forkIO $ do
    nStories <- newStories f
    putStrLn $ name f ++ " : " ++ show (S.length nStories) ++ " new Stories"
    acidAddStories acid (feed f) nStories
    let latest = S.length . stories $ f
    postGUISync $ addStoriesToDisplay (feed f) (indicesFrom latest nStories) store
  liftIO $ createCheckpoint acid
  liftIO $ putStrLn "Update Done."

addFeed url name = do
  acid <- ask
  feeds <- acidAskFeedStore acid
  unless (url `Map.member` feeds) $ liftIO $ do
    newFeed' <- feedWithMetadataFromURL url
    let newFeed = setVal storiesF S.empty newFeed'
    acidAddFeed acid url newFeed

readerT = flip runReaderT

main = do
  home <- getHomeDirectory
  state <- openAcidStateFrom (home ++ "/.feedreader") (FeedStore Map.empty)
  createCheckpoint state
  readerT state $ do
    addFeed "http://www.lawblog.de/index.php/feed/" "lawblog"
    addFeed "http://www.teamfortress.com/rss.xml" "tf2"
    addFeed "http://ivoras.sharanet.org/blog/rss2.xml" "arrow of time"
--    deleteNewest
  feeds <- acidAskFeedStore state
  uiMain state feeds
  return ()

-- modify a columnAccessor so that it does something when the user interacts
-- with the model
onChange (read, write) feedAction storyAction =
  (read, \display new -> fromDisplay (feedAction new) (storyAction new)  display
                         >> write display new)

readCol acid = onChange (toRead (acidAskFeedStore acid) (const $ return ()) ) (acidFeedRead acid) (acidStoryRead acid)

markdCol acid = onChange (toMarkd (acidAskFeedStore acid) (const $ return ()) ) (acidFeedMarkd acid) (acidStoryMarkd acid)

uiMain acid feedMap = do
  initGUI
  let feedForest = feedsToForest feedMap
  model <- liftIO $ treeStoreNew feedForest
  feedRef <- liftIO $ newIORef $ feedMap
  window <- withMainWindow  $ do
    withNotebook $ do
      (_,s) <- addPage "feeds" $ withScrolledWindow . withNewTreeView model $ do
        nameC <- addColumn "name" [ textRenderer $ toName (acidAskFeedStore acid)]
        liftIO $ set nameC [treeViewColumnExpand := True]
        addColumn "read"   [ toggleRenderer  $ readCol acid    ]
        addColumn "marked" [ toggleRenderer  $ markdCol acid ]
      liftIO $ scrolledWindowSetPolicy s PolicyNever PolicyAutomatic
      liftIO $ set s [containerResizeMode := ResizeQueue]
      button <- addPage "conf" $ addButton "update" .
        readerT acid $ do
          updateFeeds model
          return ()
      return ()
  widgetShowAll window
  onDestroy window (createCheckpoint acid >> mainQuit)
  mainGUI

-- Idee: Widgets in Writer, withContainer übernimmt add