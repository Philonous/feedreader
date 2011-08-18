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
import Data.List(sortBy)
import Data.Ord(comparing)
import qualified Data.Map as Map
import Data.IORef
import Data.SafeCopy
import qualified Data.Traversable as T

import Graphics.UI.Gtk

import Feed
import UI
import Backend

withFeeds action = ask >>= \acid -> liftIO $ do
  feeds <- acidAskFeedStore acid
  result <- action feeds
  acidPutFeedStore acid result

updateFeeds = do
  acid <- ask
  feeds <- acidAskFeedStore acid
  T.forM feeds $ \f -> liftIO $ do
    stories <- newStories f
    print stories
    acidAddStories acid (feed f) stories

addFeed url name = do
  acid <- ask
  feeds <- acidAskFeedStore acid
  unless (url `Map.member` feeds) $ liftIO $ do
    newFeed' <- feedMetadataFromURL url
    let newFeed = setVal storiesF Map.empty newFeed'
    acidAddFeed acid url newFeed

updateTVModel model = do
  acid <- ask
  feeds <- acidAskFeedStore acid
  liftIO $ do
    joinTree (feedsToForest feeds) model []
readerT = flip runReaderT

deleteNewest = do
  acid <- ask
  store <- acidAskFeedStore acid
  acidPutFeedStore acid $ Map.map (storiesF ^: (\stories -> let newest = take 3 . reverse . sortBy (comparing snd) . Map.toList $ stories in stories Map.\\ (Map.fromList newest))) store

main = do
  state <- openAcidState (FeedStore Map.empty)
  readerT state $ do
    addFeed "http://www.lawblog.de/index.php/feed/" "lawblog"
    addFeed "http://www.teamfortress.com/rss.xml" "tf2"
    addFeed "http://ivoras.sharanet.org/blog/rss2.xml" "arrow of time"
    deleteNewest
  feeds <- acidAskFeedStore state
  uiMain state feeds
  return ()

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
  window <- withMainWindow $ do
    withHBoxNew $ do
      treeView <- withNewTreeView model $ do
        addColumn "name"   [ textRenderer $ toName (acidAskFeedStore acid)]
        addColumn "read"   [ toggleRenderer  $ readCol acid    ]
        addColumn "marked" [ toggleRenderer  $ markdCol acid ]
      button <- addButton "update" $ do
        readerT acid $ do
          updateFeeds
          updateTVModel model
          return ()
      return ()
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI
