module Backend
  ( FeedStoreClass(..)
  , FeedStorage
  ) where

import           Control.Monad

import qualified Data.Foldable as F
import qualified Data.Map      as Map
import qualified Data.Sequence as S
import           Data.Time

import           Feed

type FeedStorage = Map.Map FeedID FeedMetadata

indices s = map fst $ zip [0..] (F.toList s)

class FeedStoreClass a where
  getFeeds      :: a -> IO FeedStorage
  addFeed       :: a -> FeedMetadata -> IO FeedStorage
  appendStories :: a -> FeedID -> S.Seq StoryMetadata -> IO FeedStorage
  setRead       :: a -> Bool -> FeedID -> Int -> IO FeedStorage
  setFeedRead   :: a -> Bool -> FeedID -> IO FeedStorage
  setFeedRead store read feedID = do
    f <- getFeed store feedID
    forM_ (indices $ stories f) $ \s -> setRead store read feedID s
    getFeeds store

  setMarked     :: a -> Bool -> FeedID -> Int -> IO FeedStorage
  setFeedMarked  :: a -> Bool -> FeedID -> IO FeedStorage
  setFeedMarked store marked feedID = do
    f <- getFeed store feedID
    forM_ (indices $ stories f) $ \s -> setMarked store marked feedID s
    getFeeds store

  getFeed       :: a -> FeedID -> IO FeedMetadata
  getFeed a feedID = do
    feeds <- getFeeds a
    return $ feeds Map.! feedID
  milestone     :: a -> IO ()
  milestone a = return ()

