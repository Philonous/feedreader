{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, PackageImports, TypeFamilies, DeriveDataTypeable #-}

module Backend.Acid ( AcidFeedStore
                    , createAcidFeedStore
                    ) where

import           Control.Applicative        ((<$>))
import "mtl"     Control.Monad.Reader.Class
import "mtl"     Control.Monad.State.Class
import "mtl"     Control.Monad.Trans

import           Data.Accessor
import           Data.Acid
import qualified Data.Map                   as Map
import           Data.SafeCopy
import qualified Data.Sequence              as S
import qualified Data.Strict.Maybe          as Strict
import           Data.Time
import           Data.Typeable

import           Backend
import           Feed

newtype FeedStore = FeedStore {fromFeedStore :: Map.Map FeedID FeedMetadata }
  deriving (Typeable)

newtype AcidFeedStore = AcidFeedStore (AcidState FeedStore)

deriveSafeCopy 0 'base ''Strict.Maybe
deriveSafeCopy 0 'base ''Story
deriveSafeCopy 0 'base ''StoryMetadata
deriveSafeCopy 0 'base ''FeedMetadata
deriveSafeCopy 0 'base ''FeedStore

putFeedStore ::(Map.Map FeedID FeedMetadata) -> Update FeedStore ()
putFeedStore = put . FeedStore

askFeedStore :: Query FeedStore (Map.Map FeedID FeedMetadata)
askFeedStore = fromFeedStore <$> ask

modFeedStore :: (Map.Map FeedID FeedMetadata -> Map.Map FeedID FeedMetadata)
                -> Update FeedStore (Map.Map FeedID FeedMetadata)
modFeedStore f = do
  modify $ FeedStore . f . fromFeedStore
  fromFeedStore `fmap` get


insertIfNew = Map.insertWith (flip const)

addFeed' url feed = modFeedStore $ insertIfNew url feed

addStories now feed newStories = modFeedStore $ Map.adjust (lastF ^= now) feed .
                                   Map.adjust (storiesF ^: ( newStories S.><)) feed

modStory f feed story = modFeedStore $ Map.adjust (storiesF ^: S.adjust f story) feed

setStoryRead = modStory . setVal readF
setFeedRead feed = modFeedStore . setVal (allA readF <. storiesF <. unsafeMap feed)

setStoryMarkd = modStory . setVal markdF
setFeedMarkd feed = modFeedStore . setVal (allA markdF <. storiesF <. unsafeMap feed)


makeAcidic ''FeedStore [ 'putFeedStore
                       , 'askFeedStore
                       , 'addFeed'
                       , 'addStories
                       , 'setStoryRead
                       , 'setFeedRead
                       , 'setStoryMarkd
                       , 'setFeedMarkd
                       ]

acidPutFeedStore acid store = update' acid $ PutFeedStore store
acidAskFeedStore
  :: MonadIO m =>
     AcidState (EventState AskFeedStore) -> m (Map.Map FeedID FeedMetadata)
acidAskFeedStore acid = query' acid AskFeedStore
acidAddFeed acid url feed = update' acid $ AddFeed' url feed
acidAddStories acid feed newStories = do
  now <- liftIO $ getCurrentTime
  update' acid $ AddStories now feed newStories

acidStoryRead acid newVal feed story = update' acid $ SetStoryRead newVal feed story
acidStoryMarkd acid newVal feed story  = update' acid $ SetStoryMarkd newVal feed story
acidFeedRead acid newVal feed  = update' acid $ SetFeedRead feed newVal
acidFeedMarkd acid newVal feed = update' acid $ SetFeedMarkd feed newVal

createAcidFeedStore path = do
  acid <- openAcidStateFrom path (FeedStore Map.empty)
  return $ AcidFeedStore acid


instance FeedStoreClass AcidFeedStore where
  getFeeds      (AcidFeedStore a)    = acidAskFeedStore a
  addFeed       (AcidFeedStore a) f  = acidAddFeed      a (feed f) f
  appendStories (AcidFeedStore a)    = acidAddStories   a
  setRead       (AcidFeedStore a)    = acidStoryRead    a
  setMarked     (AcidFeedStore a)    = acidStoryMarkd   a
  milestone     (AcidFeedStore a)    = createCheckpoint a