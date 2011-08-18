{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, PackageImports, TypeFamilies, DeriveDataTypeable #-}

module Backend where

import Data.Acid(makeAcidic)
import Data.Acid
import Data.Accessor
import qualified Data.Map as Map
import Data.SafeCopy

import Control.Applicative((<$>))
import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.State.Class
import "mtl" Control.Monad.Trans

import Feed

deriveSafeCopy 0 'base ''Story
deriveSafeCopy 0 'base ''StoryMetadata
deriveSafeCopy 0 'base ''FeedMetadata
deriveSafeCopy 0 'base ''FeedStore

putFeedStore ::(Map.Map FeedID FeedMetadata) -> Update FeedStore ()
putFeedStore = put . FeedStore

askFeedStore :: Query FeedStore (Map.Map FeedID FeedMetadata)
askFeedStore = fromFeedStore <$> ask

modFeedStore :: (Map.Map FeedID FeedMetadata -> Map.Map FeedID FeedMetadata)
                -> Update FeedStore ()
modFeedStore f = modify $ FeedStore . f . fromFeedStore

insertIfNew = Map.insertWith (flip const)

mkFeed url name = Feed url name Nothing Map.empty

addFeed' url feed = modFeedStore $ insertIfNew url feed

addStories feed newStories = modFeedStore $
                             Map.adjust (storiesF ^: Map.union newStories) feed

modStory f feed story = modFeedStore $ Map.adjust (storiesF ^: Map.adjust f story) feed

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
acidAddStories acid feed newStories = update' acid $ AddStories feed newStories

acidStoryRead acid newVal feed story = update' acid $ SetStoryRead newVal feed story
acidStoryMarkd acid newVal feed story  = update' acid $ SetStoryMarkd newVal feed story
acidFeedRead acid newVal feed  = update' acid $ SetFeedRead feed newVal
acidFeedMarkd acid newVal feed = update' acid $ SetFeedMarkd feed newVal
