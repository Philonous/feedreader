{-# LANGUAGE TemplateHaskell, PackageImports, KindSignatures, TypeFamilies, DeriveDataTypeable #-}

module DataBackend where

import Control.Applicative
import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.State.Class

import Data.Acid
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Set
import Feed

-- $(deriveSafeCopy 0 'base ''FeedMetadata)
-- $(deriveSafeCopy 0 'base ''Story)
-- $(deriveSafeCopy 0 'base ''StoryMetadata)

$(deriveSafeCopy 0 'base ''FeedStore)

putFeedStore ::(Map.Map FeedID FeedMetadata) -> Update FeedStore ()
putFeedStore = put . FeedStore

askFeedStore :: Query FeedStore (Map.Map FeedID FeedMetadata)
askFeedStore = fromFeedStore <$> ask

$(makeAcidic ''FeedStore ['putFeedStore , 'askFeedStore ] )




