{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, DeriveDataTypeable, PackageImports, ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}

module Feed where

import Prelude hiding(catch)

import Control.Applicative
import Control.Exception
import Control.Monad
import "mtl" Control.Monad.Trans

import Data.Acid
import Data.Either

import Data.Accessor
import qualified Data.Accessor.Container as C
import Data.Accessor.Template
import qualified Data.Foldable as F
import Data.Function
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Sequence as S
import Data.Time
import qualified Data.Time.RFC822 as RFC822
import Data.Typeable

import Network.HTTP
import Network.URI

import System.IO.Error(isUserError)

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types

type StoryID = String

data Story = Story
   { ident  :: StoryID
   , title  :: Maybe String
   , descr  :: Maybe String
   , link   :: Maybe String
   , date   :: Maybe UTCTime
   } deriving (Typeable, Eq, Show)

nameDeriveAccessors ''Story (Just . (++"F"))

data StoryMetadata = SMD
   { story   :: Story
   , fetched :: UTCTime
   , read    :: Bool
   , markd   :: Bool
   } deriving (Show)

instance Eq StoryMetadata where
  (==) = (==) `on` story

nameDeriveAccessors ''StoryMetadata (Just . (++"F"))

type FeedID = String

data FeedMetadata = Feed
            { feed    :: FeedID
            , name    :: String
            , home    :: Maybe String
            , last    :: UTCTime
            , stories :: S.Seq StoryMetadata
    } deriving (Typeable, Show)

instance Eq FeedMetadata where
  (==) = (==) `on` feed

instance Ord FeedMetadata where
  compare = comparing feed

nameDeriveAccessors ''FeedMetadata (Just . (++"F"))

newtype FeedStore = FeedStore {fromFeedStore :: M.Map FeedID FeedMetadata }
  deriving (Typeable)

orIfEQ p q a b = case p a b of
  EQ -> q a b
  x  -> x

refining = foldr1 orIfEQ

instance Ord Story where
  compare = refining [ comparing date
                     , comparing link
                     , comparing title
                     ]

instance Ord StoryMetadata where
  compare = refining [comparing $ date.story
                     , comparing fetched
                     , comparing story
                     ]

data FeedFetchError = FeedParseError | URLError String | HTTPError String
			deriving (Show, Typeable)
instance Exception FeedFetchError

infixl 1 $.
($.) = ($)

mapf = flip fmap

feedToStories :: Feed -> [Story]
feedToStories feed = catMaybes . map feedItemToStory
                     $ getFeedItems feed

feedItemToStory item = getItemId item `mapf` \iid -> Story
                                    $. snd iid
                                    $. getItemTitle item
                                    $. getItemDescription item
                                    $. getItemLink item
                                    $. (zonedTimeToUTC <$>
                                        (RFC822.parse =<< getItemDate item))

feedFromURL :: String -> IO Feed
feedFromURL url = do
  putStrLn "http..."
  httpData <- case parseURI url of
    Nothing -> throwIO . HTTPError $ "Not a valid URL: \"" ++ url ++ "\""
    Just u -> catch (simpleHTTP (mkRequest GET u))
                    (\(e :: IOError) -> if isUserError e then
                     throwIO $ HTTPError "connection error"
                     else throwIO e )
  putStrLn "response and parsing"
  feed <- parseFeedString <$> getResponseBody httpData
  case feed of
    Nothing -> throwIO FeedParseError
    Just f  -> return f

feedWithMetadataFromURL url = do
  feed <- feedFromURL url
  let stories = feedToStories feed
  now <- getCurrentTime
  let storiesMetadata = S.fromList . mapf stories
                        $ \s -> SMD s now False False
  return $ Feed url (getFeedTitle feed) (getFeedHome feed) now storiesMetadata

grabFeeds urls = do
  feeds <- mapM feedWithMetadataFromURL urls
  return $ S.fromList (zip urls feeds)

seqDiff a b = S.filter (`F.notElem` b) a

newStories f = do
  feed <- feedWithMetadataFromURL (feed f)
  return (stories feed `seqDiff` stories f)

-- data TransactionLog = forall x. UpdateEvent x => TL [ x ]

-- updateFeeds = ask >>= \acid -> liftIO $ do
--   feeds <- query acid AskFeedStore
--   forM_ (Map.assocs feeds) $ \(key, val) -> do
--     fetchedStories <- feedToStories <$> feedFromURL (feed val)
--     let newStories = Map.fromList fetchedStories Map.\\ stories(feeds Map.! key)
--     update acid AddStories newStories


indexError = error "index not found"
unsafeMap = C.mapDefault indexError

-- access bool-fields in a map combinedly
allA acc = accessor
  (F.all (getVal acc))
  (fmap . setVal acc )

-- Accessor for values wrapped in a maybe
maybeA def = accessor (fromMaybe def) (\v _ -> Just v)
