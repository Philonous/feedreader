{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, DeriveDataTypeable, PackageImports, ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}

module Feed where

import Prelude hiding(catch)

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import "mtl"     Control.Monad.Trans

import           Data.Acid
import           Data.Either

import           Data.Accessor
import qualified Data.Accessor.Container as C
import           Data.Accessor.Template
import           Data.ByteString         (ByteString)
import qualified Data.Foldable           as F
import           Data.Function
import qualified Data.Map                as M
import           Data.Ord
import qualified Data.Sequence           as S
import qualified Data.Strict             as Strict
import qualified Data.Text               as Text
import           Data.Text               (Text)
import           Data.Time
import qualified Data.Time.RFC822        as RFC822
import           Data.Typeable

import           Network.HTTP
import           Network.URI

import           System.IO.Error         (isUserError)

import           Text.Feed.Constructor
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.Feed.Types
import qualified Text.XML.Light          as XML

type StoryID = Text

data Story = Story
   { ident  :: StoryID
   , title  :: Strict.Maybe Text
   , descr  :: Strict.Maybe Text
   , link   :: Strict.Maybe Text
   , date   :: Strict.Maybe UTCTime
   } deriving (Typeable, Show)

instance Eq Story where
  (==) = (==) `on` ident

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

type FeedID = Text

data FeedMetadata = Feed
            { feed    :: !FeedID
            , name    :: !Text
            , home    :: !(Strict.Maybe Text)
            , last    :: !UTCTime
            , stories :: !(S.Seq StoryMetadata)
    } deriving (Typeable, Show)

instance Eq FeedMetadata where
  (==) = (==) `on` feed

instance Ord FeedMetadata where
  compare = comparing feed

nameDeriveAccessors ''FeedMetadata (Just . (++"F"))

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

strictCatMaybes [] = []
strictCatMaybes (Just x : xs) = x : strictCatMaybes xs
strictCatMaybes (Nothing : xs) = strictCatMaybes xs

feedToStories :: Feed -> [Story]
feedToStories feed = strictCatMaybes . map feedItemToStory
                     $ getFeedItems feed

toStrict Nothing = Strict.Nothing
toStrict (Just x) = Strict.Just x

toStrictText = toStrict . fmap Text.pack

feedItemToStory item = getItemId item `mapf` \iid -> Story
                                    $. Text.pack (snd iid)
                                    $. toStrictText (getItemTitle item)
                                    $. toStrictText (getItemDescription item)
                                    $. toStrictText (getItemLink item)
                                    $. (zonedTimeToUTC <$> toStrict
                                        (RFC822.parse =<< getItemDate item))

feedFromURL :: Text -> IO Feed
feedFromURL url = {-# SCC "feedFromURL" #-} do
  let url' = Text.unpack url
  putStrLn "http..."
  httpData <- {-# SCC "parseURI" #-} case parseURI url' of
    Nothing -> throwIO . HTTPError $ "Not a valid URL: \"" ++ url' ++ "\""
    Just u -> {-# SCC "connect" #-} catch (simpleHTTP (mkRequest GET u))
                    (\(e :: IOError) -> if isUserError e then
                     throwIO $ HTTPError "connection error"
                     else throwIO e )
  putStrLn "http response and parsing"
  unparsedFeed <- getResponseBody httpData :: IO ByteString
  let xml = {-# SCC "parseXMLDoc" #-} XML.parseXMLDoc unparsedFeed
  feed <- case xml of
    Nothing -> throwIO FeedParseError
    Just e  -> return $ readAtom e `mplus`
                        readRSS2 e `mplus`
                        readRSS1 e `mplus`
                        Just (XMLFeed e)
  case feed of
    Nothing -> throwIO FeedParseError
    Just f  ->
      putStrLn (url' ++ " is " ++ show (getFeedKind f))
      >> putStrLn "http done"
      >> return f

feedWithMetadataFromURL url = do
  feed <- feedFromURL url
  let stories = feedToStories feed
  now <- getCurrentTime
  let storiesMetadata = S.fromList . mapf stories
                        $ \s -> SMD s now False False
  return $ Feed url (Text.pack $ getFeedTitle feed) (toStrictText $ getFeedHome feed) now storiesMetadata

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

anyA acc = accessor
  (F.any (getVal acc))
  (fmap . setVal acc )

-- Accessor for values wrapped in a maybe
strictMaybeA def = accessor (Strict.fromMaybe def) (\v _ -> Strict.Just v)

showDiffTime :: NominalDiffTime -> String
showDiffTime (floor -> (d :: Integer))
              | d < 60 = show d ++ "s"
              | d < 60^2 = show (d `div` 60) ++ "m"
              | d < 60^2 * 24 = show (d `div` 3600) ++ "h"
              | d < 60^2 * 24 * 7 = show (d `div` (3600 * 24)) ++ "d"
              | d < 60^2 * 24 * 365 = show (d `div` (3600 * 24 * 7)) ++ "w"
              | otherwise = show (d `div` (3600 * 24 * 7 * 365)) ++ "y"

timeDiff x y  = showDiffTime (diffUTCTime x y) ++ " ago"
-- timeDiff x y  = show y
