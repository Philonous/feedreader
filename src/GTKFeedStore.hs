module GTKFeedStore where

import           Control.Applicative ((<$>))
import           Control.Monad

import           Data.List
import qualified Data.Map            as Map
import qualified Data.Sequence       as Seq
import qualified Data.Foldable       as F

import qualified Graphics.UI.Gtk     as GTK
import qualified System.Glib.GObject as GTK

import           Backend
import           Feed

import Data.Time


newtype GTKFeedStore a = GTKFeedStore {fromGTKFeedStore :: GTK.CustomStore () a}

instance GTK.GObjectClass (GTKFeedStore a) where
  toGObject (GTKFeedStore tm) = GTK.toGObject tm
  unsafeCastGObject = GTKFeedStore . GTK.unsafeCastGObject
instance GTK.TreeModelClass (GTKFeedStore a)
instance GTK.TypedTreeModelClass GTKFeedStore

createGTKFeedStore store =
  GTK.customStoreNew
    ()
    GTKFeedStore
    (storeDef store)
    Nothing
    Nothing

data DisplayFeed = FeedFeed FeedMetadata
                 | FeedStory FeedMetadata Int
                   deriving (Eq, Show)

checkIter store (GTK.TreeIter _ 0 0 5) = return True -- special root iter
checkIter store (GTK.TreeIter _ f 0 1) = do          -- feed
  feeds <- getFeeds store
  let f' = fromIntegral f
  return (Map.size feeds > f')
checkIter store (GTK.TreeIter _ f s 2) = do
  feeds <- getFeeds store
  let f' = fromIntegral f
  let s' = fromIntegral s
  return $ (Map.size feeds > f')
           && (Seq.length (stories . snd $ Map.toList feeds !! f') >  s')

maybeIter store x = do
  valid <- checkIter store x
  return $ if valid then Just x else Nothing

pathToIter store p = do
  case p of
      []        -> return . Just $ GTK.TreeIter 0 0 0 5
      (_:_:_:_) -> return Nothing
      (f:p)     -> maybeIter store $
        let (s,flag) = case p of
                         []  -> (0,1)
                         [p] -> (p,2)
        in GTK.TreeIter 0 (fromIntegral f) (fromIntegral s) flag

getRow store i@(GTK.TreeIter stamp f s flag) = do
--  putStrLn $ "get row: " ++ show i
  feeds <- getFeeds store
  let f' = Map.elems feeds !! (fromIntegral f)
  now <- getCurrentTime
  case flag of
    1 -> return $ FeedFeed f'
    2 -> return $ FeedStory f' (fromIntegral s)
    _ -> return $ FeedFeed $ Feed "" (show i) Nothing now (Seq.empty)
    -- _ -> -- putStrLn ("error, invalid flag:" ++ show i) >>
    --       getRow store (GTK.TreeIter stamp 0 0 1)

getRowFromPath store path = do
  iter <- pathToIter store path
  case iter of
    Nothing -> return Nothing
    Just i  -> Just <$> getRow store i


iterNext store iter@(GTK.TreeIter v f s flag) = do
  putStrLn $ "next: " ++ show iter
  maybeIter store $
    case flag of
      2 -> GTK.TreeIter v f (s+1) flag
      1 -> GTK.TreeIter v (f+1) 0 flag
      _ -> GTK.TreeIter v f s (90) -- invalid iter

iterChildren store (Just (GTK.TreeIter v f 0 1)) = maybeIter store $ GTK.TreeIter v f 0 2
iterChildren store Nothing = do
  return $ Just (GTK.TreeIter 0 0 0 1)
iterChildren _ _ = return Nothing

pprint stories = forM (zip [1..] $ F.toList stories) $ \(i,s) ->
                   putStrLn $ (show i) ++ " " ++ show (title . story $ s)

iterNChildren store Nothing = Map.size <$> getFeeds store
iterNChildren store (Just iter) = do
  row <- getRow store iter
  case row of
    FeedFeed f -> putStrLn ("nchildren " ++ show iter ++ " - " ++ show ( Seq.length $ stories f ))
                    >> pprint (stories f)
                    >> (return . Seq.length $ stories f)
    _ -> return 0

iterHasChild store iter = (> 0) <$> iterNChildren store (Just iter)

iterNthChild store i n = putStrLn ("nth: " ++ show i) >> iterNthChild' store i n
iterNthChild' store Nothing n = do
  maybeIter store $ GTK.TreeIter 0 (fromIntegral n) 0 1
iterNthChild' store (Just (GTK.TreeIter v f 0 1)) n = maybeIter store $
                                             GTK.TreeIter v f (fromIntegral n) 2
iterNthChild' _ _ _ = return Nothing

iterParent (GTK.TreeIter v f 0 1) = return . Just $ (GTK.TreeIter v 0 0 10)
iterParent (GTK.TreeIter v f s 2) = return . Just $ (GTK.TreeIter v f 0 1)
iterParent _ = return Nothing

storeDef store = GTK.TreeModelIface
  { GTK.treeModelIfaceGetFlags = return []
  , GTK.treeModelIfaceGetIter = pathToIter store
  , GTK.treeModelIfaceGetPath = \(GTK.TreeIter _ f s flag) -> return $
      case flag of
        0 -> []
        1 -> [fromIntegral f]
        2 -> [fromIntegral f, fromIntegral s]
  , GTK.treeModelIfaceGetRow = getRow store
  , GTK.treeModelIfaceIterNext = iterNext store
  , GTK.treeModelIfaceIterChildren = iterChildren store
  , GTK.treeModelIfaceIterHasChild = iterHasChild store
  , GTK.treeModelIfaceIterNChildren = iterNChildren store
  , GTK.treeModelIfaceIterNthChild = iterNthChild store
  , GTK.treeModelIfaceIterParent = iterParent
  , GTK.treeModelIfaceRefNode = const $ return ()
  , GTK.treeModelIfaceUnrefNode = const $ return ()
  }


data GTKStore a = GTKStore
  { store :: a
  , customStore :: (GTKFeedStore DisplayFeed)
  }

instance FeedStoreClass a => FeedStoreClass (GTKStore a) where
  getFeeds      (GTKStore a _) = getFeeds  a
  addFeed       = gtkAddFeed
  appendStories = gtkAppendStories
  setRead       (GTKStore a _) = setRead   a
  setMarked     (GTKStore a _) = setMarked a
  milestone     (GTKStore a _) = milestone a


feedToPath a f = do
  feeds <- getFeeds a
  case elemIndex f (Map.keys feeds) of
    Nothing -> error $ "Feed not in store: " ++ show f
    Just i -> return [i]

modelEmpty store = do
  x <- GTK.treeModelGetIterFirst store
  case x of
    Nothing -> return True
    Just _  -> return False


gtkAppendStory (GTKStore model customStore) feed story = do
  feeds <- appendStories model feed (Seq.singleton story)
  feedPath <- feedToPath model feed
  let storyPath = feedPath ++ [0]
  Just feedIter <- GTK.treeModelGetIter customStore feedPath
  Just storyIter <- GTK.treeModelIterChildren customStore feedIter
  child <- GTK.treeModelIterHasChild customStore feedIter
  GTK.treeModelRowInserted customStore (storyPath) storyIter
  unless child $ GTK.treeModelRowHasChildToggled customStore feedPath feedIter
  return feeds

gtkAppendStories store feed stories = do
  forM_ (reverse $ F.toList stories) $ gtkAppendStory store feed
  getFeeds store
  -- We have to add the stories one-by-one or the filter model will barf

gtkAddFeed
  :: FeedStoreClass a => GTKStore a -> FeedMetadata -> IO FeedStorage
gtkAddFeed m@(GTKStore store model) f = do
  let emptyFeed = f {stories = Seq.empty }
  feeds <- addFeed store emptyFeed
  path <- feedToPath store (feed emptyFeed)
  Just iter <- GTK.treeModelGetIter model path
  GTK.treeModelRowInserted model path iter
  gtkAppendStories m (feed f) (stories f)

