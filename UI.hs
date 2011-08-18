{-# LANGUAGE PackageImports, NoMonomorphismRestriction #-}
module UI where

import Data.Accessor
import Data.Accessor.Basic(fromWrapper)
import Data.List (sort, sortBy)
import Data.Ord(comparing)

import Control.Applicative((<$>))
import qualified Data.Map as Map
import Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.Trans

import Data.IORef
import Data.Maybe
import Data.Tree
import Graphics.UI.Gtk

import Text.Feed.Query

import Feed

data Diplay = FeedFeed FeedID | FeedStory FeedID StoryID deriving (Show, Eq, Ord)

feedToTree f = Node (FeedFeed $ feed f) (for  (reverse . sortBy (comparing snd) . Map.toList $ stories f) $ \(s,_) ->
                                        Node (FeedStory (feed f) s) []
                                        )
-- data UISTate = UIS
--                { view ::

--                    }

fromDisplay  f s (FeedFeed x) = f x
fromDisplay  f s (FeedStory x y) = s x y

dispAcc feedAcc storyAcc disp =
  case disp of
      FeedFeed    f -> feedAcc <. unsafeMap f
      FeedStory f s -> storyAcc <. unsafeMap s <. storiesF <. unsafeMap f

infixl 1 \/
(\/) = dispAcc

-- name accessor that handles feeds and stories equally
nameA = nameF \/ storyF .> titleF .> maybeA ""

-- is this story / every story in this feed read
readA = allA readF <. storiesF \/ readF

-- is this story / every story in this feed marked
markdA = allA markdF <. storiesF \/ markdF

for = flip map

-- prepare map as forest for use in a treeview
feedsToForest = map (feedToTree . snd) . Map.toList

-- add a column to a tree view
addColumn name renderers= do
  (view, model) <- ask
  col <- liftIO $ treeViewColumnNew
  liftIO $ set col [treeViewColumnTitle := name]
  forM_ renderers $ \i -> do
    (rend, attr) <- i
    liftIO $ do
      treeViewColumnPackStart col rend True
      --cellLayoutSetAttributes col rend model attr
      attr col model
  liftIO $ treeViewAppendColumn view col
  return ()

withNewTreeView model action = do
  current <- ask
  treeView <- liftIO $ treeViewNewWithModel model
  lift $ runReaderT action (treeView, model)
  liftIO $ containerAdd current treeView
  return treeView

cLSA rend attr col model = cellLayoutSetAttributes col rend model attr

textRenderer selector = liftIO $ do
  rend <- cellRendererTextNew
  return (toCellRenderer rend, cLSA rend $ \row -> [cellText :=> selector row])

toggleRenderer (getA, setA) = do
  (view,model) <- ask
  liftIO $ do
    rend <- cellRendererToggleNew
    on rend cellToggled $ \s -> do
      row <- treeStoreGetValue model $ stringToTreePath s
      active <- getA row
      setA row (not active)
      case row of
        FeedFeed _ -> widgetQueueDraw view
        _ -> return ()
    return (toCellRenderer rend, cLSA rend $ \row -> [cellToggleActive :=> getA row])

fromGetSet acc get set =
  ( \row -> getVal (acc row) <$> get
  , \row x ->  ( setVal (acc row) x <$> get >>= set)
  )


toRead = fromGetSet readA

toMarkd = fromGetSet markdA

testFeeds = ["http://www.lawblog.de/index.php/feed/"
            , "http://www.nachdenkseiten.de/?feed=rss2"
            ]

toName get row = do
  feeds <- get
  return $ feeds ^. nameA row

toLength ref row = length <$> toName ref row

withContainer new action = do
  current <- ask
  c <- liftIO $ new
  lift $ runReaderT action c
  liftIO $ containerAdd current c

withVBoxNew = withContainer (vBoxNew False 0)
withHBoxNew = withContainer (vBoxNew False 0)

withMainWindow action = do
  w <- windowNew
  a <- runReaderT action w
  return w

addNewWidget new = do
  current <- ask
  w <- liftIO $ new
  liftIO $ containerAdd current w
  return w

addButton name action = do
  b <- liftIO $ buttonNew
  liftIO $ buttonSetLabel b name
  liftIO $ on b buttonActivated action
  addNewWidget $ return b

treeStoreGetForest store pos = do
  (Node _ xs) <- treeStoreGetTree store pos
  return xs

invert LT = GT
invert EQ = EQ
invert GT = LT

invertComparing f a b = invert $ comparing f a b

sortFunc model left' right' = do
  left <- treeModelGetPath model left'  >>= treeStoreGetValue model
  right <- treeModelGetPath model right' >>= treeStoreGetValue model
  return $ compare left right

joinTree newTree treeStore pos = do
  oldTree <- zip [0..] <$> treeStoreGetForest treeStore pos
  let end = length oldTree
  let sortedOldTree = sortBy (comparing $ rootLabel . snd) oldTree
  let sortedNewTree = sortBy (comparing rootLabel) newTree
  mergeTrees pos end sortedOldTree sortedNewTree
    where
      mergeTrees _ _ [] [] = return ()
      mergeTrees pos end [] ys = treeStoreInsertForest treeStore pos end ys
      mergeTrees pos end ((cur,(Node x xt)):xs) ya@(yc@(Node y yt):ys) = case compare x y of
        LT -> mergeTrees pos end xs ya
        EQ -> joinTree yt treeStore (pos ++[cur]) >> mergeTrees pos end xs ys
        GT -> treeStoreInsertTree treeStore pos cur yc >> joinTree ys treeStore pos





