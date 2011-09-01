{-# LANGUAGE PackageImports, NoMonomorphismRestriction,  RankNTypes, ScopedTypeVariables #-}
module UI where

import Data.Accessor
import Data.Accessor.Basic(fromWrapper)
import qualified Data.Foldable as F
import qualified Data.Ix as Ix
import Data.List (sort, sortBy, elemIndex)
import qualified Data.Map as Map
import Data.Ord(comparing)
import qualified Data.Sequence as S

import Control.Applicative((<$>))
import Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.Trans

import Data.IORef
import Data.Maybe
import Data.Tree
import Graphics.UI.Gtk as GTK

import Text.Feed.Query

import Feed
import WidgetBuilder

data DisplayFeed = FeedFeed FeedID | FeedStory FeedID Int deriving (Show, Eq, Ord)

type GTKStore = GTK.TreeStore DisplayFeed

treeFilterModelGetValue model filterModel path = do
  path' <- treeModelFilterConvertPathToChildPath filterModel path
  treeStoreGetValue model path'

indices s = if S.null s then [] else [0.. S.length s - 1 ]
indicesFrom a s = if S.null s then [] else [a .. a + S.length s - 1 ]

feedToTree f = Node (FeedFeed $ feed f) [Node (FeedStory (feed f) i) [] | i <- indices (stories f) ]

-- data UISTate = UIS
--                { view ::

--                    }

fromGTKStore  f s (FeedFeed x) = f x
fromGTKStore  f s (FeedStory x y) = s x y

safeIndex s i = if ((0,S.length s) `Ix.inRange` i) then (s `S.index` i) else
                  error ("range error:" ++ show i ++ " out of bounds (0," ++ (show$ S.length s) ++ ") ")

seqA i = accessor (`safeIndex` i) (S.update i)

showA = accessor show (\x _ -> Prelude.read x)

dispAcc feedAcc storyAcc disp =
  case disp of
      FeedFeed    f -> feedAcc <. unsafeMap f
      FeedStory f s -> storyAcc <. seqA s <. storiesF <. unsafeMap f

infixl 1 \/
(\/) = dispAcc

-- name accessor that handles feeds and stories equally
nameA = nameF \/ storyF .> titleF .> maybeA ""

-- is this story / every story in this feed read
readA = allA readF <. storiesF \/ readF

-- is this story / every story in this feed marked
markdA = allA markdF <. storiesF \/ markdF

dateA = showA <. lastF \/ showA <. fetchedF

for = flip map

-- prepare map as forest for use in a treeview
feedsToForest = map (feedToTree . snd) . Map.toList


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


toDate get row = do
  feeds <- get
  return $ feeds ^. dateA row

toLength ref row = length <$> toName ref row


treeStoreGetForest store pos = do
  (Node _ xs) <- treeStoreGetTree store pos
  return xs

-- boxPack style pad action = do
--   container <- ask
--   runReaderT action (container, \cont widget -> boxPackEnd cont widget style pad)


-- natural action = boxPack PackNatural 0

invert LT = GT
invert EQ = EQ
invert GT = LT

invertComparing f a b = invert $ comparing f a b

sortFunc model left' right' = do
  left <- treeModelGetPath model left'  >>= treeStoreGetValue model
  right <- treeModelGetPath model right' >>= treeStoreGetValue model
  return $ compare left right

addFeedToGTKStore feed store = do
  feedCount <- treeModelIterNChildren store Nothing
  treeStoreInsertTree store [] (feedCount +1) (feedToTree feed)

addStoriesToGTKStore feedID stories store = do
  forest <- treeStoreGetForest store []
  case (elemIndex feedID  (denode forest)) of
    Nothing -> putStrLn "Warning: Could not display Story, FeedID not present"
    Just ix -> treeStoreInsertForest store [ix] 0 $ for stories (\s -> Node (FeedStory feedID s) [])
    where denode = map (\(Node (FeedFeed x) _) -> x)

toggleRenderer (getA, setA) = do
  (view,model, wrapper) <- ask
  liftIO $ do
    rend <- cellRendererToggleNew
    on rend cellToggled $ \s -> do
      path <- treeModelFilterConvertPathToChildPath wrapper $ stringToTreePath s
      row <- treeStoreGetValue model path
      active <- getA row
      setA row (not active)
      treeModelFilterRefilter wrapper
      case row of
        FeedFeed _ -> widgetQueueDraw view
        _ -> return ()
    return (toCellRenderer rend, cLSA rend $ \row -> [cellToggleActive :=> getA row])

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






