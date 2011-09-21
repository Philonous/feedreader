module FilterModel where

import qualified Graphics.UI.Gtk as GTK
-- import qualified Data.IntMap     as IM
import Control.Applicative ((<$>))
import qualified Control.Monad.Loops as Loop

import qualified Data.Sequence as Seq


newtype FilterCache = FC (Seq.Seq (Bool, FilterCache))

diag x = (x,x)



enumerateChildren model iter = do
  child <- GTK.treeModelIterChildren model iter
  case child of
    Nothing -> return Seq.empty
    Just c -> flip Loop.unfoldrM' c $ \c -> do
      next <- GTK.treeModelIterNext model c
      return $ diag <$> next


