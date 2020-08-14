{-# LANGUAGE LambdaCase #-}

module Chapter4.Section.Containers where

import qualified Data.Map as M

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k v =
  M.alter
    (\case
       Nothing -> Just v
       (Just _) -> Just v)
    k

delete :: Ord k => k -> M.Map k a -> M.Map k a
delete =
  M.alter
    (\case
       (Just _) -> Nothing)

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f =
  M.alter
    (\case
       (Just v) -> Just (f v))
