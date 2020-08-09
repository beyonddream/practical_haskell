module Chapter3.Section4.Lists where

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) =
  if f x
    then x : filter' f xs
    else filter' f xs
