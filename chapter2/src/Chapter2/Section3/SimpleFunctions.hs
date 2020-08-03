module Chapter2.Section3.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst =
  if not (null lst)
    then head lst
    else "empty"

lst1 +++ lst2 =
  if null lst1 {- check emptiness -}
    then lst2 -- base case
    else head lst1 : (tail lst1 +++ lst2)

reverse2 list =
  if null list
    then []
    else reverse2 (tail list) +++ [head list]

maxmin list =
  if null (tail list)
    then (head list, head list)
    else ( if head list > fst (maxmin (tail list))
             then head list
             else fst (maxmin (tail list))
         , if head list < snd (maxmin (tail list))
             then head list
             else snd (maxmin (tail list)))
