module Text.Search.Generic.SearchTree where

import           Data.Function          (on)
import           Data.List              (sortBy)
import           Data.Maybe             (maybeToList)
import           Data.Tree
--


addTreeItem :: (Eq a, Ord a) => [a] -> Forest (Maybe a) -> Forest (Maybe a)
addTreeItem []     ts = ts
addTreeItem (x:xs) ts =
  let (prev,rest') = break (\t -> rootLabel t == Just x) ts
  in case rest' of
       []           -> sortBy (compare `on` rootLabel) (mkTree (x,xs) : ts )
       matched:rest ->
         let matched' = matched { subForest = addTreeItem xs (subForest matched)}
         in prev ++ (matched' : rest)

            
mkTree :: (a,[a]) -> Tree (Maybe a)
mkTree (x,(y:ys)) = Node (Just x) [mkTree (y,ys)]
mkTree (x,[])     = Node (Just x) [Node Nothing []]


searchForestBy :: (a -> b -> Bool) -> [a] -> Forest (Maybe b) -> [Maybe b]
searchForestBy _  []     ts = map rootLabel ts
searchForestBy eq (x:xs) ts =
  let Just a `meq` Just b = a `eq` b
      Nothing `meq` Nothing = True
      _ `meq` _ = False
      (_prev,rest') = break (\t -> Just x `meq` rootLabel t) ts
  in case rest' of
       []           -> [] 
       matched:_    -> searchForestBy eq xs (subForest matched)


searchForest :: (Eq a) => [a] -> Forest (Maybe a) -> [Maybe a]
searchForest = searchForestBy (==)



searchFunc :: (Eq a) => Forest (Maybe a) -> [a] -> [[a]]
searchFunc ts xs = fmap (\x -> xs ++ maybeToList x) $ searchForest xs ts


