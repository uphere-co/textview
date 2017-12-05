module Text.Search.Generic.SearchTree where

import           Data.Function          (on)
import           Data.List              (sortBy)
import           Data.Maybe             (maybeToList)
import           Data.Tree
--


addTreeItem :: (Eq a, Ord a) => (Int,[a]) -> Forest (Either Int a) -> Forest (Either Int a)
addTreeItem (_,[])     ts = ts
addTreeItem (i,(x:xs)) ts =
  let (prev,rest') = break (\t -> rootLabel t == Right x) ts
  in case rest' of
       []           -> sortBy (compare `on` rootLabel) (mkTree i (x,xs) : ts )
       matched:rest ->
         let matched' = matched { subForest = addTreeItem (i,xs) (subForest matched)}
         in prev ++ (matched' : rest)

            
mkTree :: Int -> (a,[a]) -> Tree (Either Int a)
mkTree i (x,(y:ys)) = Node (Right x) [mkTree i (y,ys)]
mkTree i (x,[])     = Node (Right x) [Node (Left i) []]


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


