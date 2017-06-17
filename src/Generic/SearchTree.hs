module Generic.SearchTree where

import           Data.Function          (on)
import           Data.List              (sortBy)
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


searchForest :: (Eq a, Ord a) => [a] -> Forest (Maybe a) -> [Maybe a]
searchForest []     ts = map rootLabel ts
searchForest (x:xs) ts =
  let (_prev,rest') = break (\t -> rootLabel t == Just x) ts
  in case rest' of
       []           -> [] 
       matched:_    -> searchForest xs (subForest matched)


searchFunc :: (Eq a, Ord a) => Forest (Maybe a) -> [a] -> [[a]]
searchFunc ts str = fmap (\x->str++ f x) $ searchForest str ts
  where f (Just x) = [x]
        f Nothing = []


