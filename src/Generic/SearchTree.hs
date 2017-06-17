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


searchForestBy :: (a -> a -> Bool) -> [a] -> Forest (Maybe a) -> [Maybe a]
searchForestBy _  []     ts = map rootLabel ts
searchForestBy eq (x:xs) ts =
  let Just a `meq` Just b = a `eq` b
      Nothing `meq` Nothing = True
      _ `meq` _ = False
      (_prev,rest') = break (\t -> rootLabel t `meq` Just x) ts
  in case rest' of
       []           -> [] 
       matched:_    -> searchForestBy eq xs (subForest matched)


searchForest :: (Eq a) => [a] -> Forest (Maybe a) -> [Maybe a]
searchForest = searchForestBy (==)
{- 
searchForest []     ts = map rootLabel ts
searchForest (x:xs) ts =
  let (_prev,rest') = break (\t -> rootLabel t == Just x) ts
  in case rest' of
       []           -> [] 
       matched:_    -> searchForest xs (subForest matched)
-}


searchFuncBy :: (a -> a -> Bool) -> Forest (Maybe a) -> [a] -> [[a]]
searchFuncBy eq ts str = fmap (\x->str++ f x) $ searchForestBy eq str ts
  where f (Just x) = [x]
        f Nothing = []


searchFunc :: (Eq a) => Forest (Maybe a) -> [a] -> [[a]]
searchFunc = searchFuncBy (==)
{- 
searchFunc ts str = fmap (\x->str++ f x) $ searchForest str ts
  where f (Just x) = [x]
        f Nothing = []
-}

