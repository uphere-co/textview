module Text.Search.New.Generic.SearchTree where

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


searchForestBy :: (a -> b -> Bool) -> [a] -> Forest (Either Int b) -> [Either Int b]
searchForestBy _  []     ts = map rootLabel ts
searchForestBy eq (x:xs) ts =
  let Right a `meq` Right b = a `eq` b
      Left _ `meq` Left _ = True
      _ `meq` _ = False
      (_prev,rest') = break (\t -> Right x `meq` rootLabel t) ts
  in case rest' of
       []           -> [] 
       matched:_    -> searchForestBy eq xs (subForest matched)


searchForest :: (Eq a) => [a] -> Forest (Either Int a) -> [Either Int a]
searchForest = searchForestBy (==)



searchFunc :: (Eq a) => Forest (Either Int a) -> [a] -> [(Maybe Int,[a])]
searchFunc ts xs = let f x xs = case x of
                         Left  i -> (Just i, xs)
                         Right t -> (Nothing, xs ++ [t]) 
                   in fmap (\x -> f x xs) $ searchForest xs ts
