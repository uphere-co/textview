module SearchTree where

import           Data.Function          (on)
import           Data.List              (sortBy)
import           Data.Tree

addTreeItem :: String -> Forest (Maybe Char) -> Forest (Maybe Char)
addTreeItem []     ts = ts
addTreeItem (x:xs) ts =
  let (prev,rest') = break (\t -> rootLabel t == Just x) ts
  in case rest' of
       []           -> sortBy (compare `on` rootLabel) (mkTree (x,xs) : ts )
       matched:rest ->
         let matched' = matched { subForest = addTreeItem xs (subForest matched)}
         in prev ++ (matched' : rest)
        
  
mkTree :: (Char,String) -> Tree (Maybe Char)
mkTree (x,(y:ys)) = Node (Just x) [mkTree (y,ys)]
mkTree (x,[])     = Node (Just x) [Node Nothing []]

searchForest :: String -> Forest (Maybe Char) -> [Maybe Char]
searchForest []     ts = map rootLabel ts
searchForest (x:xs) ts =
  let (prev,rest') = break (\t -> rootLabel t == Just x) ts
  in case rest' of
       []           -> [] 
       matched:rest -> searchForest xs (subForest matched)
