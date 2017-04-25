module SearchTree where

import           Data.Function          (on)
import           Data.List              (sortBy)
import           Data.Tree

addTreeItem :: String -> Forest Char -> Forest Char
addTreeItem []     ts = ts
addTreeItem (x:xs) ts =
  let (prev,rest') = break (\t -> rootLabel t == x) ts
  in case rest' of
       []           -> sortBy (compare `on` rootLabel) (mkTree (x,xs) : ts )
       matched:rest ->
         let matched' = matched { subForest = addTreeItem xs (subForest matched)}
         in prev ++ (matched' : rest)
        
  
mkTree :: (Char,String) -> Tree Char
mkTree (x,(y:ys)) = Node x [mkTree (y,ys)]
mkTree (x,[])     = Node x []

searchForest :: String -> Forest Char -> [Char]
searchForest []     ts = map rootLabel ts
searchForest (x:xs) ts =
  let (prev,rest') = break (\t -> rootLabel t == x) ts
  in case rest' of
       []           -> [] 
       matched:rest -> searchForest xs (subForest matched)
