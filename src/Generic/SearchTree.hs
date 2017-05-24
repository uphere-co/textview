{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Generic.SearchTree where

import           Control.Applicative
import           Control.Monad                        (mzero,void)
import           Data.Attoparsec.Text                 
import qualified Data.Attoparsec.Internal.Types as AT (fromPos)
import           Data.Function          (on)
import           Data.List              (sortBy)
import           Data.Maybe             (isNothing, catMaybes) 
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
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
  let (prev,rest') = break (\t -> rootLabel t == Just x) ts
  in case rest' of
       []           -> [] 
       matched:rest -> searchForest xs (subForest matched)

searchFunc :: (Eq a, Ord a) => Forest (Maybe a) -> [a] -> [[a]]
searchFunc ts str = fmap (\x->str++ f x) $ searchForest str ts
  where f (Just x) = [x]
        f Nothing = [] -- "(END)"

makeIdiomForest :: Text -> Forest (Maybe Char)
makeIdiomForest txt =
  let (lst :: [[String]]) = map (read . T.unpack) $ drop 1 $ T.lines $ txt
      nentities = map head lst
      forest = foldr addTreeItem [] nentities
  in forest

makeF7745Forest :: Text -> Forest (Maybe Char)
makeF7745Forest txt =
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
      forest = foldr addTreeItem [] nentities
  in forest

loadIdiom :: FilePath -> IO (Forest (Maybe String))
loadIdiom fp = do
  txt <- TIO.readFile "/data/groups/uphere/data/NLP/idiom.txt"
  let (lst :: [[String]]) = map (read . T.unpack) $ drop 1 $ T.lines $ txt
      idiomsent = map head lst
      nentities = map words idiomsent
      forest = foldr addTreeItem [] nentities
  return forest

