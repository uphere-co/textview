{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative          (many)
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Attoparsec.Text         (parseOnly)
import           Data.Text                    (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Tree
import           System.Console.Haskeline
--
import           SearchTree

searchFunc :: (Eq a, Ord a) => Forest (Maybe a) -> [a] -> [[a]]
searchFunc ts str = fmap (\x->str++ f x) $ searchForest str ts
  where f (Just x) = [x]
        f Nothing = [] -- "(END)"

makeIdiomForest txt =
  let (lst :: [[String]]) = map (read . T.unpack) $ drop 1 $ T.lines $ txt
      nentities = map head lst
      forest = foldr addTreeItem [] nentities
  in forest

makeF7745Forest txt =
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
      forest = foldr addTreeItem [] nentities
  in forest

--
makeTokenForest =
  let (tokens :: [[String]]) = [["Albert","Einstein"],["Richard","Feynman"],["Steven","Weinberg"]]
      forest = foldr addTreeItem [] tokens
  in forest
--

main :: IO ()
main = do
  putStrLn "search"
  txt <- TIO.readFile "/data/groups/uphere/data/Wiki/F7745.all_entities"
  txt' <- TIO.readFile "/data/groups/uphere/data/NLP/idiom.txt"
  let forest1 = makeF7745Forest txt
      forest2 = makeIdiomForest txt'
      forest3 = makeTokenForest
      forest  = forest3
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input -> liftIO $ do
    print $ searchFunc forest [input]

main' :: IO ()
main' = do
  putStrLn "search"
  txt <- TIO.readFile "F7745.all_entities"
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
      forest = foldr addTreeItem [] nentities
      testtxt = "I think Intel will be the most successful in history."
  print (parseOnly (many (pTreeAdv forest)) testtxt)
