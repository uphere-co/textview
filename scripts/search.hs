{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Tree
import           System.Console.Haskeline
--
import           Text.Search.New.Generic.SearchTree
import           Text.Search.New.SearchTree

main :: IO ()
main = do
  let nentities = [(0,["Apple","Inc"]),(1,["Google","Inc"]),(1,["Google","Co"]),(2,["UpHere","Inc"])] :: [(Int,[Text])]
      forest = foldr addTreeItem [] nentities

  print $ searchFunc forest ["Google","Inc"]
  print $ searchFunc forest ["Google"]
  print $ searchFunc forest ["UpHere","Inc"]
  print $ searchFunc forest ["UpHere"]
