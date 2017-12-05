{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Tree
-- import           System.Console.Haskeline
--
import           Text.Search.New.Generic.SearchTree
import           Text.Search.New.SearchTree


import Test.Tasty
import Test.Tasty.HUnit



nentities = [(0,["Apple","Inc"]),(1,["Google","Inc"]),(1,["Google","Co"]),(2,["UpHere","Inc"])] :: [(Int,[Text])]
forest = foldr addTreeItem [] nentities



testcases :: [TestTree]
testcases = map (\(input,expected) ->
                   let result = (searchFunc forest . T.words) input
                   in testCase (T.unpack input) (fst (head result) @?= expected))
              [ ("Google Inc", Just 1)
              , ("Google"    , Nothing)
              , ("UpHere Inc", Just 2)
              , ("UpHere"    , Nothing) ]


main :: IO ()
main = do
  defaultMain (testGroup "Search unit tests" testcases)
