{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Tree
import           System.Console.Haskeline
--
import           SearchTree

searchFunc :: Forest Char -> String -> [String]
searchFunc ts str = fmap (\x->str++[x]) $ searchForest str ts 

main :: IO ()
main = do
  putStrLn "search"  
  txt <- TIO.readFile "F7745.all_entities"
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
  -- mapM_ putStrLn nentities

  let forest = foldr addTreeItem [] nentities
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input -> liftIO $ do
    print $ searchFunc forest input
