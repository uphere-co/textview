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
import           Generic.SearchTree
import           SearchTree


main :: IO ()
main = do
  putStrLn "search"
  txt <- TIO.readFile "/data/groups/uphere/data/Wiki/F7745.all_entities"
  let forest = makeF7745Forest txt
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input -> liftIO $ do
    print $ searchFunc forest input

main' :: IO ()
main' = do
  putStrLn "search"
  txt <- TIO.readFile "F7745.all_entities"
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
      forest = foldr addTreeItem [] nentities
      testtxt = "I think Intel will be the most successful in history."
  print (parseOnly (many (pTreeAdv forest)) testtxt)
