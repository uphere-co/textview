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
import           Control.Monad.Trans.Either (EitherT(..),left,right,hoistEither)
import           Control.Monad.State.Lazy
--
import           SearchTree
import           ParserCustom

main :: IO ()
main = do
  putStrLn "search"
  forest <- loadIdiom "/data/groups/uphere/data/NLP/idiom.txt"
  let s = runState (runEitherT (many $ pTreeAdv' forest)) ["as","long","as","possible","take","care","of","away","from","I"]
  print s
  return ()
