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

main :: IO ()
main = do
  putStrLn "search"
  txt <- TIO.readFile "/data/groups/uphere/data/Wiki/F7745.all_entities"
  txt' <- TIO.readFile "/data/groups/uphere/data/NLP/idiom.txt"
  let forest1 = makeF7745Forest txt
      -- forest2 = makeIdiomForest txt'
      -- forest3 = makeTokenForest
      -- forest  = forest3
  forest <- loadIdiom "/data/groups/uphere/data/NLP/idiom.txt"
  let s = runState (runEitherT (many $ pTreeAdv' forest)) ["as","long","as","possible","take","care","of","away","from","I"]
  print s
  return ()
{-
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input -> liftIO $ do
    print $ searchFunc forest [input]
-}
