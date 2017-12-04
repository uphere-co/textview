{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Text.Annotation.Util

main :: IO ()
main = do
  print $ parseOnly tokenizeText "American Express Bank Ltd."
