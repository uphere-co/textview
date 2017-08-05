{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Annotation.Type where

import           Data.Text           (Text)


data Zipper a = Z [a] a [a]

deriving instance (Show a) => Show (Zipper a)

data Zipper' a = Z' [a] [a] [a]

deriving instance (Show a) => Show (Zipper' a)


newtype AnnotText = AnnotText { unAnnotText :: [(Text,Bool)] } deriving (Show)


