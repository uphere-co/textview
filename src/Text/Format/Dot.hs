{-# LANGUAGE OverloadedStrings #-}

module Text.Format.Dot where

import           Data.Char      (isSpace)
import           Data.Text      (Text)
import qualified Data.Text as T

mkLabelText :: Text -> Text
mkLabelText txt = T.replace ("\"") ("\\\"") $ T.dropWhile isSpace txt
