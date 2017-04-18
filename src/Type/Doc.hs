{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Doc where

import           Data.Text                        (Text)

data Paragraph tag = Paragraph { para_content :: [(Text,tag)] } deriving Show


