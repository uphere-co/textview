{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text
import qualified Data.Text.IO as T.IO
import           Data.Tree
--
import           Text.Format.Tree


mytree :: Tree Text
mytree = Node "a" [ Node "b" [ Node "c" []
                             , Node "d" [ Node "e" []
                                        , Node "f" []]]
                  , Node "g" []
                  , Node "h" [Node "i" [ Node "j" [ Node "k" []
                                                  , Node "l" []]
                                       , Node "m" [ Node "n" []]
                                       , Node "o" []]
                             ]
                  ]
                              

main = do
  T.IO.putStrLn (linePrint id mytree)

