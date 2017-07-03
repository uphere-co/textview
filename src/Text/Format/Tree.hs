{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Format.Tree where

import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text   as T 
import           Data.Tree


linePrint :: (a -> Text) -> Tree a -> Text
linePrint = linePrint' False (0,[])


linePrint' :: Bool -> (Int,[Int]) -> (a -> Text) -> Tree a -> Text
linePrint' b (l,ls) s n
  = case n of
      Node a [] -> drawlines b (l,ls) <> s a <> "\n"
      Node a xs -> drawlines b (l,ls) <> s a <> "\n" <>
                      T.concat (map (linePrint' False (l+1,l:ls) s) (init xs)) <>
                      linePrint' True (l+1,ls) s (last xs)

  where
    drawlines _     (0,_) = ""
    drawlines False (1,_) = "\x251C\x2500"
    drawlines True  (1,_) = "\x2514\x2500"
    drawlines False (l,ls) = T.concat (map (\x -> if x `elem` ls then "\x2502 " else "  ") [0..l-2]) <> "\x251C\x2500"
    drawlines True  (l,ls) = T.concat (map (\x -> if x `elem` ls then "\x2502 " else "  ") [0..l-2]) <> "\x2514\x2500"

