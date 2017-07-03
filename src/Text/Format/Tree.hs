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

 --       <> T.replicate (n-2) "  " 
    -- drawlines xs = foldMap (\case True -> "\x2502 " ; False -> "  ") (init xs)
    --                <> (\case True -> "\x251C\x2500" ; False -> "  ") (last xs)
      

--                                  drawlines bs  <> btreePrint (bs++[False]) s b

      -- "\x251C\x2500" <>
--  <> drawlines bs <> "\x2514\x2500")

{- 
bntPrint :: [Bool] -> (e -> Text) -> (a -> Text) ->  BNTree e a -> Text
bntPrint _  _     lshow (BNTLeaf l)     = lshow l
bntPrint bs nshow lshow (BNTNode e a b) =
    "\x2299" <> nshow e <> "\n" <>
    drawlines bs <> "\x251C\x2500" <> bntPrint (bs++[True]) nshow lshow a <> "\n" <>
    drawlines bs <> "\x2514\x2500" <> bntPrint (bs++[False]) nshow lshow b
  where
    drawlines xs = foldMap (\case True -> "\x2502 " ; False -> "  ") xs

-- utility functions

convert :: BinTree Text -> BNTree Text Text
convert (BinLeaf a) = BNTLeaf a
convert (BinNode a b) = BNTNode "node" (convert a) (convert b)
-}
