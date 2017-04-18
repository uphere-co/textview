{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import           Control.Lens
import           Control.Monad                (forM_)
import qualified Data.Char             as DC
import           Data.List                    (foldl')
import qualified Data.List.Split       as DLS
import           Data.Text                    (Text)
import qualified Data.Text             as T

--
-- import Parser
import Type

makeFixedString :: Int -> String -> String
makeFixedString n str = str ++ (replicate (n - (length str)) (' ' :: Char))

takeSubText :: Int -> Int -> Text -> Text
takeSubText i f txt = T.take (f - i) $ T.drop i txt

mkOffset :: [Text] -> [(Int,Int,Text)]
mkOffset xs = reverse $ foldl' (\acc x -> ((af acc),(af acc)+(T.length x),x):acc) [(0,0,"")] xs
  where af x' = head x' ^. _2

mkPostoOffset :: [(Int,Text)] -> [(Int,Int,Text)]
mkPostoOffset xs = reverse $ foldl' (\acc x -> ((af acc),(af acc)+(T.length (snd x)),(snd x)):acc) [(0,0,"")] xs
  where af x' = head x' ^. _2

mkSenFromOffset :: [(Int,Int,Text)] -> [[(Int,Int,Text)]]
mkSenFromOffset xs = DLS.split (DLS.whenElt (\(_,_,t) -> t == "." || t == "#ROW_BEGIN#" )) xs

mergeNR :: [(Int,Int,Text)] -> [(Int,Int,Text)]
mergeNR ptw = let nrl = DLS.chunksOf 2 ptw
  in map (\x -> (head x ^. _1, last x ^. _2, T.append (head x ^. _3) $ T.append (T.replicate ((last x ^. _1) - (head x ^. _2)) " ") (last x ^. _3) ) ) nrl

mergeNR' :: [(Int,Text)] -> [(Int,Text)]
mergeNR' ptw = let nrl = DLS.chunksOf 2 ptw
  in map (\x -> (fst $ head x, T.append (snd $ head x) $ T.append (T.replicate ((fst $ last x) - (fst $ head x)) " ") (snd $ last x) ) ) nrl

sepBy :: Text -> Text -> [Text]
sepBy orig txt = map (T.pack) $ DLS.split (DLS.condense $ DLS.oneOf (T.unpack orig)) (T.unpack txt)


-- Put space before and after the period.
-- Remain the period.
-- Caution : This breaks the original offset.
isolatePeriod :: Text -> Text
isolatePeriod = T.replace "." " . "

-- replace period with space
changePtoS :: Zipper Text -> Zipper Text
changePtoS (Z xs y zs) = Z xs (T.replace "." " " y) zs

changePtoS' :: Zipper' (Int,Int,Text) -> Zipper' (Int,Int,Text)
changePtoS' (Z' xs ys zs) = Z' xs (map (\(i,f,t) -> (i,f,T.replace "." " " t)) ys) zs



len1 :: [(Int,Int,Text)] -> Int
len1 xs   = foldl' (\acc (_,_,t)  -> acc + (T.length t)) 0 xs

len2 :: [[(Int,Int,Text)]] -> Int
len2 xss  = foldl' (\acc xs -> acc + len1 xs) 0 xss

len3 :: [[[(Int,Int,Text)]]] -> Int
len3 xsss = foldl' (\acc xss -> acc + len2 xss) 0 xsss


lenword1 :: [(Int,Int,Text)] -> Int
lenword1 xs  = foldl' (\acc _ -> acc + (1 :: Int)) 0 xs

lenword2 :: [[(Int,Int,Text)]] -> Int
lenword2 xss = foldl' (\acc xs -> acc + lenword1 xs) 0 xss
