{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module View where

import           Control.Monad                 (forM_)
import           Data.List                     (foldl',nub)
import           Data.Text                     (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
--
import           Type
-- import           Usage
import           Util
--

checkLength :: AnnotText -> Int
checkLength at = sum $ map (\x -> T.length (fst x)) (unAnnotText at)

checkLengthUnA :: [(Text,Bool)] -> Int
checkLengthUnA uat = sum $ map (\x -> T.length (fst x)) uat

-- This should break when if-condition meets False case for the first time
takeUnder :: Int -> AnnotText -> AnnotText
takeUnder n at =
  let uat = unAnnotText at
  in AnnotText $ reverse $ fst $ foldl' (\(accl,accb) (t,b) -> if (checkLengthUnA ((t,b):accl) <= n && accb) then ((t,b):accl,accb) else (accl,False)) ([],True) $ take 80 uat

takeUnder' :: Int -> [(Text,Bool)] -> [(Text,Bool)]
takeUnder' n uat = reverse $ fst $ foldl' (\(accl,accb) (t,b) -> if (checkLengthUnA ((t,b):accl) <= n && accb) then ((t,b):accl,accb) else (accl,False)) ([],True) $ take 80 uat

remainAbove :: Int -> AnnotText -> Maybe AnnotText
remainAbove n at =
  let uat = unAnnotText at
  in if (length uat > 0)
     then Just $ AnnotText $ drop (length $ unAnnotText $ (takeUnder n at)) uat
     else Nothing


lineSplitAnnot :: Int -> AnnotText -> [AnnotText]
lineSplitAnnot n at = (takeUnder n at) : ( remained )
 where remained = case (remainAbove n at) of
                    Just  v -> lineSplitAnnot n v
                    Nothing -> []    

-- lineSplitAnnot80 = lineSplitAnnot 80
-- takeUnder80 = takeUnder 80
-- remainAbove80 = remainAbove 80 

cutePrintAnnot :: AnnotText -> IO ()
cutePrintAnnot at = do
  let uat = unAnnotText at
      al = foldr (\(x,y) acc -> if (y == True) then (T.append (T.replicate (T.length x) "-") acc) else (T.append (T.replicate (T.length x) " ") acc)) "" uat

  TIO.putStrLn $ T.intercalate "" $ map (\x -> fst x) uat
  TIO.putStrLn al

cutePrintOnlyAnnot :: AnnotText -> IO ()
cutePrintOnlyAnnot at = do
  let uat = unAnnotText at
  print $ T.intercalate " " $ map (\x -> fst x ) $ filter (\(_,y) -> y) uat

{- 
cutePrintNER :: [(Int,Int,Text)] -> IO ()
cutePrintNER mnr = do
  let ner = getNERList mnr
  forM_ ner $ \(uid,(txt,pat,i,f)) -> do
    putStrLn $ (makeFixedString 10 (T.unpack uid)) ++ (makeFixedString 30 (T.unpack txt)) ++ (makeFixedString 30 (T.unpack pat)) ++ "(" ++ (makeFixedString 10 (show i)) ++ "," ++ (makeFixedString 10 (show f)) ++ ")"
--    putStrLn $ (makeFixedString 30 (T.unpack t)) ++ (makeFixedString 30 (T.unpack $ replaceNumber t))++ "(" ++ (makeFixedString 10 (show i)) ++ "," ++ (makeFixedString 10 (show f)) ++ ")"

cutePrintPTW :: [(Int,Int,Text)] -> IO ()
cutePrintPTW mnr = do
  let li = nub $ map (\(_,_,t) -> replaceNumber t) mnr
      uidTag uid' i = T.pack $ uid' ++ (show i)
      uid = zipWith uidTag (repeat "UID") [(1 :: Int)..]
      result = zip uid li
  forM_ result $ \(u',txt) -> do
    putStrLn $ (makeFixedString 10 (T.unpack u')) ++ (makeFixedString 30 (T.unpack txt))

cutePrint :: [Text] -> IO ()
cutePrint txts = do
  forM_ txts $ \t -> do
    putStrLn (T.unpack t)
-}
