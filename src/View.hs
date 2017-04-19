{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module View where

import           Control.Monad                 (forM_)
import           Data.List                     (foldl',intersperse,nub)
import           Data.List.Split               (splitWhen)
import           Data.Text                     (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
--
import           Type
-- import           Usage
import           Util
--
import           Debug.Trace

checkLength :: AnnotText -> Int
checkLength at = sum $ map (\x -> T.length (fst x)) (unAnnotText at)

checkLengthUnA :: [(Text,Bool)] -> Int
checkLengthUnA uat = sum $ map (\x -> T.length (fst x)) uat




markPosition xs = let xs' = scanl (\(a,_) y -> (a + T.length (fst y) ,y)) (0,("",False)) xs   
                  in zipWith (\x0 x1 -> (fst x0+1,fst x1,snd x1)) xs' (tail xs')

chunkAt n lst = let (bef,aft) = break (\(b,e,_) -> n >= b && n < e) lst
                in case aft of
                     [] -> (bef,[])
                     ((b,e,(t,m)):xs) -> let (t0,t1) = T.splitAt (n-b) t
                                         in (bef ++[(b,n,(t0,m))],(n+1,e,(t1,m)):xs)

chunkEveryAt n [] = [] -- go 0 [] lst
chunkEveryAt n lst@(y:ys) =  go (n0-1) [] lst
  where (n0,_,_) = y
        go m acc xs = let (bef,aft) = chunkAt (m+n) xs
                      in if null aft then acc++[bef] else go (m+n) (acc++[bef]) aft


data Chunk a = Chunked  a
             | Splitted
             deriving (Show, Functor,Eq)

unChunk (Chunked x) = x
                      
chunkLines (b,e,(t,m)) = let f (a,_) (Chunked x) = let l = T.length x in (a+l, Chunked x)
                             f (a,_) Splitted    = (a+1,Splitted)
                             ys = scanl f (0,Chunked "") ((intersperse Splitted . map Chunked . T.lines) t)
                             g ((a0,t0),(a1,t1)) = (b+a0,b+a1-1,fmap (\z->(z,m)) t1)
                         in map g (zip ys (tail ys))


-- lineSplitAnnot :: Int -> AnnotText -> [AnnotText]
-- lineSplitAnnot n = map (AnnotText . map (\(_,_,x)->x)) . chunkEveryAt n . markPosition . unAnnotText

lineSplitAnnot :: Int -> AnnotText -> [[AnnotText]]
lineSplitAnnot n (AnnotText tagged) = 
  let marked = markPosition tagged
      chunked = concatMap chunkLines marked
      renormed = map (map (\(b,e,x) -> (b,e,unChunk x))) . splitWhen (\(_,_,x) -> x == Splitted) $ chunked
  in map (map (AnnotText . map (\(_,_,x)->x)) . chunkEveryAt n) renormed
  -- mapM_ print renormed


        
{-



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
-}

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
