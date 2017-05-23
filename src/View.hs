{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module View where

import           Data.List                     (intersperse)
import           Data.List.Split               (splitWhen)
import           Data.Text                     (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
--
import           Type

checkLength :: AnnotText -> Int
checkLength at = sum $ map (\x -> T.length (fst x)) (unAnnotText at)

checkLengthUnA :: [(Text,Bool)] -> Int
checkLengthUnA uat = sum $ map (\x -> T.length (fst x)) uat



markPosition :: [(Text, Bool)] -> [(Int, Int, (Text, Bool))]
markPosition xs = let xs' = scanl (\(a,_) y -> (a + T.length (fst y) ,y)) (0,("",False)) xs   
                  in zipWith (\x0 x1 -> (fst x0+1,fst x1,snd x1)) xs' (tail xs')

chunkAt :: Int
        -> [(Int, Int, (Text, t))]
        -> ([(Int, Int, (Text, t))], [(Int, Int, (Text, t))])
chunkAt n lst = let (bef,aft) = break (\(b,e,_) -> n <= e) lst
                in case aft of
                     [] -> (bef,[])
                     ((b,e,(t,m)):xs) -> let (t0,t1) = T.splitAt (n-b+1) t
                                         in (bef ++[(b,n,(t0,m))],(n+1,e,(t1,m)):xs)

chunkEveryAt :: Int -> [(Int, Int, (Text, t))]
             -> [[(Int, Int, (Text, t))]]
chunkEveryAt n [] = [] -- go 0 [] lst
chunkEveryAt n lst@(y:ys) =  go (n0-1) [] lst
  where (n0,_,_) = y
        go m acc xs = let (bef,aft) = chunkAt (m+n) xs
                      in if null aft then acc++[bef] else go (m+n) (acc++[bef]) aft


data Chunk a = Chunked  a
             | Splitted
             deriving (Show, Functor,Eq)

unChunk :: Chunk t -> t
unChunk (Chunked x) = x

chunkLines :: (Int, t, (Text, t1)) -> [(Int, Int, Chunk (Text, t1))]
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

