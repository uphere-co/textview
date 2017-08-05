{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Annotation.View where

import           Data.List                     (intersperse)
import           Data.List.Split               (splitWhen)
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
--
import           Text.Annotation.Type


data Chunk a = Chunked  a
             | Splitted
             deriving (Show, Functor,Eq)

unChunk :: Chunk t -> t
unChunk (Chunked x) = x
unChunk Splitted    = error "unChunk: Splitted"

checkLength :: AnnotText tag -> Int
checkLength = sum . map (\x -> T.length (fst x)) . unAnnotText


checkLengthUnA :: [(Text,tag)] -> Int
checkLengthUnA = sum . map (\x -> T.length (fst x))


markPosition :: tag                      -- ^ default tag
             -> [(Text,tag)]
             -> [(Int, Int, (Text,tag))]
markPosition def xs = let xs' = scanl (\(a,_) y -> (a + T.length (fst y) ,y)) (0,("",def)) xs   
                      in zipWith (\x0 x1 -> (fst x0+1,fst x1,snd x1)) xs' (tail xs')


chunkAt :: Int
        -> [(Int, Int, (Text, tag))]
        -> ([(Int, Int, (Text, tag))], [(Int, Int, (Text, tag))])
chunkAt n lst = let (bef,aft) = break (\(_b,e,_) -> n <= e) lst
                in case aft of
                     [] -> (bef,[])
                     ((b,e,(t,m)):xs) -> let (t0,t1) = T.splitAt (n-b+1) t
                                         in (bef ++[(b,n,(t0,m))],(n+1,e,(t1,m)):xs)


chunkEveryAt :: Int -> [(Int, Int, (Text, tag))]
             -> [[(Int, Int, (Text, tag))]]
chunkEveryAt _n [] = []
chunkEveryAt n lst@(y:_) =  go (n0-1) [] lst
  where (n0,_,_) = y
        go m acc xs = let (bef,aft) = chunkAt (m+n) xs
                      in if null aft then acc++[bef] else go (m+n) (acc++[bef]) aft


chunkLines :: (Int, t, (Text, tag)) -> [(Int, Int, Chunk (Text, tag))]
chunkLines (b,_e,(t,m)) = let f (a,_) (Chunked x) = let l = T.length x in (a+l, Chunked x)
                              f (a,_) Splitted    = (a+1,Splitted)
                              ys = scanl f (0,Chunked "") ((intersperse Splitted . map Chunked . T.lines) t)
                              g ((a0,_t0),(a1,t1)) = (b+a0,b+a1-1,fmap (\z->(z,m)) t1)
                          in map g (zip ys (tail ys))


lineSplitAnnot :: tag -> Int -> AnnotText tag -> [[AnnotText tag]]
lineSplitAnnot def n (AnnotText tagged) = 
  let marked = markPosition def tagged
      chunked = concatMap chunkLines marked
      renormed = map (map (\(b,e,x) -> (b,e,unChunk x))) . splitWhen (\case (_,_,Splitted) -> True ; _ -> False) $ chunked
  in map (map (AnnotText . map (\(_,_,x)->x)) . chunkEveryAt n) renormed


cutePrintAnnotWithLabel :: (tag -> Maybe Text) -> AnnotText tag -> IO ()
cutePrintAnnotWithLabel lblf at = do
  let uat = unAnnotText at
      step (x,tag) acc = let n = T.length x
                         in case lblf tag of
                              Just lbl -> T.append (T.take n (lbl <> T.replicate n "-")) acc
                              Nothing  -> T.append (T.replicate n " ") acc
      al = foldr step "" uat
  TIO.putStrLn $ T.intercalate "" $ map (\x -> fst x) uat
  TIO.putStrLn al


cutePrintAnnot :: (tag -> Bool) -> AnnotText tag -> IO ()
cutePrintAnnot f = cutePrintAnnotWithLabel (\x -> if f x then Just "" else Nothing)


cutePrintOnlyAnnot :: (tag -> Bool) -> AnnotText tag -> IO ()
cutePrintOnlyAnnot f at = do
  let uat = unAnnotText at
  print $ T.intercalate " " $ map (\x -> fst x ) $ filter (\(_,y) -> f y) uat

