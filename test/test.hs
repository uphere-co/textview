{-# LANGUAGE OverloadedStrings #-}

import           Data.List       (mapAccumL)
import           Data.List.Split (splitWhen)
import           Data.Maybe      (isJust)
import           Data.Text       (Text)
import qualified Data.Text  as T
--
import           Type
import           View
import           Util.Doc
--
import Debug.Trace


txt = "I just got reject by 500 Startups (company), Techsters and Y Combinator. And wrote e medium post about it. Here it goes. Lessons learned from a startup rejected by 500 Startups, Techstars and YCombinator The ups and downs, excitement and frustration, of being part of the selection process of the world’s top startup acceleration programs — and being rejected by all of them.\n By reading this article’s title you probably think my business is just bad — and it’s fine I won’t judge you — but please read the full story before reaching a conclusion."

txt2 = "a012345678\nb012345678\nc012345678\nd012345678"
-- chunkLines :: (Int,Int,(Text,Bool)] -> [(Int,Int,(Text,Bool))]

main = do
  -- print txt2
  -- let tagged = [(txt2,False)]
  let tagged = (map (\(t,m)->(t,isJust m)) . tagText [((),15,20),((),120,126)]) txt
  let ann = AnnotText tagged
      marked = markPosition tagged
      chunked = concatMap chunkLines marked
  -- print marked
  putStrLn "================="
  mapM_ print $ map (map (\(b,e,x) -> (b,e,unChunk x))) $ (splitWhen (\(_,_,x) -> x == Splitted) chunked) 
  -- print (map clutter tagged)
  -- (print . map (chunkEveryAt 5)) $ chunked
  -- mapM_ (print . chunkEveryAt 5 . chunkLines) marked 
  -- (mapM_ print . concatMap chunkLines . markPosition) tagged
  -- print ann
  -- mapM_ cutePrintAnnot (lineSplitAnnot 80 ann)
