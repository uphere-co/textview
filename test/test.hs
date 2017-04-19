{-# LANGUAGE OverloadedStrings #-}

import           Data.List       (mapAccumL)
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

txt2 = "a\nb\nc\nd"
-- chunkLines :: (Int,Int,(Text,Bool)] -> [(Int,Int,(Text,Bool))]
chunkLines (b,e,(t,m)) = let f (a,_) x = let l = T.length x in (a+l+1, x)
                             ys = scanl f (0,"") (T.lines t)
                             g ((a0,t0),(a1,t1)) = (b+a0,b+a1-2,t1)
                         in map g (zip ys (tail ys))

main = do
  let -- tagged = [(txt2,False)]
  let tagged = (map (\(t,m)->(t,isJust m)) . tagText [((),15,20),((),120,126)]) txt
  let ann = AnnotText tagged
  mapM_ print (markPosition tagged)
  putStrLn "================="
  (mapM_ print . concatMap chunkLines . markPosition) tagged
  -- print ann
  -- mapM_ cutePrintAnnot (lineSplitAnnot 80 ann)
