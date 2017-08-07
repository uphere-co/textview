{-# LANGUAGE OverloadedStrings #-}

import           Data.List       (mapAccumL)
import           Data.List.Split (splitWhen)
import           Data.Maybe      (isJust)
import           Data.Text       (Text)
import qualified Data.Text  as T
--
import           Text.Annotation.Type
import           Text.Annotation.View
import           Text.Annotation.Util.Doc
--
-- import Debug.Trace


txt = "I just got reject by 500 Startups (company), Techsters and Y Combinator. And wrote e medium post about it. Here it goes. Lessons learned from a startup rejected by 500 Startups, Techstars and YCombinator The ups and downs, excitement and frustration, of being part of the selection process of the world’s top startup acceleration programs — and being rejected by all of them.\n By reading this article’s title you probably think my business is just bad — and it’s fine I won’t judge you — but please read the full story before reaching a conclusion."

txt2 = "a012345678\nb012345678\nc012345678\nd012345678"

main = do
  let tagged = (map (\(t,m)->(t,isJust m)) . tagText [((),15,20),((),120,126)]) txt
      ann = AnnotText tagged
      xss = lineSplitAnnot 80 ann
  flip mapM_ xss $ \xs -> mapM_ cutePrintAnnot xs
