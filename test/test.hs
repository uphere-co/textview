{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe      (isJust)
import           Data.Text       (Text)
import qualified Data.Text  as T
--
import           Type
import           View
import           Util.Doc


txt = "I just got reject by 500 Startups (company), Techsters and Y Combinator. And wrote e medium post about it. Here it goes. Lessons learned from a startup rejected by 500 Startups, Techstars and YCombinator The ups and downs, excitement and frustration, of being part of the selection process of the world’s top startup acceleration programs — and being rejected by all of them. By reading this article’s title you probably think my business is just bad — and it’s fine I won’t judge you — but please read the full story before reaching a conclusion."


ann0 = AnnotText [(T.take 30 txt,False)
                ,((T.drop 30 . T.take 35) txt, True)
                ,(T.drop 35 txt, False)
                ]
{-
ann1 = AnnotText [(T.take 30 txt,False)
                ,((T.drop 30 . T.take 35) txt, True)
                ,((T.drop 35 . T.take 80) txt, False)
                ]

ann2 = AnnotText [((T.drop 80 . T.take 160) txt, False)]
-}



main = do
  let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText [((),1,5),((),120,126)]) txt
  -- print ann
  mapM_ cutePrintAnnot (lineSplitAnnot 80 ann)
