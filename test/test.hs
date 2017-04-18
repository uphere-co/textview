{-# LANGUAGE OverloadedStrings #-}

import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Text as T
--
import View
import Type

txt = "I just got reject by 500 Startups (company), Techsters and Y Combinator. And wrote e medium post about it. Here it goes. Lessons learned from a startup rejected by 500 Startups, Techstars and YCombinator The ups and downs, excitement and frustration, of being part of the selection process of the world’s top startup acceleration programs — and being rejected by all of them. By reading this article’s title you probably think my business is just bad — and it’s fine I won’t judge you — but please read the full story before reaching a conclusion."


ann0 = AnnotText [(T.take 30 txt,False)
                ,((T.drop 30 . T.take 35) txt, True)
                ,(T.drop 35 txt, False)
                ]

ann1 = AnnotText [(T.take 30 txt,False)
                ,((T.drop 30 . T.take 35) txt, True)
                ,((T.drop 35 . T.take 80) txt, False)
                ]

ann2 = AnnotText [((T.drop 80 . T.take 160) txt, False)]

markPosition xs = let xs' = scanl (\(a,_) y -> (a + T.length (fst y) ,y)) (0,("",False)) xs   
                  in zipWith (\x0 x1 -> (fst x0+1,fst x1,snd x1)) xs' (tail xs')

chunkAt n lst = let (bef,aft) = break (\(b,e,_) -> n >= b && n < e) lst
                in case aft of
                     [] -> (bef,[])
                     ((b,e,(t,m)):xs) -> let (t0,t1) = T.splitAt (n-b) t
                                         in (bef ++[(b,n,(t0,m))],(n+1,e,(t1,m)):xs)

chunkEveryAt n lst = go 0 [] lst
  where go m acc xs = let (bef,aft) = chunkAt (m+n) xs
                      in if null aft then acc else go (m+n) (acc++[bef]) aft
        
             

{-

((b,e,(t,m)):xs) (ys,y)
  | n < b           = chunkEveryAt n     xs (ys,y `snoc` (b,e,(t,m)))
  | n == b          = chunkEveryAt (n-b) xs (ys `snoc` y,[])
  | n > b && n < e  = chunkEveryAt  
-}
main = do -- mapM_ cutePrintAnnot (lineSplitAnnot 80 ann)
   --  cutePrintAnnot ann
    -- cutePrintAnnot ann2
    -- print (lineSplitAnnot 80 ann0)
   let marked = markPosition (unAnnotText ann0)
   print $ chunkAt 80 marked

   print $ chunkEveryAt 80 marked
