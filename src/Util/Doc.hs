{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Util.Doc where

import           Control.Lens
import           Control.Monad.Trans.State        (State,runState,get,put)
import qualified Data.ByteString.Char8     as B   (drop,take)
import           Data.Function                    (on)
import           Data.List                        (intersperse,groupBy,sortBy)
import           Data.Text                        (Text)
import qualified Data.Text                 as T 
import qualified Data.Text.Encoding        as TE  (decodeUtf8,encodeUtf8)
--

data Paragraph tag = Paragraph { para_content :: [(Text,tag)] } deriving Show

data ChunkedText tag = Normal (Text,tag)
                     | Split
                    
deriving instance (Show tag) => Show (ChunkedText tag)

instance Eq (ChunkedText tag) where
  Split == Split = True
  _     == _     = False

slice :: Int -> Int -> Text -> Text
slice b e =  T.drop b . T.take e 

sliceB :: Int -> Int -> Text -> Text
sliceB b e = TE.decodeUtf8 . B.drop b . B.take (e+1) . TE.encodeUtf8 

splitter :: (Int,Int) -> State (Int,Text) (Text,Text)
splitter (i,f) = do
  (n,t) <- get
  let (b,a) = T.splitAt (f-n) t
  put (f,a)
  return (T.splitAt (i-n) b)

                       
clutter :: (Text,tag) -> [ChunkedText tag]
clutter (txt,tag) = let splitted = Prelude.map (Normal . (,tag)) (T.split (== '\n') txt)
                    in intersperse Split splitted 


mkParagraph :: [(Text,tag)] -> [ChunkedText tag] -> [Paragraph tag]
mkParagraph partial []     = Paragraph partial : []
mkParagraph partial (x:xs) = case x of
                               Split            -> Paragraph partial : mkParagraph [] xs
                               Normal (txt,tag) -> mkParagraph (partial ++ [(txt,tag)]) xs


taggedTextToParagraph :: [(Text,tag)] -> [Paragraph tag]
taggedTextToParagraph para_pre =
  let chunked = para_pre >>= clutter
      f (Normal (txt,_tag)) = T.null txt
      f Split               = False
  in mkParagraph [] . filter (not.f) $ chunked


markOnly :: Text -> Paragraph (Maybe Text) -> Paragraph (Maybe Text)
markOnly paraid Paragraph {..} = Paragraph (map (\(txt,mpid) -> (txt,convert mpid)) para_content)
  where
    convert Nothing = Nothing
    convert (Just pid) = if pid == paraid then Just pid else Nothing

tagResult1 :: Text -> (tag,Int,Int) -> (Text,(tag,Text),Text)
tagResult1 txt (s,b,e) =
  let (t1,t1') = T.splitAt b txt
      (t2,t2') = T.splitAt (e-b) t1'
  in (t1,(s,t2),t2')

-- | generalized from tagResultFromScore
tagText :: [(tag,Int,Int)] -> Text -> [(Text,Maybe tag)]
tagText []  rtxt = [(rtxt,Nothing)]
tagText scs rtxt =
  let idxs = sortBy (compare `on` (view _2)) scs
      (tagged,(_,remaining)) =
        flip runState (0,rtxt) . flip mapM idxs $ \(s,b,e) -> do
          (n,t0) <- get
          let (t1,(_,t2),t2') = tagResult1 t0 (s,b-n-1,e-n)
          put (e,t2')
          return ((t1,Nothing),(t2,Just s))
      combine = Prelude.foldr (\(x,y) acc -> (x:).(y:).acc) id 
  in (combine tagged . ((remaining,Nothing):)) [] 
