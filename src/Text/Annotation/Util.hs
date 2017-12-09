{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Annotation.Util where

import           Control.Applicative          ((<|>))
import           Control.Lens
import           Data.Attoparsec.Combinator   (lookAhead)
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text  as A
import           Data.Char                    (isAlpha,isControl,isDigit,isMark,isNumber,isSymbol,isPunctuation)
import           Data.List                    (foldl')
import qualified Data.List.Split       as DLS
import           Data.Text                    (Text)
import qualified Data.Text             as T
--
import           Text.Annotation.Type


makeFixedString :: Int -> String -> String
makeFixedString n str = str ++ (replicate (n - (length str)) (' ' :: Char))

takeSubText :: Int -> Int -> Text -> Text
takeSubText i f txt = T.take (f - i) $ T.drop i txt

mkOffset :: [Text] -> [(Int,Int,Text)]
mkOffset xs = reverse $ foldl' (\acc x -> ((af acc),(af acc)+(T.length x),x):acc) [(0,0,"")] xs
  where af x' = head x' ^. _2

mkPostoOffset :: [(Int,Text)] -> [(Int,Int,Text)]
mkPostoOffset xs = reverse $ foldl' (\acc x -> ((af acc),(af acc)+(T.length (snd x)),(snd x)):acc) [(0,0,"")] xs
  where af x' = head x' ^. _2

mkSenFromOffset :: [(Int,Int,Text)] -> [[(Int,Int,Text)]]
mkSenFromOffset xs = DLS.split (DLS.whenElt (\(_,_,t) -> t == "." || t == "#ROW_BEGIN#" )) xs

mergeNR :: [(Int,Int,Text)] -> [(Int,Int,Text)]
mergeNR ptw = let nrl = DLS.chunksOf 2 ptw
  in map (\x -> (head x ^. _1, last x ^. _2, T.append (head x ^. _3) $ T.append (T.replicate ((last x ^. _1) - (head x ^. _2)) " ") (last x ^. _3) ) ) nrl

mergeNR' :: [(Int,Text)] -> [(Int,Text)]
mergeNR' ptw = let nrl = DLS.chunksOf 2 ptw
  in map (\x -> (fst $ head x, T.append (snd $ head x) $ T.append (T.replicate ((fst $ last x) - (fst $ head x)) " ") (snd $ last x) ) ) nrl

sepBy :: Text -> Text -> [Text]
sepBy orig txt = map (T.pack) $ DLS.split (DLS.condense $ DLS.oneOf (T.unpack orig)) (T.unpack txt)


-- Put space before and after the period.
-- Remain the period.
-- Caution : This breaks the original offset.
isolatePeriod :: Text -> Text
isolatePeriod = T.replace "." " . "

-- replace period with space
changePtoS :: Zipper Text -> Zipper Text
changePtoS (Z xs y zs) = Z xs (T.replace "." " " y) zs

changePtoS' :: Zipper' (Int,Int,Text) -> Zipper' (Int,Int,Text)
changePtoS' (Z' xs ys zs) = Z' xs (map (\(i,f,t) -> (i,f,T.replace "." " " t)) ys) zs



len1 :: [(Int,Int,Text)] -> Int
len1 xs   = foldl' (\acc (_,_,t)  -> acc + (T.length t)) 0 xs

len2 :: [[(Int,Int,Text)]] -> Int
len2 xss  = foldl' (\acc xs -> acc + len1 xs) 0 xss

len3 :: [[[(Int,Int,Text)]]] -> Int
len3 xsss = foldl' (\acc xss -> acc + len2 xss) 0 xsss


lenword1 :: [(Int,Int,Text)] -> Int
lenword1 xs  = foldl' (\acc _ -> acc + (1 :: Int)) 0 xs

lenword2 :: [[(Int,Int,Text)]] -> Int
lenword2 xss = foldl' (\acc xs -> acc + lenword1 xs) 0 xss


--

character = satisfy (\c -> isAlpha c || isDigit c || (c == '-') || (c == '/') || (c == ':'))

skipSpaceMatch1Char p = skipSpace >> fmap T.singleton (satisfy p)

punc = skipSpaceMatch1Char isPunctuation
  
symbol =
  (do skipSpace
      w <- (fmap T.pack $ many1' character)
      s <- string "\195"
      return (T.append w s))
  <|>
  skipSpaceMatch1Char isSymbol


onumber = skipSpaceMatch1Char isNumber
  
mark    = skipSpaceMatch1Char isMark

control = skipSpaceMatch1Char isControl
  
word =
  (do skipSpace
      w <- (fmap T.pack $ many1' character)
      c <- string ".com"
      return (T.append w c))
  <|>
  (do skipSpace
      w <- (fmap T.pack $ many1' character)
      c <- string ".COM"
      return (T.append w c))
  <|>
  (do skipSpace
      w <- (fmap T.pack $ many1' character)
      c <- string ".Com"
      return (T.append w c))
  <|>
  (do skipSpace
      w <- (fmap T.pack $ many1' character)
      c <- string ".net"
      return (T.append w c))
  <|>
  (do skipSpace
      w <- (fmap T.pack $ many1' character)
      c <- string ".edu"
      return (T.append w c))
  <|>
  (do skipSpace
      w <- (fmap T.pack $ many1' character)
      c <- string ".xyz"
      return (T.append w c))
  <|>
  (do skipSpace
      w <- (fmap T.pack $ many1' character)
      return w)
  

apst =
  (do skipSpace
      a <- string "'s"
      s <- many1' space
      return a)
  <|>
  (do skipSpace
      a <- string "'s"
      c <- lookAhead (string ",")
      return a
  )
  <|>
  (do skipSpace
      a <- string "'"
      return a)
  <|>
{-  (do skipSpace
      a <- string "&#39;s"
      s <- many1' space
      return a)
  <|> -}
  (do skipSpace
      a <- string "&#39;"
      return a)

bra =
  (do skipSpace
      char '('
      return "-LRB-")
  <|>
  (do skipSpace
      char ')'
      return "-RRB-")

token = do
  skipSpace
  t <- abbr <|> bra <|> apst <|> symbol <|> word <|> string "." <|> string "," <|> string "&" <|> onumber <|> punc <|> control
  return t


abbr = string "Inc." <|> string "Corp." <|> string "Ltd." <|> string "Co." <|> string "Mfg." <|> string "S.A.B." <|> string "S.A.B" <|> string "S.A." <|> string "C.V." <|> string "E.A." <|> string "E." <|> string "PG&E" <|> string "U.S." <|> "A." <|> string "N.V." <|> string "L.P." 

tokenizeText = do
  tokens <- manyTill token (skipSpace *> endOfInput)
  return tokens
--  T.split (\c -> (isSpace c) || (c == '\8217'))
