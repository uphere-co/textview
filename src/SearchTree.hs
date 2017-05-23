{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SearchTree where

import           Control.Applicative
import           Control.Monad                        (mzero,void)
import           Control.Monad.Trans                  (lift)
import           Data.Attoparsec.Text                 
import qualified Data.Attoparsec.Internal.Types as AT (Parser(..),fromPos)
import           Data.Function          (on)
import           Data.List              (sortBy)
import           Data.Maybe             (isNothing, catMaybes) 
import           Data.Text                     (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Tree
--
import           Control.Monad.Trans.Either (EitherT(..),left,right)
import           Control.Monad.State.Lazy   (State(..),get,put)
--
import           Debug.Trace

addTreeItem :: (Eq a, Ord a) => [a] -> Forest (Maybe a) -> Forest (Maybe a)
addTreeItem []     ts = ts
addTreeItem (x:xs) ts =
  let (prev,rest') = break (\t -> rootLabel t == Just x) ts
  in case rest' of
       []           -> sortBy (compare `on` rootLabel) (mkTree (x,xs) : ts )
       matched:rest ->
         let matched' = matched { subForest = addTreeItem xs (subForest matched)}
         in prev ++ (matched' : rest)
          
mkTree :: (a,[a]) -> Tree (Maybe a)
mkTree (x,(y:ys)) = Node (Just x) [mkTree (y,ys)]
mkTree (x,[])     = Node (Just x) [Node Nothing []]

searchForest :: (Eq a, Ord a) => [a] -> Forest (Maybe a) -> [Maybe a]
searchForest []     ts = map rootLabel ts
searchForest (x:xs) ts =
  let (prev,rest') = break (\t -> rootLabel t == Just x) ts
  in case rest' of
       []           -> [] 
       matched:rest -> searchForest xs (subForest matched)


getPos :: Parser Int
getPos = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

skipTill :: Alternative f => f a -> f b -> f b
skipTill p end = scan'
  where scan' = end <|> (p *> scan')

tokencloser :: Parser ()
tokencloser = void (satisfy (`elem` (" .,!?:;()-+=\"'`/\\|\8217\8220\8221" :: String))) <|> endOfInput


pTree :: Forest (Maybe Char) -> String -> Parser (String,Int)
pTree forest acc = 
  let lst = searchForest acc forest
      lst' = catMaybes lst
      term = filter isNothing lst
  in (satisfy (\c -> c `elem` lst') >>= \x -> pTree forest (acc++[x]))
     <|>
     if Nothing `elem` lst then getPos >>= \e -> tokencloser >> return (acc,e) else mzero

pTreeAdv :: Forest (Maybe Char) -> Parser (Int,Int,String)
pTreeAdv forest = skipTill anyChar p
  where p = do
          b <- getPos
          (x,e) <- pTree forest []
          return (b+1,e,x)

searchFunc :: (Eq a, Ord a) => Forest (Maybe a) -> [a] -> [[a]]
searchFunc ts str = fmap (\x->str++ f x) $ searchForest str ts
  where f (Just x) = [x]
        f Nothing = [] -- "(END)"

makeIdiomForest txt =
  let (lst :: [[String]]) = map (read . T.unpack) $ drop 1 $ T.lines $ txt
      nentities = map head lst
      forest = foldr addTreeItem [] nentities
  in forest

makeF7745Forest txt =
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
      forest = foldr addTreeItem [] nentities
  in forest

makeTokenForest =
  let (tokens :: [[String]]) = [["Albert","Einstein"],["Richard","Feynman"],["Steven","Weinberg"]]
      forest = foldr addTreeItem [] tokens
  in forest

loadIdiom fp = do
  txt <- TIO.readFile "/data/groups/uphere/data/NLP/idiom.txt"
  let (lst :: [[String]]) = map (read . T.unpack) $ drop 1 $ T.lines $ txt
      idiomsent = map head lst
      nentities = map words idiomsent
      forest = foldr addTreeItem [] nentities
  return forest

type Parser' tok = EitherT String (State [tok])

instance {-# OVERLAPPING #-}Alternative (Parser' tok) where
  empty = EitherT (return (Left "error"))
  e1 <|> e2 = EitherT $ do
    s <- get
    r1 <- runEitherT e1
    case r1 of
      Left _   -> do
        put s
        runEitherT e2
      Right r' -> return $ Right r' 

pTree' :: (Eq a, Ord a) => Forest (Maybe a) -> [a]-> Parser' a [a]
pTree' forest acc = 
  let lst = searchForest acc forest
      lst' = catMaybes lst
      term = filter isNothing lst
  in (satisfy' (\c -> c `elem` lst') >>= \x -> pTree' forest (acc++[x]))
     <|>
     if Nothing `elem` lst then return acc else left "not matched!"


pTreeAdv' :: (Eq a, Ord a, Show a) => Forest (Maybe a) -> Parser' a [a]
pTreeAdv' forest = skipTill' anyChar' p
  where p = do
          x <- pTree' forest []
          return x

anyChar' :: (Show a) => Parser' a a
anyChar' = satisfy' $ const True

  {-do
  l <- lift get
  trace (show l) $ 
    case l of
      []     -> left "anyChar': no more input"
      (x:xs) -> right x
  -}
skipTill' :: Parser' t a -> Parser' t b -> Parser' t b
skipTill' p end = scan'
  where scan' = end <|> (p *> scan')

satisfy' p = do
  l <- get
  case l of
    []     -> left "satisfy': no more input"
    (x:xs) -> if p x then put xs >> right x else left "satisfy': condition is not satisfied."
