{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ParserCustom where

import           Control.Applicative
import           Data.Maybe             (isNothing, catMaybes) 
import qualified Data.Text              as T
import           Data.Tree
--
import           Control.Monad.Trans.Either (EitherT(..),left,right)
import           Control.Monad.State.Lazy   (State(..),get,put)
--
import           SearchTree

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
