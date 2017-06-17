{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ParserCustom where

import           Control.Applicative
import           Control.Monad.State.Lazy        (State,get,put)
import           Control.Monad.Trans.Either      (EitherT(..),left,right)
import           Data.Maybe                      (catMaybes) 
import           Data.Tree
--
import           Generic.SearchTree

type ParserG tok = EitherT String (State [tok])

instance {-# OVERLAPPING #-}Alternative (ParserG tok) where
  empty = EitherT (return (Left "error"))
  e1 <|> e2 = EitherT $ do
    s <- get
    r1 <- runEitherT e1
    case r1 of
      Left _   -> do
        put s
        runEitherT e2
      Right r' -> return $ Right r' 


pTreeG :: (Eq a, Ord a) => Forest (Maybe a) -> [a]-> ParserG a [a]
pTreeG forest acc = 
  let lst = searchForest acc forest
      lst' = catMaybes lst
  in (satisfyG (\c -> c `elem` lst') >>= \x -> pTreeG forest (acc++[x]))
     <|>
     if Nothing `elem` lst then return acc else left "not matched!"


pTreeAdvG :: (Eq a, Ord a, Show a) => Forest (Maybe a) -> ParserG a [a]
pTreeAdvG forest = skipTillG anyTokenG p
  where p = do
          x <- pTreeG forest []
          return x


anyTokenG :: (Show a) => ParserG a a
anyTokenG = satisfyG $ const True


skipTillG :: ParserG t a -> ParserG t b -> ParserG t b
skipTillG p end = scan'
  where scan' = end <|> (p *> scan')


satisfyG :: (t -> Bool) -> ParserG t t
satisfyG p = do
  l <- get
  case l of
    []     -> left "satisfyG: no more input"
    (x:xs) -> if p x then put xs >> right x else left "satisfyG: condition is not satisfied."
