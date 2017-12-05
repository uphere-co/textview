{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Search.ParserCustom where

import           Control.Applicative
import           Control.Monad.State.Lazy        (State,get,put)
import           Control.Monad.Trans.Either      (EitherT(..),left,right)
import           Data.Either                     (isLeft,lefts,rights)
import           Data.Maybe                      (catMaybes,isNothing) 
import           Data.Tree
--
import           Text.Search.Generic.SearchTree


type ParserG tok = EitherT String (State [tok])


instance {-# OVERLAPPING #-} Alternative (ParserG tok) where
  empty = EitherT (return (Left "error"))
  e1 <|> e2 = EitherT $ do
    s <- get
    r1 <- runEitherT e1
    case r1 of
      Left _   -> do
        put s
        runEitherT e2
      Right r' -> return $ Right r' 


pTreeG :: (Eq a) => Forest (Either Int a) -> [a]-> ParserG a (Int,[a])
pTreeG = pTreeGBy (==)


pTreeGBy :: (a -> b -> Bool) -> Forest (Either Int b) -> [a] -> ParserG a (Int,[a])
pTreeGBy eq forest acc = 
  let lst = searchForestBy eq acc forest
      lst' = rights lst
  in (satisfyG (\c -> any (eq c) lst') >>= \x -> pTreeGBy eq forest (acc++[x]))
     <|>
     if (not.null.filter isLeft) lst then return (let [i] = lefts lst in (i,acc)) else left "not matched!"


pTreeAdvG :: (Eq a) => Forest (Either Int a) -> ParserG a (Int,[a])
pTreeAdvG = pTreeAdvGBy (==)


pTreeAdvGBy :: (a -> b -> Bool) -> Forest (Either Int b) -> ParserG a (Int,[a])
pTreeAdvGBy eq forest = skipTillG anyTokenG (pTreeGBy eq forest [])


anyTokenG :: ParserG a a
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
