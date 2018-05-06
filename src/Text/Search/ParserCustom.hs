{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Search.ParserCustom where

import           Control.Applicative
import           Control.Monad.State.Lazy        (State,get,put)
import           Control.Monad.Trans.Either      (EitherT,newEitherT,runEitherT,left,right)
import           Data.Maybe                      (catMaybes,isNothing) 
import           Data.Tree
--
import           Text.Search.Generic.SearchTree


type ParserG tok = EitherT String (State [tok])


instance {-# OVERLAPPING #-} Alternative (ParserG tok) where
  empty = newEitherT (return (Left "error"))
  e1 <|> e2 = newEitherT $ do
    s <- get
    r1 <- runEitherT e1
    case r1 of
      Left _   -> do
        put s
        runEitherT e2
      Right r' -> return $ Right r' 


pTreeG :: (Eq a) => Forest (Maybe a) -> [a]-> ParserG a [a]
pTreeG = pTreeGBy (==)


pTreeGBy :: (a -> b -> Bool) -> Forest (Maybe b) -> [a] -> ParserG a [a]
pTreeGBy eq forest acc = 
  let lst = searchForestBy eq acc forest
      lst' = catMaybes lst
  in (satisfyG (\c -> any (eq c) lst') >>= \x -> pTreeGBy eq forest (acc++[x]))
     <|>
     if (not.null.filter isNothing) lst then return acc else left "not matched!"


pTreeAdvG :: (Eq a) => Forest (Maybe a) -> ParserG a [a]
pTreeAdvG = pTreeAdvGBy (==)


pTreeAdvGBy :: (a -> b -> Bool) -> Forest (Maybe b) -> ParserG a [a]
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
