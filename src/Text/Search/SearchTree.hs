module Text.Search.SearchTree where

import           Control.Applicative
import           Control.Monad                        (mzero,void)
import           Data.Attoparsec.Text                 
import qualified Data.Attoparsec.Internal.Types as AT (Parser(..),fromPos)
import           Data.Maybe                           (catMaybes) 
import           Data.Tree
--
import           Text.Search.Generic.SearchTree

pTree :: Forest (Maybe Char) -> String -> Parser (String,Int)
pTree forest acc = 
  let lst = searchForest acc forest
      lst' = catMaybes lst
  in (satisfy (\c -> c `elem` lst') >>= \x -> pTree forest (acc++[x]))
     <|>
     if Nothing `elem` lst then getPos >>= \e -> tokencloser >> return (acc,e) else mzero

pTreeAdv :: Forest (Maybe Char) -> Parser (Int,Int,String)
pTreeAdv forest = skipTill anyChar p
  where p = do
          b <- getPos
          (x,e) <- pTree forest []
          return (b+1,e,x)

getPos :: Parser Int
getPos = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

tokencloser :: Parser ()
tokencloser = void (satisfy (`elem` (" .,!?:;()-+=\"'`/\\|\8217\8220\8221" :: String))) <|> endOfInput

skipTill :: Alternative f => f a -> f b -> f b
skipTill p end = scan'
  where scan' = end <|> (p *> scan')
