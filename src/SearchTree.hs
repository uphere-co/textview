module SearchTree where

import           Control.Applicative
import           Control.Monad                        (mzero,void)
import           Data.Attoparsec.Text                 
import qualified Data.Attoparsec.Internal.Types as AT (Parser(..),fromPos)
import           Data.Function          (on)
import           Data.List              (sortBy)
import           Data.Maybe             (isNothing, catMaybes) 
import           Data.Tree


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
