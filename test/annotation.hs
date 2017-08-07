{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List       (mapAccumL)
import           Data.List.Split (splitWhen)
import           Data.Maybe      (isJust)
import           Data.Text       (Text)
import qualified Data.Text  as T
--
import           Text.Annotation.Type
import           Text.Annotation.View
import           Text.Annotation.Util.Doc
--


txt = "President Trump's national security adviser H.R. McMaster on Saturday defended President Trump's strategy on winning the war in Afghanistan.\nThe president has not announced a comprehensive strategy on Afghanistan yet, but according to McMaster he has made \"a number of decisions\" on the military approach there.\n\"The president’s already made some important decisions on Afghanistan,\" McMaster said in an interview with conservative radio host Hugh Hewitt that aired Saturday.\n"


printAnnot tagged = mapM_ (mapM_ (cutePrintAnnot isJust)) (lineSplitAnnot Nothing 80 tagged)


printAnnotWithLabel f tagged = mapM_ (mapM_ (cutePrintAnnotWithLabel f)) (lineSplitAnnot Nothing 80 tagged)


main = do
  let tagged1 = AnnotText (tagText [((),11,15),((),118,124)] txt)
      tagged2 = AnnotText (tagText [(1,11,15),(2,118,124)] txt)
  printAnnot tagged1
  printAnnotWithLabel (fmap (T.pack . show)) tagged2
