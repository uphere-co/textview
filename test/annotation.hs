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

main = do
  let tagged = AnnotText (tagText [((),15,20),((),120,126)] txt)
      xss = lineSplitAnnot Nothing 80 tagged
  flip mapM_ xss $ \xs -> mapM_ (cutePrintAnnot isJust) xs
