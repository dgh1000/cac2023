 
import Instruments.Dynamics_was2

import qualified Data.List as L
import qualified Text.XML.Light as XL
import qualified Data.Map.Strict as M
import Text.XML.Light hiding (qName)
import Score.ScoreData
import Data.Map.Strict(Map)
import Util.FileUtil
import Score.XmlToScore
import Score.ShowScore
import XmlDoc.ParseXml
import Instruments.TimeMap
import Instruments
import Util.Showable
import Instruments.ShowInstruments



readXml :: IO Score
readXml = do
  buf <- readFileStrictly "/Users/Mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . XL.qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e


computeAtms :: Score -> Map String AbsTimeMap
computeAtms score = M.map (const $ toAbsolute btm) $ scStaves score
  where
    btm = computeBaseTimeMap score 1


main = do
  score <- readXml
  let atms = computeAtms score
      curves = hpDynCurves atms score
      g :: (String,Curve) -> ShowItem
      g (name,c) = Component name True [showI c]
  writeFile "curves.txt" $ showiToString $ Component "" False $
            map g $ M.toAscList curves
