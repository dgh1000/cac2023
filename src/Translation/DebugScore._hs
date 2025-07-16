
module Translation.DebugScore where

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)
import Data.Ratio
import Common
import Util.Showable
import qualified Control.Exception as E
import qualified Sound.PortMidi as SP
import qualified Data.List as L
import qualified Data.Set as S
import qualified Text.XML.Light as XL
import Debug.Trace
import Sound.PortMidi hiding (name,initialize)
import System.Random
import Control.Exception
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.DeepSeq
import Control.Concurrent
import Text.XML.Light hiding (qName)
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Data.Map.Strict(Map)
import Data.Either
import Data.Maybe
import Data.Set (Set)
import Translation
import Translation.ToMidi2
import Instruments.Piano2
import Midi.Interface
import Score.ScoreData
import Score.XmlToScore
import Score.ShowScore
import XmlDoc.ParseXml
import Util.FileUtil
import Util.Exception
import Util.Map
import Instruments.Piano2

main = do
  putStrLn "writing score.txt"
  writeFile "score.txt" (showIString debugScore)
  gen <- newStdGen
  let metaMap = M.fromList $ map (iName &&& id) debugMetas
      s = TrState debugScore metaMap gen M.empty M.empty M.empty [] [] []
                  M.empty
      (err_or_shorts,finalState) =
            runState (runExceptT (toMidi (1,Just 2))) s
  putStrLn "writing time maps"
  let sf (n,atm) = let (Component _ _ items) = showI atm
                   in Component n True items
  writeFile "state.txt" . showiToString $
    Component "" False (map sf $ M.toAscList $ tsRelTimeMaps finalState)
  return ()


debugMetas = [piano]

piano = MetaInstr "piano" ["debug"]
  (Piano2 (M.fromList [("debug",(0,1))])
          (VelCurve [ (0.45, 10)
                    , (8.55, 100) ])
          (VelCurve [ (0.45, 10)
                    , (8.55, 100) ]))
  pianoRun




  

debugScore =
  Score
  { scTimeSigs = debugTimeSigs
  , scMarks    = debugMarks
  , scMarksByStaff = flipMap debugMarks
  , scMarkers = M.empty
  , scStaves = M.fromList [("debug", debugStaff)]
  , scUsedMsrs = A.listArray (1,2) (repeat True)
  }


insMark :: Mark3 -> Map String [Mark3]
insMark m = M.fromList [("debug", [m])]

{-
debugMarks = M.fromList
             [ (Loc 1 2, insMark $ Boundary23 Nothing)
             , (Loc 1 3, insMark $ Adjust23 True (0.2))
             , (Loc 1 4, insMark $ Adjust23 True (0.2))
             , (Loc 2 1, insMark $ Boundary23 Nothing)
             , (Loc 2 2, insMark $ W3)
             , (Loc 2 (3+15%16), insMark $ AbsWarp3 LeftWarp 3)
             , (Loc 3 1, insMark $ Pause3 0.1)
             ]
-}
debugMarks = M.fromList
             [ (Loc 1 2, insMark $ RampBeg3 1)
             , (Loc 1 4, insMark $ RampEnd3 2)
             , (Loc 2 1, insMark $ Boundary23 Nothing)
             , (Loc 2 2, insMark $ W3)
             , (Loc 2 (3+15%16), insMark $ AbsWarp3 LeftWarp 3)
             , (Loc 3 1, insMark $ Pause3 0.1)
             ]

debugTimeSigs = M.fromList [ (1,TimeSig 4 4)
                           , (2,TimeSig 4 4)
                           , (3,TimeSig 4 4)
                           , (4,TimeSig 4 4) ]


debugStaff =
  Staff
  { stName = "debug"
  , stDynamics = M.fromList [(Loc 1 1,[SimpleDyn 4 1])]
  , stHairpins = M.empty
  , stPedalEvts = M.empty
  , stMetSymMarks = M.empty
  , stMaxTrueEnd = Loc 3 1
  , stUsedMsrs   = S.fromList [1,2]
  , stSlurs = M.empty
  , stBrackets = M.empty
  , stChords = debugChords
  }


debugChords :: Map Loc (Map Int Chord)
debugChords =
  M.fromList [(Loc 1 1,debugVnChord)]

debugVnChord :: Map Int Chord
debugVnChord = M.fromList [(1, debugChord)]


debugChord :: Chord
debugChord =
  Chord
  { cEndLoc = Loc 2 1
  , cModifiers = S.empty
  , cNotes = debugNotes
  }

mkPitch = Pitch 60 0 0 5

debugNotes :: Notes
debugNotes =
  NSingles $ M.fromList [(1, Note mkPitch  False (Loc 2 1) NormalHead)]
