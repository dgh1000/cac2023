module Cac.SimplerD.SearchSpecific where


import Cac.SimplerD.Data
import Control.Lens



listSteps :: Comp -> [Step]
listSteps compIn = 
    case view lastChosenStepType compIn of
      StepTypeAmpl -> listStepsPit compIn
      StepTypePit  -> listStepsTime compIn
      StepTypeSpan -> listStepsAmpl compIn
    

listStepsPit :: Comp -> [Step]
listStepsPit _ = map StepPit [30..90]


listStepsTime :: Comp -> [Step] 
listStepsTime compIn = map StepSpan [1, 2, 3, 4, 6, 8, 12] 


listStepsAmpl :: Comp -> [Step]
listStepsAmpl compIn = map StepAmpl [1.0,1.5..8.0]

addStep :: Comp -> Step -> Comp
addStep compIn = error "kbn23948"
