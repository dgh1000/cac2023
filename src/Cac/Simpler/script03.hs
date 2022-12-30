
import Control.Monad.State

type InnerState = Char

type OuterState = Integer

type S1 = State InnerState

type S2 = StateT OuterState S1


runS1 :: S1 a -> InnerState -> (a,InnerState)
runS1 m s = runState m s


runS2 :: S2 a -> OuterState -> S1 (a,OuterState)
runS2 m s = do
  innerState <- get
  let (e,innerState') = runS1 (runStateT m s) innerState
  put innerState'
  return e

foo :: S2 String
foo = do
  i <- get :: S2 Integer
  return "foo"
