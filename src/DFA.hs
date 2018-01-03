module DFA
(
    Input,
    Emission,
    Output,
    Negated,
    Enabled,
    State,
    toInput,
    fromInput,
    step,
    mapEmission,
    evaluate,
    evaluateWithStartState
) where

import qualified Control.Monad.Trans.State.Lazy as S

data Input = Plus | Minus | ToggleNegate | ToggleEnabled
type Emission = Integer
type Output = [Emission]
type Negated = Bool
type Enabled = Bool
type State = (Negated, Enabled)

toInput :: Char -> Maybe Input
toInput '+' = Just Plus
toInput '-' = Just Minus
toInput 'n' = Just ToggleNegate
toInput 'e' = Just ToggleEnabled
toInput  _  = Nothing

fromInput :: Input -> Char
fromInput Plus          = '+'
fromInput Minus         = '-'
fromInput ToggleNegate  = 'n'
fromInput ToggleEnabled = 'e'

-- Determine new state of state machine along with transition emission
step :: Input -> State -> (Emission, (Negated, Enabled))
step ToggleNegate    (n, e)     = (0, (not n,     e))
step ToggleEnabled   (n, e)     = (0, (    n, not e))
step _             s@(_, False) = (0,                    s)
step Plus          s@(n, _)     = (if n then -1 else  1, s)
step Minus         s@(n, _)     = (if n then  1 else -1, s)

-- Helper function for "evaluate"'s traversal
mapEmission :: Input -> S.State (Negated, Enabled) Emission
mapEmission input = S.state (step input)

-- Process an input string and return the result
-- (False, True) is the start state: (not negated, enabled)
evaluate :: [Input] -> Output
evaluate = evaluateWithStartState (False, True)

evaluateWithStartState :: State -> [Input] -> Output
evaluateWithStartState state inputs = S.evalState (mapM mapEmission inputs) state