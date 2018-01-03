module DFA
(
    Input,
    Output,
    Negated,
    Enabled,
    State,
    toInput,
    toInputs,
    fromInput,
    fromInputs,
    evaluate,
    evaluateWithStartState
) where

import DFA.Internal