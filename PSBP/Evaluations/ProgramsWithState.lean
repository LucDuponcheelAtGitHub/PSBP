import Init.Control.State

import PSBP.Implementations.ProgramImplementations
import PSBP.Implementations.WithStateImplementation

import PSBP.Materializations.WithStateMaterialization

import PSBP.Programs.ProgramsWithState

#eval
  materializeActiveWithState fibonacciIncrementingArgumentPair () 10 ==
  (89, 144)
