import PSBP.Implementations.ProgramImplementations
import PSBP.Implementations.WithFailureImplementation

import PSBP.Materializations.WithFailureMaterialization

import PSBP.Programs.ProgramsWithFailure

#eval
  materializeActiveWithFailure safeDiv (10, 5) ==
    .inr 2

#eval
  materializeActiveWithFailure safeDiv (10, 0) ==
    .inl "tried to divide 10 by 0"

#eval
  materializeActiveWithFailure safeDivIsOne (10, 10) ==
    .inr true

#eval
  materializeActiveWithFailure safeDivIsOne (10, 0) ==
    .inl "tried to divide 10 by 0"

#eval
  materializeActiveWithFailure twiceSafeDiv ((10, 5), 2) ==
    .inr 1

#eval
  materializeActiveWithFailure twiceSafeDiv ((10, 5), 0) ==
    .inl "tried to divide 2 by 0"

#eval
  materializeActiveWithFailure twiceSafeDiv ((10, 0), 2) ==
    .inl "tried to divide 10 by 0"

#eval
  materializeActiveWithFailure twiceSafeDiv ((10, 0), 0) ==
    .inl "tried to divide 10 by 0"
