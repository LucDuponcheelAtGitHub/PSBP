import PSBP.Implementations.ProgramImplementations
import PSBP.Implementations.WithFailureImplementation

import PSBP.Implementations.ListMonoidImplementation

import PSBP.Materializations.WithValidationMaterialization

import PSBP.Programs.ProgramsWithListOfStringsValidation

#eval
  materializeActiveWithValidation accumulatingSafeDiv (10, 5) ==
    .inr 2

#eval
  materializeActiveWithValidation accumulatingSafeDiv (10, 0) ==
    .inl ["tried to divide 10 by 0"]

#eval
  materializeActiveWithValidation accumulatingSafeDivIsOne (10, 10) ==
    .inr true

#eval
  materializeActiveWithValidation accumulatingSafeDivIsOne (10, 0) ==
    .inl ["tried to divide 10 by 0"]

#eval
  materializeActiveWithValidation twiceAccumulatingSafeDivIsOne ((10, 5), 2) ==
    .inr 1

#eval
  materializeActiveWithValidation twiceAccumulatingSafeDivIsOne ((10, 5), 0) ==
    .inl ["tried to divide 2 by 0"]

#eval
  materializeActiveWithValidation twiceAccumulatingSafeDivIsOne ((10, 0), 2) ==
    .inl ["tried to divide 10 by 0"]

#eval
  materializeActiveWithValidation twiceAccumulatingSafeDivIsOne ((10, 0), 0) ==
    .inl ["tried to divide 10 by 0"]

#eval
  materializeActiveWithValidation accumulatingSafeDivProduct ((10, 5), (8, 2)) ==
    .inr (2, 4)

#eval
  materializeActiveWithValidation accumulatingSafeDivProduct ((10, 0), (8, 2)) ==
    .inl ["tried to divide 10 by 0"]

#eval
  materializeActiveWithValidation accumulatingSafeDivProduct ((10, 5), (8, 0)) ==
    .inl ["tried to divide 8 by 0"]

#eval
  materializeActiveWithValidation accumulatingSafeDivProduct ((10, 0), (8, 0)) ==
    .inl ["tried to divide 10 by 0", "tried to divide 8 by 0"]

#eval
  materializeActiveWithValidation addAccumulatingSafeDivProduct ((10, 5), (8, 2)) ==
    .inr 6

#eval
  materializeActiveWithValidation addAccumulatingSafeDivProduct ((10, 0), (8, 2)) ==
    .inl ["tried to divide 10 by 0"]

#eval
  materializeActiveWithValidation addAccumulatingSafeDivProduct ((10, 5), (8, 0)) ==
   .inl ["tried to divide 8 by 0"]

#eval
  materializeActiveWithValidation addAccumulatingSafeDivProduct ((10, 0), (8, 0)) ==
    .inl ["tried to divide 10 by 0", "tried to divide 8 by 0"]
