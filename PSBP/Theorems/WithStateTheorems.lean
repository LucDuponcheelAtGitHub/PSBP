import PSBP.Specifications.ProgramSpecifications

import PSBP.Structures.ComputationValuedFunction

import PSBP.Implementations.ProgramImplementations

import PSBP.Implementations.WithStateImplementation

import PSBP.Laws.WithStateLaws

@[simp] theorem withState_write_read
    [Monad computation]
    [MonadStateOf σ computation]
    [LawfulStateOf σ computation] :
  ((writeState >=> readState) :
      FromComputationValuedFunction computation σ σ) =
    identity := by simp [andThenP, identity, asProgram]
