import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

import PSBP.Specifications.WithState

class LawfulWithState
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [WithState σ program] : Prop where
  withState_write_read :
    ((writeState : program σ Unit) >=> readState) =
      identity
