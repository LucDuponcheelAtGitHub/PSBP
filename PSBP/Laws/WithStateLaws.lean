import PSBP.Specifications.ProgramSpecifications

import PSBP.Specifications.WithStateSpecification

import PSBP.Structures.ComputationValuedFunction

class LawfulStateOf (σ : Type) (computation : Type → Type)
    [Monad computation]
    [MonadStateOf σ computation] : Prop where
  stateOf_write_read :
    ((λ s => set s >>= λ _ => getThe σ) : σ → computation σ) =
      (pure)

export LawfulStateOf (stateOf_write_read)

attribute [simp] stateOf_write_read

class LawfulWithState
    [Functional program]
    [Sequential program]
    [WithState σ program] : Prop where
  withState_write_read :
    ((writeState : program σ Unit) >=>
      (readState : program Unit σ)) =
      identity
