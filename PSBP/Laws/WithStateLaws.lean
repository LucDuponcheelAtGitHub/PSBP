import PSBP.Specifications.ProgramSpecifications

import PSBP.Specifications.WithStateSpecification

import PSBP.Structures.ComputationValuedFunction

class LawfulStateOf (computation : Type → Type)
    [Monad computation]
    [MonadStateOf σ computation] : Prop where
  stateOf_write_read :
    (λ σ => set σ >>= λ _ : PUnit => get) =
      (pure : σ → computation σ)

@[simp] theorem stateOf_write_read_theorem
  {σ : Type}
    [Monad computation]
    [MonadStateOf σ computation] :
  (λ σ => set σ >>= λ _ : PUnit => get
    : σ → computation σ ) =
      (pure : σ → computation σ) :=
        sorry

class LawfulWithState (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [WithState σ program] : Prop where
  withState_write_read :
    (writeState >=> (readState : program Unit σ)) =
      identity

