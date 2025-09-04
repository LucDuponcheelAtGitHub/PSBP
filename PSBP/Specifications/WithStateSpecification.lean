import PSBP.Specifications.ProgramSpecifications

class WithState
    (σ : outParam Type)
    (program : Type → Type → Type) where
  readState {α : Type} : program α σ
  writeState : program σ Unit

export WithState (readState writeState)

def modifyStateWith
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  (σ → σ) → program τ τ :=
    λ σfσ =>
      let_ ((readState >=> asProgram σfσ) >=> writeState) $
        in_ $
          first

def withInitialStateAsInitialValue
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithState σ program] :
  program σ τ → program α τ :=
    λ σpτ =>
      readState >=> σpτ
