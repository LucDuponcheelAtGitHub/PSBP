import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

import PSBP.Specifications.Creational

class WithState
    (σ : outParam Type)
    (program : Type → Type → Type) where
  readState {α : Type} : program α σ
  writeState : program σ Unit

export WithState (readState writeState)

def modifyStateWithFunction
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  function σ σ → program α α :=
    λ σfσ =>
      let_ ((readState >=> asProgram σfσ) >=> writeState) $
        first

def usingInitialStateAsInitialValue
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  program σ β → program α β :=
    λ σpβ =>
      readState >=> σpβ
