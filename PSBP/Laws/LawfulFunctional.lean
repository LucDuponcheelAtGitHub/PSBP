import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

class LawfulFunctional
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program] : Prop where
  functional_identity :
    (asProgram id : program α α) =
      identity
  functional_sequential
      (αfβ : α → β)
      (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ : program α γ) =
      asProgram (βfγ ∘ αfβ)
