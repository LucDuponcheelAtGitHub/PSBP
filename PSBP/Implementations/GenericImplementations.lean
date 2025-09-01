import PSBP.Specifications.ProgramSpecifications

import PSBP.Specifications.PositionalSpecification

instance
    [Functional program]
    [Sequential program] :
  Functorial program where
    andThenF {α β γ: Type} :
      program α β → (β → γ) → program α γ :=
        λ αpβ βfγ => αpβ >=> asProgram βfγ

instance
    [Functional program]
    [Creational program]
    [Sequential program] :
    Positional program where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ :=
      λ αpβ σpα =>
        let_ (σpα >=> αpβ)
