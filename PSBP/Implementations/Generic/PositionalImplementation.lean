import PSBP.Abbreviations

import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

import PSBP.Specifications.Creational

import PSBP.Specifications.Positional

instance
    [Functional program]
    [Sequential program]
    [Creational program] :
    Positional program where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ :=
      λ αpβ σpα =>
        let_ (σpα >=> αpβ)
