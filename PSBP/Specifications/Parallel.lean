import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

class Parallel (program : Type → Type → Type) where
  bothParallel {α β γ δ : Type} :
  program α γ → program β δ → program (α × β) (γ × δ)

export Parallel (bothParallel)

infixl:60 " |&| " => bothParallel

def parallelProduct {α β γ : Type}
    [Functional program]
    [Sequential program]
    [Parallel program] :
  program α β → program α γ → program α (β × γ) :=
   λ αpβ αpγ => duplicate >=> αpβ |&| αpγ

infixl:60 " &|& " => parallelProduct
