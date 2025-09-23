import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

import PSBP.Specifications.Functional

class Creational
    (program : Type → Type → Type) where
  sequentialProduct {α β γ : Type} :
    program α β → program α γ → program α (β × γ)

export Creational (sequentialProduct)

infixl:60 " &&& " => sequentialProduct

def bothSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      (first >=> αpγ) &&& second >=>
        first &&& (second >=> βpδ)

infixl:60 " <&> " => bothSequential

def onlyFirst
    [Functional program]
    [Creational program]
    [Sequential program] :
  program α β → program (α × γ) (β × γ) :=
    λ αpβ => (first >=> αpβ) &&& second

def onlySecond
    [Functional program]
    [Creational program]
    [Sequential program] :
  program γ δ → program (α × γ) (α × δ) :=
    λ γpδ => first &&& (second >=> γpδ)

def let_
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program (α × β) γ → program α γ :=
    λ αpβ αaβpγ => identity &&& αpβ >=> αaβpγ
