import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

import PSBP.Specifications.Creational

class Conditional
    (program : Type → Type → Type) where
  sum {α β γ : Type} :
    program γ α → program β α → program (γ ⊕ β) α

export Conditional (sum)

infixl:55 " ||| " => sum

def if_
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program α Bool →
  program α β →
  program α β →
  program α β :=
    λ αpb t_apβ f_apβ =>
      let_ αpb $
        asProgram (
          λ αab => match αab with
            | ⟨α, true⟩ => .inl α
            | ⟨α, false⟩ => .inr α
        ) >=>
        t_apβ ||| f_apβ
