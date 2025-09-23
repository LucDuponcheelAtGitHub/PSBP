import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

class LawfulSequential
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program] : Prop where
  sequential_right_identity
      (αpβ : program α β) :
    (αpβ >=> identity) =
      αpβ
  sequential_left_identity
      (αpβ : program α β) :
    (identity >=> αpβ) =
      αpβ
  sequential_associativity
      (αpβ : program α β)
      (βpγ : program β γ)
      (γpδ : program γ δ) :
    ((αpβ >=> βpγ) >=> γpδ) =
      (αpβ >=> (βpγ >=> γpδ))
