import PSBP.Specifications.Functional

import PSBP.Specifications.Sequential

import PSBP.Specifications.Conditional

class LawfulConditional
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Conditional program] : Prop where
  conditional_left
      (γpα : program γ α)
      (βpα : program β α) :
    (left >=> γpα ||| βpα) =
      γpα
  conditional_right
      (γpα : program γ α)
      (βpα : program β α) :
    (right >=> γpα ||| βpα) =
      βpα
