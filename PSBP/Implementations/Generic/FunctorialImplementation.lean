import PSBP.Abbreviations

import PSBP.Specifications.Functional

import PSBP.Specifications.Functorial

import PSBP.Specifications.Sequential

instance
    [Functional program]
    [Sequential program] :
  Functorial program where
    functionAction {α β γ: Type} :
      function β γ → (program α β → program α γ) :=
        λ βfγ αpβ => αpβ >=> asProgram βfγ
