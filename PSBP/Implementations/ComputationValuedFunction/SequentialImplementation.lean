import PSBP.Abbreviations

import PSBP.Specifications.Sequential

instance
    [Monad computation] :
  Sequential
    (computationValuedFunction computation) where
    andThenProgram :=
      λ αfcβ βfcγ α =>
        αfcβ α >>= βfcγ
