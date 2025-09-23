import PSBP.Specifications.Functorial

instance
    [Functor computation] :
  Functorial
    (computationValuedFunction computation) where
    functionAction :=
      λ βfγ =>
        λ αfcβ α => βfγ <$> αfcβ α
