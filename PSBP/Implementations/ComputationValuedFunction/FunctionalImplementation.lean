import PSBP.Specifications.Functional

instance
    [Applicative computation] :
  Functional
    (computationValuedFunction computation) where
    asProgram :=
      λ αfβ α => pure $ αfβ α
