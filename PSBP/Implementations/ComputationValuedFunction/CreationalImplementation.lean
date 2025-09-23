import PSBP.Specifications.Creational

instance
    [Applicative computation] :
  Creational
    (computationValuedFunction computation) where
    sequentialProduct :=
      λ αfcβ αfcγ α =>
        .mk <$> αfcβ α <*> αfcγ α
