import PSBP.Specifications.WithFailure

import PSBP.Implementations.Transformations.FailureT

import PSBP.Specifications.Monoid

instance
    [Applicative computation] :
  WithFailure ε
    (computationValuedFunction
      (FailureT ε computation)) where
    failWithFunction {α β : Type} :
      function α ε →
      computationValuedFunction computation α (ε ⊕ β) :=
      λ αfε α =>
        pure $ .inl $ αfε α
