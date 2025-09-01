structure FromComputationValuedFunction
    (computation : Type → Type) (α β : Type) where
  toComputationValuedFunction : α → computation β
