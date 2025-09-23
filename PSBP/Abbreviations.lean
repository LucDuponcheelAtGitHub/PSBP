abbrev function α β := α → β

abbrev computationValuedFunction
    (computation : Type → Type) (α β : Type) :=
  function α (computation β)
