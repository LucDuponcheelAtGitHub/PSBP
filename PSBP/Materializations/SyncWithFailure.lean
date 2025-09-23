import PSBP.Abbreviations

import PSBP.Implementations.Transformations.FailureT

import PSBP.Materializations.Sync

abbrev programWithFirstFailure ε computation :=
  computationValuedFunction (FailureT ε computation)

def materializeWithFirstFailure
    [Monad computation] {α β : Type} :
  programWithFirstFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ αpwffcβ α =>
      αpwffcβ α

abbrev syncProgramWithFirstFailure ε :=
  programWithFirstFailure ε Sync

def materializeSyncWithFirstFailure {α β : Type} :
 syncProgramWithFirstFailure ε α β → function α (ε ⊕ β) :=
  materializeWithFirstFailure

abbrev programWithAccumulatedFailure ε computation :=
  computationValuedFunction (FailureT ε computation)

def materializeWithAccumulatedFailure
    [Monad computation]
    [Monoid ε] {α β : Type} :
  programWithAccumulatedFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ αpwafcβ α =>
      αpwafcβ α

abbrev syncProgramWithAccumulatedFailure ε :=
  programWithAccumulatedFailure ε Sync

def materializeSyncWithAccumulatedFailure
    [Monoid ε]
    {α β : Type} :
 syncProgramWithAccumulatedFailure ε α β → function α (ε ⊕ β) :=
  materializeWithAccumulatedFailure
