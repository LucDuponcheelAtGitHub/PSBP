import PSBP.Implementations.ComputationValuedFunction.FunctionalImplementation

import PSBP.Implementations.ComputationValuedFunction.SequentialImplementation

theorem sequential_right_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >=> asProgram id)
      = αpβ :=
    calc
      (αpβ >=> asProgram id)
          = λ α => αpβ α >>= λ β => pure (id β)
            := rfl
      _   = λ α => id <$> αpβ α
            := funext λ α => bind_pure_comp id (αpβ α)
      _   = λ α => αpβ α
            := funext λ α => id_map (αpβ α)
      _   = αpβ
            := rfl

@[simp] theorem sequential_right_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >=> asProgram id)
      = αpβ := by
  simp[andThenProgram, asProgram]

theorem sequential_left_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (asProgram id >=> αpβ)
      = αpβ :=
  calc
    (asProgram id >=> αpβ)
        = λ α => pure α >>= αpβ
          := rfl
    _   = λ α => αpβ α
          := (funext λ α => pure_bind α αpβ)
    _   = αpβ
          := rfl

theorem sequential_left_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (asProgram id >=> αpβ)
    = αpβ := by
  simp[asProgram, andThenProgram]

@[simp] theorem sequential_associative'
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ)
  (γpδ : computationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ
       : computationValuedFunction computation α δ) =
    (αpβ >=> (βpγ >=> γpδ)) :=
  calc
    ((αpβ >=> βpγ) >=> γpδ
       : computationValuedFunction computation α δ)
       = ((λ α => αpβ α >>= βpγ) >=> γpδ)
         := rfl
    _   = λ α => αpβ α >>= (λ β => βpγ β >>= γpδ)
          := funext λ α => bind_assoc (αpβ α) βpγ γpδ
    _   = (λ α => αpβ α >>= (βpγ >=> γpδ))
          := rfl
    _   = (αpβ >=> (βpγ >=> γpδ))
          := rfl

@[simp] theorem sequential_associative
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ)
  (γpδ : computationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ) =
      (αpβ >=> (βpγ >=> γpδ)) := by
  simp[andThenProgram]
