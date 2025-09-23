import PSBP.Implementations.ComputationValuedFunction.FunctionalImplementation

import PSBP.Implementations.ComputationValuedFunction.SequentialImplementation

@[simp] theorem functional_identity
  {α : Type}
    [Applicative computation] :
    (identity :
      computationValuedFunction computation α α)
      = asProgram id :=
  calc
    identity
        = asProgram id
          := rfl

theorem functional_sequential'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : function α β)
  (βfγ : function β γ) :
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) :=
  calc
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
    _   = λ α => (pure $ αfβ α) >>= λ β => pure $ βfγ β
          := rfl
    _   = λ α => pure $ βfγ (αfβ α)
          := funext λ α =>
               pure_bind (αfβ α) (λ β => pure $ βfγ β)
    _   = (asProgram (βfγ ∘ αfβ) :
            computationValuedFunction computation α γ)
          := rfl

@[simp] theorem functional_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : function α β)
  (βfγ : function β γ) :
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) := by
  simp[asProgram, andThenProgram]
