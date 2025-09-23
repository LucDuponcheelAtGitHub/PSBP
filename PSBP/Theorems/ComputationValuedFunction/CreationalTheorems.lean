import PSBP.Implementations.ComputationValuedFunction.FunctionalImplementation

import PSBP.Implementations.ComputationValuedFunction.SequentialImplementation

import PSBP.Implementations.ComputationValuedFunction.CreationalImplementation

@[simp] theorem creational_onlyFirst_asProgram
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β) :
    (onlyFirst (asProgram αfβ) :
      computationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by
    simp [ onlyFirst, first, andThenProgram, asProgram,
    sequentialProduct, second]

@[simp] theorem creational_onlyFirst_sequential
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      computationValuedFunction
        computation (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ :
        computationValuedFunction
          computation (α × δ) (γ × δ)) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second]

@[simp] theorem creational_onlyFirst_first
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (onlyFirst αpβ >=> first :
      computationValuedFunction computation (α × γ) β) =
      (first >=> αpβ) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second]

@[simp] theorem creational_onlyFirst_applyAtSecond
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ) =
      (applyAtSecond γfδ >=> onlyFirst αpβ) := by
  simp[onlyFirst, andThenProgram, applyAtSecond, asProgram,
  sequentialProduct, first, second]

@[simp] theorem creational_onlyFirst_assoc
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc :
      computationValuedFunction
        computation ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second, assoc]
