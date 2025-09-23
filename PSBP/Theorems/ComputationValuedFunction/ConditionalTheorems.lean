import PSBP.Implementations.ComputationValuedFunction.FunctionalImplementation

import PSBP.Implementations.ComputationValuedFunction.SequentialImplementation

import PSBP.Implementations.ComputationValuedFunction.ConditionalImplementation

@[simp] theorem conditional_left
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (γpα : computationValuedFunction computation γ α)
  (βpα : computationValuedFunction computation β α) :
    (left >=> γpα ||| βpα
      : computationValuedFunction computation γ α) =
      γpα := by
  simp[left, asProgram, andThenProgram, sum, foldSum]

@[simp] theorem conditional_right
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (γpα : computationValuedFunction computation γ α)
  (βpα : computationValuedFunction computation β α) :
    (right >=> γpα ||| βpα
      : computationValuedFunction computation β α) =
      βpα := by
  simp[right, asProgram, andThenProgram, sum, foldSum]
