import PSBP.Specifications.ProgramSpecifications

import PSBP.Structures.ComputationValuedFunction

import PSBP.Implementations.ReactiveImplementations

instance [Applicative computation] :
    Functional
      (FromComputationValuedFunction computation) where
  asProgram :=
    λ αfβ ↦ ⟨λ α ↦ pure $ αfβ α⟩

instance [Functor computation] :
    Functorial
      (FromComputationValuedFunction computation) where
  andThenF :=
    λ ⟨αfcβ⟩ βfγ ↦ ⟨λ α ↦ βfγ <$> αfcβ α⟩

instance [Monad computation] :
    Sequential
      (FromComputationValuedFunction computation) where
  andThenP :=
    λ ⟨αfcβ⟩ ⟨βfcγ⟩ ↦ ⟨λ α ↦ αfcβ α >>= βfcγ⟩

instance [Applicative computation] :
    Creational
      (FromComputationValuedFunction computation) where
  productSeq := λ ⟨αfcβ⟩ ⟨αfcγ⟩ ↦
    ⟨λ α ↦ .mk <$> αfcβ α <*> αfcγ α⟩

def foldSum {γ β α : Type}
    (γfα : γ → α)
    (βfα : β → α)
    (sum : γ ⊕ β) : α :=
  match sum with
  | .inl tc => γfα tc
  | .inr tb => βfα tb

instance :
    Conditional
      (FromComputationValuedFunction computation) where
  sum := λ ⟨γfγα⟩ ⟨βfγα⟩ ↦ ⟨foldSum γfγα βfγα⟩

class MonadAsync
    (computation : Type → Type) where
  async {α : Type} (ufα : Unit → α) : computation α

export MonadAsync (async)

instance
    [Monad computation]
    [MonadAsync computation] :
  Parallel (FromComputationValuedFunction computation) where
    bothPar := λ ⟨αfcγ⟩ ⟨βfcδ⟩ =>
      ⟨λ ⟨α, β⟩ =>
        async (λ (_: Unit) => αfcγ α) >>=
          λ cγ => async (λ (_: Unit) => βfcδ β) >>=
            λ cδ => .mk <$> cγ <*> cδ⟩
