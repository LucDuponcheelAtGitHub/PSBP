import PSBP.Specifications.Conditional

def foldSum {γ β α : Type}
    (γfα : γ → α)
    (βfα : β → α)
    (sum : γ ⊕ β) : α :=
  match sum with
  | .inl tc => γfα tc
  | .inr tb => βfα tb

instance :
  Conditional
    (computationValuedFunction computation) where
    sum :=
      λ γfγα βfγα =>
        foldSum γfγα βfγα
