@[simp] theorem function_sequential
    (αfβ : α → β)
    (βfγ : β → γ):
  ((βfγ ∘ αfβ): α → γ) =
    λ α => βfγ (αfβ α) := by
      rfl
