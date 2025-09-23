import PSBP.Specifications.Parallel

class MonadAsync
    (computation : Type → Type) where
  async {α : Type} (ufα : Unit → α) : computation α

export MonadAsync (async)

instance
    [Monad computation]
    [MonadAsync computation] :
  Parallel
    (computationValuedFunction computation) where
    bothParallel :=
      λ αfcγ βfcδ ⟨α, β⟩ =>
        async (λ (_: Unit) => αfcγ α) >>=
          λ cγ => async (λ (_: Unit) => βfcδ β) >>=
            λ cδ => .mk <$> cγ <*> cδ
