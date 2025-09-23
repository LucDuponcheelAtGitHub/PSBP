import PSBP.Specifications.Monoid

abbrev FailureT
    (ε : Type)
    (computation : Type → Type)
    (β : Type) : Type :=
  computation (ε ⊕ β)

instance
    [Monad computation] :
  Monad (FailureT ε computation) where
    map {α β : Type} :
      (α → β) →
      (computation (ε ⊕ α) → computation (ε ⊕ β)) :=
        λ αfβ cεoα =>
          cεoα >>= λ (εoα : ε ⊕ α) => match εoα with
            | .inr α => pure $ .inr (αfβ α)
            | .inl ε => pure $ .inl ε
    pure {α : Type} :
      α → computation (ε ⊕ α) :=
        λ α =>
          pure $ .inr α
    bind {α β : Type} :
        (computation (ε ⊕ α) →
          (α → computation (ε ⊕ β)) →
          computation (ε ⊕ β)) :=
      λ cεoα αfftεcβ =>
        cεoα >>= λ εoα => match εoα with
          | .inr α  => αfftεcβ α
          | .inl ε  => pure $ .inl ε

instance
    [Functor computation] :
  Functor (FailureT ε computation) where
    map {α β : Type} :
      (α → β) →
      (computation (ε ⊕ α) → computation (ε ⊕ β)) :=
      λ αfβ cεoα =>
        (λ εoα =>
          match εoα with
            | .inl ε => .inl ε
            | .inr α => .inr (αfβ α)) <$> cεoα

instance
    [Applicative computation]
    [Monoid ε] :
  Applicative (FailureT ε computation) where
    pure {α : Type} :
      α → computation (ε ⊕ α) :=
      λ α =>
        pure $ .inr α
    seq {α β : Type} :
      (computation (ε ⊕ (α → β))) →
      ((Unit → computation (ε ⊕ α)) →
      computation (ε ⊕ β)) :=
      λ cεoαfβ ufftεcα =>
        let cεoα :=
          (ufftεcα ())
        let εoαfεoαfβfεoβ {α β : Type} :
          (ε ⊕ α) → (ε ⊕ (α → β)) → (ε ⊕ β) :=
            λ εoα εoαfβ =>
              match εoα with
                | .inl ε =>
                  match εoαfβ with
                    | .inr _  => .inl ε
                    | .inl ε' => .inl (ε' * ε)
                | .inr α =>
                  match εoαfβ with
                    | .inr αfβ  => .inr (αfβ α)
                    | .inl ε' => .inl ε'
        εoαfεoαfβfεoβ <$> cεoα <*> cεoαfβ
