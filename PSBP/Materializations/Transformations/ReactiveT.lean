import PSBP.Abbreviations

import PSBP.Materializations.Sync

abbrev ReactiveT
    (ρ : Type)
    (computation: Type → Type)
    (α : Type) :=
  (α → computation ρ) → computation ρ

instance :
  Functor (ReactiveT ρ computation) where
    map :=
      λ αfβ rcα γ =>
        rcα (γ ∘ αfβ)

instance :
  Applicative (ReactiveT ρ computation) where
    pure := λ α αfcρ =>
      αfcρ α
    seq :=
      λ rcαfβ ufrtρcα βfcρ =>
        rcαfβ $
          λ αfβ =>
            ufrtρcα () (βfcρ ∘ αfβ)

instance :
  Monad (ReactiveT ρ computation) where
    bind :=
      λ rcα αfrtρcβ βfcρ =>
        rcα λ α => αfrtρcβ α βfcρ

abbrev ReactiveProgram ρ computation :=
  computationValuedFunction (ReactiveT ρ computation)

