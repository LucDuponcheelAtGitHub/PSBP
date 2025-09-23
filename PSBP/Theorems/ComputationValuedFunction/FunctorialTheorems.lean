import PSBP.Implementations.ComputationValuedFunction.FunctorialImplementation

import PSBP.Implementations.ComputationValuedFunction.SequentialImplementation

theorem functorial_identity'
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >-> id)
      = αpβ :=
  calc
    (αpβ >-> id)
        = λ α => id <$> αpβ α
          := rfl
    _   = λ α => αpβ α
          := funext λ α => id_map (αpβ α)
    _   = αpβ
          := rfl

@[simp] theorem functorial_identity
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >-> id)
      = αpβ := by
  simp[functionAction, andThenFunction]

theorem functorial_sequential'
  {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : computationValuedFunction computation α β)
  (βfγ : function β γ)
  (γfδ : function γ δ) :
    ((αpβ >-> βfγ) >-> γfδ)
      = (αpβ >-> (γfδ ∘ βfγ)) :=
  calc
    (αpβ >-> βfγ) >-> γfδ
    _   = λ α => γfδ <$> βfγ <$> αpβ α
          := rfl
    _   = λ α => (γfδ ∘ βfγ) <$> αpβ α
          := funext λ α =>
               Eq.symm (comp_map βfγ γfδ (αpβ α))
    _   = (αpβ >-> (γfδ ∘ βfγ))
          := rfl

@[simp] theorem functorial_sequential
  {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : computationValuedFunction computation α β)
  (βfγ : function β γ)
  (γfδ : function γ δ) :
    ((αpβ >-> βfγ) >-> γfδ)
      = (αpβ >-> (γfδ ∘ βfγ)) := by
  simp[functionAction, andThenFunction, comp_map]


