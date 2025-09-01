import PSBP.Specifications.ProgramSpecifications

import PSBP.Laws.ProgramLaws

import PSBP.Structures.ComputationValuedFunction

import PSBP.Implementations.ProgramImplementations

set_option linter.unusedVariables false

open Function

@[simp] theorem functional_identity
  {α : Type}
    [Applicative computation] :
    (identity : FromComputationValuedFunction computation α α)
      = asProgram id := by
  calc
    identity
        = asProgram id
          := rfl

theorem functional_sequential'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β)
  (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) := by
  calc
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
        = (⟨λ α => pure $ αfβ α⟩ >=> ⟨λ β => pure $ βfγ β⟩)
          := rfl
    _   = ⟨λ α => (pure $ αfβ α) >>= λ β => pure $ βfγ β⟩
          := rfl
    _   = ⟨λ α => pure $ βfγ (αfβ α)⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α => pure_bind (αfβ α) (λ β => pure $ βfγ β))
    _   = ⟨λ α => pure $ (βfγ ∘ αfβ) α⟩
          := rfl
    _   = asProgram (βfγ ∘ αfβ)
          := rfl

@[simp] theorem functional_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β)
  (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) := by
    simp[asProgram, andThenP]

theorem functorial_identity'
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >-> id :
      FromComputationValuedFunction computation α β)
      = αpβ := by
  let αfcβ := αpβ.toComputationValuedFunction
  calc
    (αpβ >-> id)
        = ⟨λ α => id <$> αfcβ α⟩
          := rfl
    _   = ⟨λ α => αfcβ α ⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α => id_map (αfcβ α))
    _   = ⟨αfcβ⟩
          := rfl

@[simp] theorem functorial_identity
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >-> id :
      FromComputationValuedFunction computation α β)
      = αpβ := by
    simp[andThenF]

theorem functorial_sequential'
  {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βfγ : β → γ)
  (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ :
      FromComputationValuedFunction computation α δ)
      = (αpβ >-> (γfδ ∘ βfγ)) := by
  let αfcβ := αpβ.toComputationValuedFunction
  calc
    ((αpβ >-> βfγ) >-> γfδ)
        = (⟨λ α => βfγ <$> αfcβ α⟩ >-> γfδ)
          := rfl
    _   = ⟨λ α => γfδ <$> (λ α => βfγ <$> αfcβ α) α⟩
          := rfl
    _   = ⟨λ α => γfδ <$> βfγ <$> αfcβ α⟩
          := rfl
    _   = ⟨λ α => (γfδ ∘ βfγ) <$> αfcβ α⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α => Eq.symm (comp_map βfγ γfδ (αfcβ α)))
    _   = (αpβ >-> (γfδ ∘ βfγ))
          := rfl

@[simp] theorem function_sequential
    (αfβ : α → β)
    (βfγ : β → γ)
    (a : α):
  ((βfγ ∘ αfβ): α → γ) =
    λ α => βfγ (αfβ α) := by
     rfl

@[simp] theorem functorial_sequential
    {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βfγ : β → γ)
  (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ :
      FromComputationValuedFunction computation α δ)
      = (αpβ >-> (γfδ ∘ βfγ)) := by
        simp[andThenF, function_sequential]

theorem sequential_right_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >=> asProgram id :
      FromComputationValuedFunction computation α β)
    = αpβ := by
   let αfcβ := αpβ.toComputationValuedFunction
   calc
      (αpβ >=> asProgram id :
          FromComputationValuedFunction computation α β)
          = ⟨λ α => αfcβ α >>= λ β => pure (id β)⟩
            := rfl
      _   = ⟨λ α => id <$> αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => bind_pure_comp id (αfcβ α))
      _   = ⟨λ α => αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => id_map (αfcβ α))
      _   = αpβ
            := rfl

@[simp] theorem sequential_right_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    ((αpβ >=> asProgram id) :
      FromComputationValuedFunction computation α β)
      = αpβ := by simp[andThenP]

theorem sequential_left_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (asProgram id >=> αpβ :
      FromComputationValuedFunction computation α β)
    = αpβ := by
    let αfcβ := αpβ.toComputationValuedFunction
    calc
      (asProgram id >=> ⟨αfcβ⟩ :
        FromComputationValuedFunction computation α β)
          = ⟨λ α => pure α >>= αfcβ⟩
            := rfl
      _   = ⟨λ α => αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => pure_bind α αfcβ)
      _   = ⟨αfcβ⟩
            := rfl

@[simp] theorem sequential_left_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (asProgram id >=> αpβ :
      FromComputationValuedFunction computation α β)
    = αpβ := by simp[andThenP]

theorem sequential_associative'
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ)
  (γpδ : FromComputationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ) =
      (αpβ >=> (βpγ >=> γpδ)) := by
  let αfcβ : α → computation β := αpβ.toComputationValuedFunction
  let βfcγ : β → computation γ := βpγ.toComputationValuedFunction
  let γfcδ : γ → computation δ := γpδ.toComputationValuedFunction
  let βfcδ : β → computation δ := λ β => βfcγ β >>= γfcδ
  calc
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ)
        = (⟨λ α => αfcβ α >>= βfcγ⟩ >=> ⟨γfcδ⟩)
          := rfl
    _   = ⟨λ α => αfcβ α >>= βfcγ >>= γfcδ⟩
          := rfl
    _   = ⟨λ α => αfcβ α >>= (λ β => βfcγ β >>= γfcδ)⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α => bind_assoc (αfcβ α) βfcγ γfcδ)
    _   = (⟨λ α => αfcβ α >>= βfcδ⟩ :
            FromComputationValuedFunction computation α δ)
          := rfl
    _   = (αpβ >=> (βpγ >=> γpδ):
            FromComputationValuedFunction computation α δ)
          := rfl

@[simp] theorem sequential_associative
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ)
  (γpδ : FromComputationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ) =
      (αpβ >=> (βpγ >=> γpδ)) := by simp[andThenP]

theorem creational_onlyFirst_asProgram'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β) :
    (onlyFirst (asProgram αfβ) :
      FromComputationValuedFunction computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by
  calc
    (onlyFirst (asProgram αfβ))
        = onlyFirst ⟨λ α => pure $ αfβ α⟩ :=
        rfl
    _   = ((first : FromComputationValuedFunction computation (α × γ) α) >=>
            (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = (((asProgram λ (α, _) => α) :
            FromComputationValuedFunction computation (α × γ) α) >=>
              (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = ((⟨λ (α, _) => pure α⟩ :
            FromComputationValuedFunction computation (α × γ) α) >=>
              (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = (⟨λ (α, _) => pure α >>= (λ α => pure $ αfβ α)⟩ :
            FromComputationValuedFunction computation (α × γ) β) &&& second :=
        rfl
    _   = (⟨(λ (α, _) => pure $ αfβ α)⟩ :
            FromComputationValuedFunction computation (α × γ) β) &&& second :=
        congrArg
          (λ (αfcβ : α → computation β) => ((⟨λ (α, _) => αfcβ α⟩ :
            FromComputationValuedFunction computation (α × γ) β) &&& second))
          (funext (λ α => pure_bind α (λ α => pure $ αfβ α)))
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction computation (α × γ) β) &&& second :=
        rfl
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction computation (α × γ) β) &&&
              (asProgram (λ (_, γ) => γ) :
                FromComputationValuedFunction computation (α × γ) γ) :=
        rfl
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction computation (α × γ) β) &&&
              (⟨λ (_, γ) => pure $ γ⟩ :
                FromComputationValuedFunction computation (α × γ) γ) :=
        rfl
    _   = (⟨λ (α, γ) => (Prod.mk <$> (pure $ αfβ α)) <*> (pure $ γ)⟩ :
             FromComputationValuedFunction computation (α × γ) (β × γ)) :=
        rfl
    _   = (⟨λ (α, γ) => (pure $ Prod.mk (αfβ α)) <*> (pure $ γ)⟩ :
             FromComputationValuedFunction computation (α × γ) (β × γ)) :=
        congrArg
         (FromComputationValuedFunction.mk ∘
           ((λ αfβaγ => λ (α, γ) => αfβaγ α <*> (pure $ γ)) :
             (α → computation (γ → (β × γ))) → ((α × γ) → computation (β × γ))))
          (funext λ α => (map_pure (Prod.mk) (αfβ α)))
    _   = (⟨λ (α, γ) => Prod.mk (αfβ α) <$> (pure $ γ)⟩) :=
        congrArg
         FromComputationValuedFunction.mk
          (funext λ (α, γ) => (pure_seq (Prod.mk (αfβ α)) (pure $ γ)))
    _   = (⟨λ (α, γ) => pure (Prod.mk (αfβ α) γ)⟩) :=
        congrArg
         FromComputationValuedFunction.mk
          (funext λ (α, γ) => (map_pure (Prod.mk (αfβ α)) γ))
    _   = (⟨λ (α, γ) => pure $ (αfβ α, γ)⟩) :=
        rfl
    _   = (asProgram (λ (α, γ) => (αfβ α, γ))) :=
        rfl

@[simp] theorem creational_onlyFirst_asProgram
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β) :
    (onlyFirst (asProgram αfβ) :
      FromComputationValuedFunction computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ)))
      := by simp [onlyFirst, asProgram, productSeq, first, second]

@[simp] theorem creational_onlyFirst_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      FromComputationValuedFunction computation (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ :
        FromComputationValuedFunction
          computation (α × δ) (γ × δ)) := by
          simp[onlyFirst, andThenP, asProgram, productSeq, first, second]

@[simp] theorem creational_onlyFirst_first
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (onlyFirst αpβ >=> (first : FromComputationValuedFunction computation (β × γ) β)
      : FromComputationValuedFunction computation (α × γ) β) =
      ((first : FromComputationValuedFunction computation (α × γ) α) >=> αpβ) := by
          simp[onlyFirst, andThenP, asProgram, productSeq, first, second]

@[simp] theorem creational_onlyFirst_applyAtSecond
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ
      : FromComputationValuedFunction computation (α × γ) (β × δ)) =
      (applyAtSecond γfδ >=> onlyFirst αpβ) := by
          simp[onlyFirst, andThenP, applyAtSecond, asProgram, productSeq, first, second]

@[simp] theorem creational_onlyFirst_assoc
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc
      : FromComputationValuedFunction computation ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ) := by
        simp[onlyFirst, andThenP, asProgram, productSeq, first, second, assoc]

@[simp] theorem conditional_left
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (γpα : FromComputationValuedFunction computation γ α)
  (βpα : FromComputationValuedFunction computation β α) :
    (left >=> γpα ||| βpα
      : FromComputationValuedFunction computation γ α) =
      γpα := by
       simp[left, asProgram, andThenP, foldSum]

@[simp] theorem conditional_right
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (γpα : FromComputationValuedFunction computation γ α)
  (βpα : FromComputationValuedFunction computation β α) :
    (right >=> γpα ||| βpα
      : FromComputationValuedFunction computation β α) =
      βpα := by
       simp[right, asProgram, andThenP, foldSum]
