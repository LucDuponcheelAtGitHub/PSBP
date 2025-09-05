namespace All

#eval "--begin--"

class Functional
    (program : Type → Type → Type) where
  asProgram {α β : Type} :
    (α → β) → program α β

export Functional (asProgram)

class Functorial
    (program : Type → Type → Type) where
  andThenF {α β γ : Type} :
    program α β → (β → γ) → program α γ

export Functorial (andThenF)

infixl:50 " >-> " => andThenF

class Sequential
    (program : Type → Type → Type) where
  andThenP {α β γ : Type} :
    program α β → program β γ → program α γ

export Sequential (andThenP)

infixl:50 " >=> " => andThenP

class Creational
    (program : Type → Type → Type) where
  productSeq {α β γ : Type} :
    program α β → program α γ → program α (β × γ)

export Creational (productSeq)

infixl:60 " &&& " => productSeq

class Conditional
    (program : Type → Type → Type) where
  sum {α β γ : Type} :
    program γ α → program β α → program (γ ⊕ β) α

export Conditional (sum)

infixl:55 " ||| " => sum

class Parallel (program : Type → Type → Type) where
  bothPar {α β γ δ : Type} :
  program α γ → program β δ → program (α × β) (γ × δ)

export Parallel (bothPar)

infixl:60 " |&| " => bothPar

-- exercise
def identity
    [Functional program] :
  program α α :=
    asProgram λ α => α
--

def let_
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program (α × β) γ → program α γ :=
    λ αpβ αaβpγ => identity &&& αpβ >=> αaβpγ

 def in_ : α → α := id

def creational_αfγ {α β γ : Type} :
    (α → β) → ((α × β) → γ) → (α → γ) :=
  λ αfβ αaβfγ α =>
    let β := αfβ α
    αaβfγ (α, β)

def sequential_αfγ {α β γ : Type} :
    (α → β) → (β → γ) → (α → γ) :=
  λ αfβ βfγ α =>
    let β := αfβ α
    βfγ β

 def if_
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program α Bool →
  program α β →
  program α β →
  program α β :=
    λ αpb t_apβ f_apβ =>
      let_ αpb $
        in_ $
          asProgram (
            λ αab => match αab with
              | ⟨α, true⟩ => .inl α
              | ⟨α, false⟩ => .inr α
          ) >=>
          t_apβ ||| f_apβ

def else_ : α → α := id

-- exercise
def isZeroF: Nat → Bool :=
  λ n => n == 0

def isOneF : Nat → Bool :=
  λ n => n == 1

def oneF : Nat → Nat :=
  λ _ => 1

def minusOneF : Nat → Nat :=
  λ n => n - 1

def minusTwoF : Nat → Nat :=
  λ n => n - 2

def addF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n + m

def multiplyF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n * m

def isZero
    [Functional program] :
  program Nat Bool :=
    asProgram isZeroF

def isOne
    [Functional program] :
  program Nat Bool :=
    asProgram isOneF

def one
    [Functional program] :
  program Nat Nat :=
    asProgram oneF

def minusOne
    [Functional program] :
  program Nat Nat :=
    asProgram minusOneF

def minusTwo
    [Functional program] :
  program Nat Nat :=
    asProgram minusTwoF

def add
    [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram addF

def multiply
    [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram multiplyF
--

unsafe def fibonacci
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      else_ $
        if_ isOne one $
          else_ $
            (minusOne >=> fibonacci) &&&
            (minusTwo >=> fibonacci) >=>
            add

-- exercise
unsafe def fibonacci'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      if_ isOne one $
        (minusOne >=> fibonacci') &&&
        (minusTwo >=> fibonacci') >=>
        add
--

unsafe def factorial
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      else_ $
        let_ (minusOne >=> factorial) $
          in_ $
            multiply

-- exercise
unsafe def factorial'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      let_ (minusOne >=> factorial') $
        multiply
--

def dup
    [Functional program] :
  program α (α × α) :=
    asProgram λ α => (α, α)

def productPar {α β γ : Type}
    [Functional program]
    [Sequential program]
    [Parallel program] :
  program α β → program α γ → program α (β × γ) :=
   λ αpβ αpγ => dup >=> αpβ |&| αpγ

infixl:60 " &|& " => productPar

unsafe def parallelFibonacci
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [Parallel program] :
  program Nat Nat :=
    if_ isZero one $
      if_ isOne one $
        (minusOne >=> parallelFibonacci) &|&
        (minusTwo >=> parallelFibonacci) >=>
        add

unsafe def fibonacci''
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one (
      if_ isOne one (
        (minusOne >=> fibonacci') &&&
        (minusTwo >=> fibonacci') >=>
        add
      )
    )

unsafe def factorial''
    [Functional program]
    [Sequential program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one (
      let_ (minusOne >=> factorial')
        multiply
    )

-- exercise
def first
    [Functional program] :
  program (α × β) α :=
    asProgram λ (α, _) => α

def second
    [Functional program] :
  program (α × β) β :=
    asProgram λ (_, β) => β

def bothSeq
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      (first >=> αpγ) &&& second >=>
        first &&& (second >=> βpδ)

infixl:60 " <&> " => bothSeq
--

-- exercise
def onlyFirst
    [Functional program]
    [Creational program]
    [Sequential program] :
  program α β → program (α × γ) (β × γ) :=
    λ αpβ => (first >=> αpβ) &&& second

def onlySecond
    [Functional program]
    [Creational program]
    [Sequential program] :
  program γ δ → program (α × γ) (α × δ) :=
    λ γpδ => first &&& (second >=> γpδ)

def bothSeq'
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      onlyFirst αpγ >=> onlySecond βpδ
--

-- exercise
instance
    [Functional program]
    [Sequential program] :
  Functorial program where
    andThenF {α β γ: Type} :
      program α β → (β → γ) → program α γ :=
        λ αpβ βfγ => αpβ >=> asProgram βfγ
--

def twiceMinusOneFunctorial
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >-> addF

def twiceMinusOneSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >=> add

-- exercise
def productSeq'
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program α γ → program α (β × γ) :=
    λ αpβ αpγ =>
      let_ αpβ $
        let_ (first >=> αpγ) $
          asProgram λ ((_, β), γ) => (β, γ)
--

-- exercise
def sum'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program γ α → program β α → program (γ ⊕ β) α :=
    λ γpα βpα =>
      if_ (asProgram
            (λ γoβ => match γoβ with
              | Sum.inl _ => True
              | Sum.inr _ => False))
        (asProgram
          (λ γoβ => match γoβ with
            | Sum.inl γ => γ
            | Sum.inr β => sorry) >=> γpα) $
        asProgram
          (λ γoβ => match γoβ with
            | Sum.inl _ => sorry
            | Sum.inr β => β) >=> βpα
--

class LawfulFunctional
    [Functional program]
    [Sequential program] : Prop where
  functional_identity :
    (asProgram id : program α α) =
      identity
  functional_sequential
      (αfβ : α → β)
      (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ : program α γ) =
      asProgram (βfγ ∘ αfβ)

class LawfulFunctorial
    [Functorial program] : Prop where
  functorial_identity
    (αpβ : program α β) :
    (αpβ >-> id) =
      αpβ
  functorial_sequential
      (αpβ : program α β)
      (βfγ : β → γ)
      (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ) =
      (αpβ >-> (γfδ ∘ βfγ))

class LawfulSequential
    [Functional program]
    [Sequential program] : Prop where
  sequential_right_identity
      (αpβ : program α β) :
    (αpβ >=> identity) =
      αpβ
  sequential_left_identity
      (αpβ : program α β) :
    (identity >=> αpβ) =
      αpβ
  sequential_associativity
      (αpβ : program α β)
      (βpγ : program β γ)
      (γpδ : program γ δ) :
    ((αpβ >=> βpγ) >=> γpδ) =
      (αpβ >=> (βpγ >=> γpδ))

def applyAtFirst
    [Functional program] :
  (α → β) → program (α × γ) (β × γ) :=
    λ αfβ => asProgram λ (α, γ) => (αfβ α, γ)

def applyAtSecond
    [Functional program] :
  (β → γ) → program (α × β) (α × γ) :=
    λ βfγ => asProgram λ (α, β) => (α, βfγ β)

def assoc
    [Functional program] :
  program ((α × β) × γ) (α × (β × γ)) :=
    asProgram λ ((a, b), c) => (a, (b, c))

class LawfulCreational
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_onlyFirst_asProgram
      (αfβ : α → β) :
    (onlyFirst (asProgram αfβ)
      : program (α × γ) (β × γ)) =
      applyAtFirst αfβ
  creational_onlyFirst_sequential
      (αpβ : program α β)
      (βpγ : program β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      program (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ)
  creational_onlyFirst_first
      (αpβ : program α β) :
    (onlyFirst αpβ >=> (first : program (β × γ) β)) =
      ((first : program (α × γ) α) >=> αpβ)
  creational_onlyFirst_applyAtSecond
      (αpβ : program α β)
      (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ) =
      (applyAtSecond γfδ >=> onlyFirst αpβ)
  creational_onlyFirst_assoc
      (αpβ : program α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc
      : program ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ)

def left
    [Functional program] :
  program γ (γ ⊕ β) :=
    asProgram .inl

def right
    [Functional program] :
  program β (γ ⊕ β) :=
    asProgram .inr

class LawfulConditional
    [Functional program]
    [Sequential program]
    [Conditional program] : Prop where
  conditional_left
      (γpα : program γ α)
      (βpα : program β α) :
    (left >=> γpα ||| βpα) =
      γpα
  conditional_right
      (γpα : program γ α)
      (βpα : program β α) :
    (right >=> γpα ||| βpα) =
      βpα

-- exercise
class InCompleteExtraLawfulCreational
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_productSeq
      (αpβ : program α β)
      (αpγ : program α γ)
      (βfδ : β → δ)
      (γfε : γ → ε) :
    (αpβ &&& αpγ >=> (asProgram βfδ <&> asProgram γfε)) =
      sorry

class ExtraLawfulCreational
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_productSeq
      (αpβ : program α β)
      (αpγ : program α γ)
      (βfδ : β → δ)
      (γfε : γ → ε) :
    (αpβ &&& αpγ >=> (asProgram βfδ <&> asProgram γfε)) =
      ((αpβ >=> asProgram βfδ) &&& (αpγ >=> asProgram γfε))

class ExtraLawfulCreationalQuestion
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_productSeq
      (αpβ : program α β)
      (αpγ : program α γ)
      (βpδ : program β δ)
      (γpε : program γ ε) :
    (αpβ &&& αpγ >=> (βpδ <&> γpε)) =
      ((αpβ >=> βpδ) &&& (αpγ >=> γpε))
--

-- exercise
class IncompleteLawfulCreationalLet
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_let_sequential
      (αpβ : program α β)
      (αaβpγ : program (α × β) γ)
      (γpδ : program γ δ) :
    ((let_ αpβ αaβpγ) >=> γpδ) =
      sorry

class LawfulCreationalLet
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_let_sequential
      (αpβ : program α β)
      (αaβpγ : program (α × β) γ)
      (γpδ : program γ δ) :
    ((let_ αpβ αaβpγ) >=> γpδ) =
      (let_ αpβ (αaβpγ >=> γpδ))
--

-- exercise
class LawfulCreationalIf
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] : Prop where
  conditional_if_sequential
      (αpb : program α Bool)
      (t_apβ : program α β)
      (f_apβ : program α β)
      (βpγ: program β γ) :
    ((if_ αpb t_apβ f_apβ) >=> βpγ) =
      ((if_ αpb (t_apβ >=> βpγ) (f_apβ >=> βpγ)))
--

structure FromComputationValuedFunction
    (computation : (Type → Type)) (α β : Type) where
  toComputationValuedFunction : α → computation β

instance [Applicative computation] :
    Functional
      (FromComputationValuedFunction computation) where
  asProgram :=
    λ αfβ => ⟨λ α => pure $ αfβ α⟩

instance [Functor computation] :
    Functorial
      (FromComputationValuedFunction computation) where
  andThenF :=
    λ ⟨αfcβ⟩ βfγ => ⟨λ α => βfγ <$> αfcβ α⟩

instance [Applicative computation] :
    Creational
      (FromComputationValuedFunction computation) where
  productSeq :=
    λ ⟨αfcβ⟩ ⟨αfcγ⟩ =>
      ⟨λ α => .mk <$> αfcβ α <*> αfcγ α⟩

instance [Monad computation] :
    Sequential
      (FromComputationValuedFunction computation) where
  andThenP :=
    λ ⟨αfcβ⟩ ⟨βfcγ⟩ =>
      ⟨λ α => αfcβ α >>= βfcγ⟩

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
  sum :=
    λ ⟨γfγα⟩ ⟨βfγα⟩ =>
      ⟨foldSum γfγα βfγα⟩

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

@[simp] theorem functional_identity
  {α : Type}
    [Applicative computation] :
    (identity :
      FromComputationValuedFunction computation α α)
      = asProgram id :=
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
      = asProgram (βfγ ∘ αfβ) :=
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
               (funext λ α =>
                 pure_bind (αfβ α) (λ β => pure $ βfγ β))
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
               (funext λ α =>
                 Eq.symm (comp_map βfγ γfδ (αfcβ α)))
    _   = (αpβ >-> (γfδ ∘ βfγ))
          := rfl

@[simp] theorem function_sequential
    (αfβ : α → β)
    (βfγ : β → γ)
    (_ : α):
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

@[simp] theorem sequential_left_identity'
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

@[simp] theorem sequential_associative'
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ)
  (γpδ : FromComputationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ) =
      (αpβ >=> (βpγ >=> γpδ)) := by
  let αfcβ : α → computation β :=
    αpβ.toComputationValuedFunction
  let βfcγ : β → computation γ :=
    βpγ.toComputationValuedFunction
  let γfcδ : γ → computation δ :=
    γpδ.toComputationValuedFunction
  let βfcδ : β → computation δ :=
    λ β => βfcγ β >>= γfcδ
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
      FromComputationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by
  calc
    (onlyFirst (asProgram αfβ))
        = onlyFirst ⟨λ α => pure $ αfβ α⟩ :=
        rfl
    _   = ((first :
           FromComputationValuedFunction
             computation (α × γ) α) >=>
            (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = (((asProgram λ (α, _) => α) :
            FromComputationValuedFunction
              computation (α × γ) α) >=>
              (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = ((⟨λ (α, _) => pure α⟩ :
            FromComputationValuedFunction
              computation (α × γ) α) >=>
              (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = (⟨λ (α, _) => pure α >>= (λ α => pure $ αfβ α)⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        rfl
    _   = (⟨(λ (α, _) => pure $ αfβ α)⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        congrArg
          (λ (αfcβ : α → computation β) =>
            ((⟨λ (α, _) => αfcβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second))
          (funext (λ α =>
            pure_bind α (λ α => pure $ αfβ α)))
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        rfl
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&&
              (asProgram (λ (_, γ) => γ) :
                FromComputationValuedFunction
                  computation (α × γ) γ) :=
        rfl
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&&
              (⟨λ (_, γ) => pure $ γ⟩ :
                FromComputationValuedFunction
                  computation (α × γ) γ) :=
        rfl
    _   = (⟨λ (α, γ) =>
            (Prod.mk <$> (pure $ αfβ α)) <*> (pure $ γ)⟩ :
             FromComputationValuedFunction
               computation (α × γ) (β × γ)) :=
        rfl
    _   = (⟨λ (α, γ) =>
            (pure $ Prod.mk (αfβ α)) <*> (pure $ γ)⟩ :
             FromComputationValuedFunction
             computation (α × γ) (β × γ)) :=
        congrArg
         (FromComputationValuedFunction.mk ∘
           ((λ αfβaγ =>
             λ (α, γ) => αfβaγ α <*> (pure $ γ)) :
             (α → computation (γ → (β × γ))) →
               ((α × γ) → computation (β × γ))))
          (funext λ α => (map_pure (Prod.mk) (αfβ α)))
    _   = (⟨λ (α, γ) => Prod.mk (αfβ α) <$> (pure $ γ)⟩) :=
        congrArg
         FromComputationValuedFunction.mk
          (funext λ (α, γ) =>
            (pure_seq (Prod.mk (αfβ α)) (pure $ γ)))
    _   = (⟨λ (α, γ) => pure (Prod.mk (αfβ α) γ)⟩) :=
        congrArg
         FromComputationValuedFunction.mk
          (funext λ (α, γ) =>
            (map_pure (Prod.mk (αfβ α)) γ))
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
      FromComputationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by simp [
        onlyFirst, asProgram, productSeq, first, second
        ]

@[simp] theorem creational_onlyFirst_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      FromComputationValuedFunction
        computation (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ :
        FromComputationValuedFunction
          computation (α × δ) (γ × δ)) := by simp[
            onlyFirst, andThenP, asProgram, productSeq,
            first, second
            ]

abbrev Active := Id

abbrev ActiveProgram :=
  FromComputationValuedFunction Active

def materializeActive :
    ActiveProgram α β → (α → β) :=
  λ ⟨αfaβ⟩ α => αfaβ α

unsafe def activeFibonacci :=
  materializeActive fibonacci

#eval activeFibonacci 10

unsafe def activeFactorial :=
  materializeActive factorial

#eval activeFactorial 10

structure ReactiveT
    (ρ : Type)
    (computation: Type → Type)
    (α : Type) where
  runReactiveT : (α → computation ρ) → computation ρ

abbrev Reactive ρ := ReactiveT ρ Active

instance {ρ: Type} :
    Functor (ReactiveT ρ computation) where
  map :=
    λ αfβ ⟨rcα⟩ =>
      ⟨λ γ => rcα (γ ∘ αfβ)⟩

instance {ρ: Type} :
    Applicative (ReactiveT ρ computation) where
  pure := λ α => ReactiveT.mk (λ αfcρ => αfcρ α)
  seq :=
    λ ⟨rcαfβ⟩ ufrtρcα =>
      ⟨λ βfcρ =>
        rcαfβ $
          (λ αfβ =>
            (ufrtρcα ()).runReactiveT (βfcρ ∘ αfβ))⟩

instance {ρ: Type} :
    Monad (ReactiveT ρ computation) where
  bind :=
    λ ⟨rcα⟩ αfrtρcβ =>
      ⟨λ βfcρ =>
        rcα (λ α =>
        (αfrtρcβ α).runReactiveT βfcρ)⟩

abbrev ReactiveProgram ρ computation :=
  FromComputationValuedFunction (ReactiveT ρ computation)

def materializeReactive {α β : Type} :
    ReactiveProgram β Active α β → α → β :=
  λ ⟨αfrtaβcβ⟩ α =>
      (αfrtaβcβ α).runReactiveT id

unsafe def reactiveFibonacci :=
  materializeReactive fibonacci

#eval reactiveFibonacci 10

unsafe def reactiveFactorial :=
  materializeReactive factorial

#eval reactiveFactorial 10

instance : Monad Task where
  pure := Task.pure
  bind := Task.bind

instance : MonadAsync Task where
  async := Task.spawn

abbrev TasksProgram :=
  FromComputationValuedFunction Task

def materializeTasks {α β : Type} :
  TasksProgram α β → (α → β) :=
    λ ⟨αftβ⟩ α => (αftβ α).get

unsafe def tasksFibonacci :=
  materializeTasks parallelFibonacci

#eval tasksFibonacci 10

#eval tasksFibonacci 24

def twoF : Nat → Nat := λ _ => 2

def threeF : Nat → Nat := λ _ => 3

def two [Functional program] :
  program Nat Nat :=
    asProgram twoF

def three [Functional program] :
  program Nat Nat :=
    asProgram threeF

def somePositionalProgramFunctorial
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusTwo &&& two &&& three >->
      λ (((n1, n2), n3), n4) =>
        n2 + n3 * n1 + n4

def somePositionalProgramSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusTwo &&& two &&& three >=>
      asProgram (λ (((n1, n2), n3), n4) =>
        n2 + n3 * n1 + n4)

def twentyNineFunctorial :=
  materializeActive somePositionalProgramFunctorial 10

#eval twentyNineFunctorial

def twentyNineSequential :=
  materializeActive somePositionalProgramSequential 10

#eval twentyNineFunctorial

class Positional
    (program : Type → Type → Type) where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ

export Positional (at_)

infixl:45 " @ " => at_

instance
    [Functional program]
    [Sequential program]
    [Creational program] :
    Positional program where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ :=
      λ αpβ σpα =>
        let_ (σpα >=> αpβ)

def positionOne
    [Functional program] :
  program (σ × α) α :=
    asProgram  λ (_, α) => α

def positionTwo
    [Functional program] :
  program ((σ × β) × α) β :=
    asProgram λ ((_, β), _) => β

def positionOneAndTwo
    [Functional program] :
  program ((σ × β) × α) (α × β) :=
    asProgram λ ((_, β), α) => (α, β)

-- ...

unsafe def positionalFactorialOfFibonacci
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program (σ × Nat) Nat :=
    fibonacci @ positionOne $
      factorial @ positionOne $
        positionOne

unsafe def positionalSumOfFibonacciAndFactorial
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program (σ × Nat) Nat :=
    fibonacci @ positionOne $
      factorial @ positionTwo $
        add @ positionOneAndTwo $
          positionOne

unsafe def factorialOfFibonacci : (Unit × Nat) → Nat :=
  materializeActive positionalFactorialOfFibonacci

#eval factorialOfFibonacci ((), 5)

unsafe def sumOfFibonacciAndFactorial : (Unit × Nat) → Nat :=
  materializeActive positionalSumOfFibonacciAndFactorial

#eval sumOfFibonacciAndFactorial ((), 5)

unsafe def positionalFactorialOfFibonacci'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program (σ × Nat) (((σ × Nat) × Nat) × Nat) :=
    fibonacci @ positionOne $
      factorial @ positionOne $
        identity

unsafe def positionalSumOfFibonacciAndFactorial'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program (σ × Nat) ((((σ × Nat) × Nat) × Nat) × Nat) :=
    fibonacci @ positionOne $
      factorial @ positionTwo $
        add @ positionOneAndTwo $
          identity

unsafe def factorialOfFibonacci' :
    (Unit × Nat) → (((Unit × Nat) × Nat) × Nat) :=
  materializeActive positionalFactorialOfFibonacci'

#eval factorialOfFibonacci' ((), 5)

unsafe def sumOfFibonacciAndFactorial' :
    (Unit × Nat) → ((((Unit × Nat) × Nat) × Nat) × Nat) :=
  materializeActive positionalSumOfFibonacciAndFactorial'

#eval sumOfFibonacciAndFactorial' ((), 5)

class WithState
    (σ : outParam Type)
    (program : Type → Type → Type) where
  readState {α : Type} : program α σ
  writeState : program σ Unit

export WithState (readState writeState)

def modifyStateWith
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  (σ → σ) → program α α :=
    λ σfσ =>
      let_ ((readState >=> asProgram σfσ) >=> writeState) $
        in_ $
          first

def withInitialStateAsInitialValue
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithState σ program] :
  program σ τ → program α τ :=
    λ σpτ =>
      readState >=> σpτ

instance [MonadStateOf σ computation] :
    WithState σ
      (FromComputationValuedFunction computation) where
  readState := ⟨λ _ => getThe σ⟩
  writeState := ⟨set⟩

abbrev ProgramWithState σ computation :=
  FromComputationValuedFunction (StateT σ computation)

def materializeWithState
    [Monad computation] {α β : Type} :
  ProgramWithState σ computation α β →
  α →
  σ →
  computation β :=
    λ ⟨αfstσcβ⟩ =>
      λ α =>
        λ σ =>
          StateT.run (αfstσcβ α) σ >>=
            λ (β, _) => pure β

def materializeActiveWithState {α β : Type} :
  ProgramWithState σ Active α β → α → σ → β :=
    materializeWithState

class LawfulWithState
    [Functional program]
    [Sequential program]
    [WithState σ program] : Prop where
  withState_write_read :
    ((writeState : program σ Unit) >=>
      (readState : program Unit σ)) =
      identity

class LawfulStateOf (σ : Type) (computation : Type → Type)
    [Monad computation]
    [MonadStateOf σ computation] : Prop where
  stateOf_write_read :
    ((λ s => set s >>= λ _ => getThe σ) :
       σ → computation σ) =
      (pure)

export LawfulStateOf (stateOf_write_read)

attribute [simp] stateOf_write_read

@[simp] theorem withState_write_read
    [Monad computation]
    [MonadStateOf σ computation]
    [LawfulStateOf σ computation] :
  ((writeState >=> readState) :
      FromComputationValuedFunction computation σ σ) =
    identity := by simp [andThenP, identity, asProgram]

unsafe def fibonacciWithState
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit Nat :=
    withInitialStateAsInitialValue fibonacci >=>
    modifyStateWith (λ σ => σ + 1)

unsafe def statefulFibonacciPair :
  Unit → Nat → (Nat × Nat) :=
    materializeActiveWithState
    (fibonacciWithState &&& fibonacciWithState)

#eval statefulFibonacciPair () 10

class WithFailure
    (ε : outParam Type)
    (program : Type → Type →Type) where
  failureWith {α β : Type} : (α → ε) → program α β

export WithFailure (failureWith)

structure FailureT
    (ε : Type)
    (computation : Type → Type)
    (β : Type) : Type where
  toComputationOfSum : computation (ε ⊕ β)

instance [Monad computation] :
    Monad (FailureT ε computation) where
  map :=
  λ αfβ ⟨cεoα⟩  =>
    ⟨cεoα >>= λ εoα => match εoα with
      | (.inr α) => pure $ .inr (αfβ α)
      | (.inl ε) => pure $ .inl ε⟩
  pure :=
    λ α =>
      .mk (pure (Sum.inr α))
  bind :=
    λ ⟨cεoα⟩ αfftεcβ =>
      ⟨cεoα >>= λ εoα => match εoα with
        | .inr α  => (αfftεcβ α).toComputationOfSum
        | .inl ε  => pure (.inl ε)⟩

instance {ε : Type}
    [Applicative computation] :
  WithFailure ε
    (FromComputationValuedFunction
      (FailureT ε computation)) where
  failureWith :=
    λ αfε =>
      ⟨λ α =>
        ⟨pure $ Sum.inl $ αfε α⟩⟩

abbrev ProgramWithFailure ε computation :=
  FromComputationValuedFunction (FailureT ε computation)

def materializeWithFailure
    [Monad computation] {α β : Type} :
  ProgramWithFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ ⟨αftεcβ⟩ α =>
      (αftεcβ α).toComputationOfSum

def materializeActiveWithFailure {α β : Type} :
 ProgramWithFailure ε Active α β → α → (ε ⊕ β) :=
  materializeWithFailure

def isNotZeroF: Nat → Bool :=
  λ n => n != 0

def unsafeDivF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n / m

def isNotZero [Functional program] :
  program Nat Bool :=
    asProgram isNotZeroF

def unsafeDiv [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram unsafeDivF

def safeDiv
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithFailure String program] :
  program (Nat × Nat) Nat :=
    if_ (second >=> isNotZero) unsafeDiv $
      else_ $
        failureWith (λ (n, m) => s!"{n}/{m}")

unsafe def failFastSafeDiv :
  (Nat × Nat) → (String ⊕ Nat) :=
    materializeActiveWithFailure safeDiv

#eval failFastSafeDiv (10, 5)

#eval failFastSafeDiv (10, 2)

#eval failFastSafeDiv (10, 0)

def twiceSafeDiv
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program ((Nat × Nat) × Nat) Nat :=
    (first >=> safeDiv) &&& second >=> safeDiv

unsafe def failFastTwiceSafeDiv :
  ((Nat × Nat) × Nat) → (String ⊕ Nat) :=
    materializeActiveWithFailure twiceSafeDiv

#eval failFastTwiceSafeDiv ((10, 5), 2)

#eval failFastTwiceSafeDiv ((10, 0), 2)

#eval failFastTwiceSafeDiv ((10, 5), 0)

class Monoid (μ : Type) where
  ν : μ
  combine : μ → μ → μ

export Monoid (ν combine)

infixl:60 " * " => combine

instance : Monoid (List α) where
  ν := []
  combine := .append

instance
    [Functor computation] :
  Functor (FailureT ε computation) where
    map :=
     λ αfβ ⟨cεoα⟩ =>
       ⟨(λ εoα =>
           match εoα with
            | .inl ε => .inl ε
            | .inr α => .inr (αfβ α)) <$> cεoα
       ⟩

instance
    [Applicative computation]
    [Monoid ε] :
  Applicative (FailureT ε computation) where
    pure :=
      λ α =>
        ⟨pure $ .inr α⟩
    seq :=
      λ ⟨cεoαfβ⟩ ufftεcα =>
        let cεoα :=
          (ufftεcα ()).toComputationOfSum
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
        ⟨εoαfεoαfβfεoβ <$> cεoα <*> cεoαfβ⟩

abbrev ProgramWithValidation ε computation :=
  FromComputationValuedFunction (FailureT ε computation)

def materializeWithValidation
    [Monad computation]
    [Monoid ε] {α β : Type} :
  ProgramWithValidation ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ ⟨αftεcβ⟩ α =>
      (αftεcβ α).toComputationOfSum

def materializeActiveWithValidation
    [Monoid ε] {α β : Type} :
 ProgramWithValidation ε Active α β → α → (ε ⊕ β) :=
  materializeWithValidation

def accumulatingSafeDiv
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program (Nat × Nat) Nat :=
    if_ (second >=> isNotZero) unsafeDiv $
      else_ $
        failureWith (λ (n, m) =>
          [s!"{n}/{m}"])

def accumulatingSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) (Nat × Nat) :=
    (first >=> accumulatingSafeDiv) &&& (second >=>
    accumulatingSafeDiv)

unsafe def validatingSafeDivProduct :
  ((Nat × Nat) × (Nat × Nat)) → (List String ⊕ (Nat × Nat)) :=
    materializeActiveWithValidation accumulatingSafeDivProduct

#eval validatingSafeDivProduct ((10, 5), (8, 2))

#eval validatingSafeDivProduct ((10, 0), (8, 2))

#eval validatingSafeDivProduct ((10, 5), (8, 0))

#eval validatingSafeDivProduct ((10, 0), (8, 0))

def addAccumulatingSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    accumulatingSafeDivProduct >=>
    add

unsafe def validatingAddSafeDivProduct :
  ((Nat × Nat) × (Nat × Nat)) → (List String ⊕ Nat) :=
    materializeActiveWithValidation addAccumulatingSafeDivProduct

#eval validatingAddSafeDivProduct ((10, 5), (8, 2))

#eval validatingAddSafeDivProduct ((10, 0), (8, 2))

#eval validatingAddSafeDivProduct ((10, 5), (8, 0))

#eval validatingAddSafeDivProduct ((10, 0), (8, 0))

#eval "---end---"

end All
