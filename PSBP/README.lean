namespace README

#eval "--begin--"

abbrev function α β := α → β

class Functional
    (program : Type → Type → Type) where
  asProgram {α β : Type} :
    function α β → program α β

export Functional (asProgram)

-- exercise

def identity
    [Functional program] :
  program α α :=
    asProgram λ α => α

--

class Functorial
    (program : Type → Type → Type) where
  functionAction {α β γ : Type} :
    function β γ → (program α β → program α γ)

export Functorial (functionAction)

-- exercise

def andThenFunction {α β γ : Type}
    [Functorial program] :
  program α β → function β γ → program α γ :=
    λ αpβ βfγ => functionAction βfγ αpβ

infixl:50 " >-> " => andThenFunction

--

class Sequential
    (program : Type → Type → Type) where
  andThenProgram {α β γ : Type} :
    program α β → program β γ → program α γ

export Sequential (andThenProgram)

macro_rules
  | `($_ >=> $_) => Lean.Macro.throwError "disabled"

infixl:50 " >=> " => andThenProgram


-- exercise

def programAction
    [Sequential program] :
  program β γ → (program α β → program α γ) :=
    λ βpγ αpβ => αpβ >=> βpγ

--

-- exercise

section FunctorialInstanceExercise

end FunctorialInstanceExercise

instance
    [Functional program]
    [Sequential program] :
  Functorial program where
    functionAction {α β γ: Type} :
      function β γ → (program α β → program α γ) :=
        λ βfγ αpβ => αpβ >=> asProgram βfγ

--

class Creational
    (program : Type → Type → Type) where
  sequentialProduct {α β γ : Type} :
    program α β → program α γ → program α (β × γ)

export Creational (sequentialProduct)

infixl:60 " &&& " => sequentialProduct


-- exercise

def first
    [Functional program] :
  program (α × β) α :=
    asProgram λ (α, _) => α

def second
    [Functional program] :
  program (α × β) β :=
    asProgram λ (_, β) => β

def bothSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      (first >=> αpγ) &&& second >=>
        first &&& (second >=> βpδ)

infixl:60 " <&> " => bothSequential

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

def minusOneF : Nat → Nat := λ n => n - 1

def addF : Nat × Nat → Nat := λ ⟨n, m⟩ => n + m

def minusOne
    [Functional program] :
  program Nat Nat :=
    asProgram minusOneF

def add
    [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram addF

def minusOneTimesTwoFunctorial
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >-> addF

def minusOneTimesTwoSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >=> add

def let_
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program (α × β) γ → program α γ :=
    λ αpβ αaβpγ => identity &&& αpβ >=> αaβpγ

def creational_let {α β γ : Type} :
    function α β → function (α × β) γ → function α γ :=
  λ αfβ αaβfγ α =>
    let β := αfβ α
    αaβfγ (α, β)

def sequential_let {α β γ : Type} :
    function α β → function β γ → function α γ :=
  λ αfβ βfγ α =>
    let β := αfβ α
    βfγ β

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

class Conditional
    (program : Type → Type → Type) where
  sum {α β γ : Type} :
    program γ α → program β α → program (γ ⊕ β) α

export Conditional (sum)

infixl:55 " ||| " => sum

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
        asProgram (
          λ αab => match αab with
            | ⟨α, true⟩ => .inl α
            | ⟨α, false⟩ => .inr α
        ) >=>
        t_apβ ||| f_apβ

def conditional_if :
  function α Bool →
  function α β →
  function α β →
  function α β :=
    λ αfb t_afβ f_afβ α =>
      if αfb α
        then t_afβ α
        else f_afβ α

def sum'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program γ α → program β α → program (γ ⊕ β) α :=
    λ γpα βpα =>
      if_ (asProgram
            (λ γoβ => match γoβ with
              | .inl _ => True
              | .inr _ => False))
        (asProgram
          (λ γoβ => match γoβ with
            | .inl γ => γ
            | .inr β => sorry) >=> γpα) $
        asProgram
          (λ γoβ => match γoβ with
            | .inl _ => sorry
            | .inr β => β) >=> βpα

class Positional
    (program : Type → Type → Type) where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ

export Positional (at_)

infixl:45 " @ " => at_

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

def minusTwoPositional
    [Functional program]
    [Sequential program]
    [Creational program] :
  program (σ × Nat) Nat :=
    minusOne @ positionOne $
      minusOne @ positionOne $
        positionOne

def minusOneTimesTwoPositional
    [Functional program]
    [Sequential program]
    [Creational program] :
  program (σ × Nat) Nat :=
    minusOne @ positionOne $
      minusOne @ positionTwo $
        add @ positionOneAndTwo $
          positionOne

def stackShowingMinusTwoPositional
    [Functional program]
    [Creational program]
    [Sequential program] :
  program (σ × Nat) (((σ × Nat) × Nat) × Nat) :=
    minusOne @ positionOne $
      minusOne @ positionOne $
        identity

def stackShowingTwiceMinusOnePositional
    [Functional program]
    [Creational program]
    [Sequential program] :
  program (σ × Nat) ((((σ × Nat) × Nat) × Nat) × Nat) :=
    minusOne @ positionOne $
      minusOne @ positionTwo $
        add @ positionOneAndTwo $
          identity

-- exercise

def isZeroF: Nat → Bool := (. == 0)

def isOneF : Nat → Bool := (. == 1)

def oneF : Nat → Nat := λ _ => 1

def minusTwoF : Nat → Nat := λ n => n - 2

def multiplyF : Nat × Nat → Nat := λ ⟨n, m⟩ => n * m

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

def minusTwo
    [Functional program] :
  program Nat Nat :=
    asProgram minusTwoF

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
      if_ isOne one $
        (minusOne >=> fibonacci) &&&
        (minusTwo >=> fibonacci) >=>
        add

unsafe def factorial
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      (identity) &&&
      (minusOne >=> factorial) >=>
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

class Parallel (program : Type → Type → Type) where
  bothParallel {α β γ δ : Type} :
  program α γ → program β δ → program (α × β) (γ × δ)

export Parallel (bothParallel)

infixl:60 " |&| " => bothParallel

def duplicate
    [Functional program] :
  program α (α × α) :=
    asProgram λ α => (α, α)

def parallelProduct {α β γ : Type}
    [Functional program]
    [Sequential program]
    [Parallel program] :
  program α β → program α γ → program α (β × γ) :=
   λ αpβ αpγ => duplicate >=> αpβ |&| αpγ

infixl:60 " &|& " => parallelProduct

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

unsafe def fibonacci'
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
      let_ (minusOne >=> factorial'')
        multiply
    )

class WithState
    (σ : outParam Type)
    (program : Type → Type → Type) where
  readState {α : Type} : program α σ
  writeState : program σ Unit

export WithState (readState writeState)

def modifyStateWithFunction
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  function σ σ → program α α :=
    λ σfσ =>
      let_ ((readState >=> asProgram σfσ) >=> writeState) $
        first

-- exercise

def modifyStateWithProgram
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  program σ σ → program α α :=
    λ σpσ =>
      let_ ((readState >=> σpσ) >=> writeState) $
        first

--

def usingInitialStateAsInitialValue
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithState σ program] :
  program σ β → program α β :=
    λ σpβ =>
      readState >=> σpβ

unsafe def fibonacciWithState
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit Nat :=
    usingInitialStateAsInitialValue fibonacci >=>
    modifyStateWithFunction (λ σ => σ + 1)

unsafe def fibonacciWithStatePair
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit (Nat × Nat) :=
    fibonacciWithState &&& fibonacciWithState

class WithFailure
    (ε : outParam Type)
    (program : Type → Type →Type) where
  failWithFunction {α β : Type} :
    function α ε → program α β

export WithFailure (failWithFunction)

class Monoid (μ : Type) where
  ν : μ
  combine : μ → μ → μ

export Monoid (ν combine)

infixl:60 " * " => combine

def unsafeDiv [Functional program] :
  program (Nat × Nat) Nat := asProgram λ ⟨n, m⟩ => n / m

def isNotZero [Functional program] :
  program Nat Bool := asProgram (. != 0)

def divWithFailureUsing
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithFailure ε program] :
  function (Nat × Nat) ε →
    program (Nat × Nat) Nat :=
    λ nfε =>
      if_ (second >=> isNotZero) unsafeDiv $
        failWithFunction nfε

def divPairWithFailureUsing
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithFailure ε program] :
  function (Nat × Nat) ε →
    program ((Nat × Nat) × (Nat × Nat)) (Nat × Nat) :=
    λ nfε =>
      let safeDiv := divWithFailureUsing nfε
      (first >=> safeDiv) &&& (second >=>
      safeDiv)

def addedDivPairWithFailureUsing
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithFailure ε program] :
  function (Nat × Nat) ε →
    program ((Nat × Nat) × (Nat × Nat)) Nat :=
    λ nfε =>
      divPairWithFailureUsing nfε >=>
        add

def addedDivPairWithStringFailure
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    addedDivPairWithFailureUsing
      λ (n, m) => s!"{n}/{m}"

def addedDivPairWithStringListFailure
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    addedDivPairWithFailureUsing
      λ (n, m) => [s!"{n}/{m}"]

instance : Monoid (List α) where
  ν := .nil
  combine := .append

class LawfulFunctional
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program] : Prop where
  functional_identity :
    (asProgram id : program α α) =
      identity
  functional_sequential
      (αfβ : function α β)
      (βfγ : function β γ) :
    (asProgram αfβ >=> asProgram βfγ : program α γ) =
      asProgram (βfγ ∘ αfβ)

class LawfulFunctorial
    (program : Type → Type → Type)
    [Functorial program] : Prop where
  functorial_identity
    (αpβ : program α β) :
    (αpβ >-> id) =
      αpβ
  functorial_sequential
      (αpβ : program α β)
      (βfγ : function β γ)
      (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ) =
      (αpβ >-> (γfδ ∘ βfγ))

def applyAtFirst
    [Functional program] :
  function α β → program (α × γ) (β × γ) :=
    λ αfβ => asProgram λ (α, γ) => (αfβ α, γ)

def applyAtSecond
    [Functional program] :
  function β γ → program (α × β) (α × γ) :=
    λ βfγ => asProgram λ (α, β) => (α, βfγ β)

def assoc
    [Functional program] :
  program ((α × β) × γ) (α × (β × γ)) :=
    asProgram λ ((a, b), c) => (a, (b, c))

class LawfulCreational
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_onlyFirst_asProgram
      (αfβ : function α β) :
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
    (program : Type → Type → Type)
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

class AlternativeLawfulFunctorial
    (program : Type → Type → Type)
    [Functorial program] : Prop where
  functorial_sequential
      (βfγ : function β γ)
      (γfδ : γ → δ) :
    (((. >-> γfδ) ∘ (. >-> βfγ)) :
      program α β → program α δ) =
     (. >-> (γfδ ∘ βfγ))

--

-- exercise

class ExtraLawfulCreationalQuestion
    (program : Type → Type → Type)
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

class LawfulWithState
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [WithState σ program] : Prop where
  withState_write_read :
    ((writeState : program σ Unit) >=> readState) =
      identity

abbrev computationValuedFunction
    (computation : Type → Type) (α β : Type) :=
  function α (computation β)

instance
    [Applicative computation] :
  Functional
    (computationValuedFunction computation) where
    asProgram :=
      λ αfβ α => pure $ αfβ α

instance
    [Functor computation] :
  Functorial
    (computationValuedFunction computation) where
    functionAction :=
      λ βfγ =>
        λ αfcβ α => βfγ <$> αfcβ α

instance
    [Monad computation] :
  Sequential
    (computationValuedFunction computation) where
    andThenProgram :=
      λ αfcβ βfcγ α =>
        αfcβ α >>= βfcγ

instance
    [Applicative computation] :
  Creational
    (computationValuedFunction computation) where
    sequentialProduct :=
      λ αfcβ αfcγ α =>
        .mk <$> αfcβ α <*> αfcγ α

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

instance
    [MonadStateOf σ computation] :
  WithState σ
    (computationValuedFunction computation) where
    readState := λ _ => getThe σ
    writeState := set

abbrev FailureT
    (ε : Type)
    (computation : Type → Type)
    (β : Type) : Type :=
  computation (ε ⊕ β)

instance
    [Monad computation] :
  Monad (FailureT ε computation) where
    map {α β : Type} :
      (function α β) →
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
      (function α β) →
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
      (computation (ε ⊕ (function α β))) →
      ((Unit → computation (ε ⊕ α)) →
      computation (ε ⊕ β)) :=
      λ cεoαfβ ufftεcα =>
        let cεoα :=
          (ufftεcα ())
        let εoαfεoαfβfεoβ {α β : Type} :
          (ε ⊕ α) → (ε ⊕ (function α β)) → (ε ⊕ β) :=
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

instance
    [Applicative computation] :
  WithFailure ε
    (computationValuedFunction
      (FailureT ε computation)) where
    failWithFunction {α β : Type} :
      function α ε →
      computationValuedFunction computation α (ε ⊕ β) :=
      λ αfε α =>
        pure $ .inl $ αfε α

@[simp] theorem functional_identity
  {α : Type}
    [Applicative computation] :
    (identity :
      computationValuedFunction computation α α)
      = asProgram id :=
  calc
    identity
        = asProgram id
          := rfl

theorem functional_sequential'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : function α β)
  (βfγ : function β γ) :
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) :=
  calc
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
    _   = λ α => (pure $ αfβ α) >>= λ β => pure $ βfγ β
          := rfl
    _   = λ α => pure $ βfγ (αfβ α)
          := funext λ α =>
               pure_bind (αfβ α) (λ β => pure $ βfγ β)
    _   = (asProgram (βfγ ∘ αfβ) :
            computationValuedFunction computation α γ)
          := rfl

@[simp] theorem functional_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : function α β)
  (βfγ : function β γ) :
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) := by
  simp[asProgram, andThenProgram]

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

theorem sequential_right_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >=> asProgram id)
      = αpβ :=
    calc
      (αpβ >=> asProgram id)
          = λ α => αpβ α >>= λ β => pure (id β)
            := rfl
      _   = λ α => id <$> αpβ α
            := funext λ α => bind_pure_comp id (αpβ α)
      _   = λ α => αpβ α
            := funext λ α => id_map (αpβ α)
      _   = αpβ
            := rfl

@[simp] theorem sequential_right_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >=> asProgram id)
      = αpβ := by
  simp[andThenProgram, asProgram]

theorem sequential_left_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (asProgram id >=> αpβ)
      = αpβ :=
  calc
    (asProgram id >=> αpβ)
        = λ α => pure α >>= αpβ
          := rfl
    _   = λ α => αpβ α
          := (funext λ α => pure_bind α αpβ)
    _   = αpβ
          := rfl

theorem sequential_left_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (asProgram id >=> αpβ)
    = αpβ := by
  simp[asProgram, andThenProgram]

@[simp] theorem sequential_associative'
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ)
  (γpδ : computationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ
       : computationValuedFunction computation α δ) =
    (αpβ >=> (βpγ >=> γpδ)) :=
  calc
    ((αpβ >=> βpγ) >=> γpδ
       : computationValuedFunction computation α δ)
       = ((λ α => αpβ α >>= βpγ) >=> γpδ)
         := rfl
    _   = λ α => αpβ α >>= (λ β => βpγ β >>= γpδ)
          := funext λ α => bind_assoc (αpβ α) βpγ γpδ
    _   = (λ α => αpβ α >>= (βpγ >=> γpδ))
          := rfl
    _   = (αpβ >=> (βpγ >=> γpδ))
          := rfl

@[simp] theorem sequential_associative
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ)
  (γpδ : computationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ) =
      (αpβ >=> (βpγ >=> γpδ)) := by
  simp[andThenProgram]

@[simp] theorem creational_onlyFirst_asProgram
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : function α β) :
    (onlyFirst (asProgram αfβ) :
      computationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by
    simp [ onlyFirst, first, andThenProgram, asProgram,
    sequentialProduct, second]

@[simp] theorem creational_onlyFirst_sequential
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      computationValuedFunction
        computation (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ :
        computationValuedFunction
          computation (α × δ) (γ × δ)) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second]

@[simp] theorem creational_onlyFirst_first
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (onlyFirst αpβ >=> first :
      computationValuedFunction computation (α × γ) β) =
      (first >=> αpβ) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second]

@[simp] theorem creational_onlyFirst_applyAtSecond
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ) =
      (applyAtSecond γfδ >=> onlyFirst αpβ) := by
  simp[onlyFirst, andThenProgram, applyAtSecond, asProgram,
  sequentialProduct, first, second]

@[simp] theorem creational_onlyFirst_assoc
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc :
      computationValuedFunction
        computation ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second, assoc]

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

class LawfulStateOf
    (σ : Type)
    (computation : Type → Type)
    [Monad computation]
    [MonadStateOf σ computation] : Prop where
  set_getThe :
    ((λ s => set s >>= λ _ => getThe σ) :
       σ → computation σ) =
      pure

export LawfulStateOf (set_getThe)

attribute [simp] set_getThe

@[simp] theorem withState_write_read
    [Monad computation]
    [MonadStateOf σ (StateT σ computation)]
    [LawfulStateOf σ (StateT σ computation)] :
  ((writeState >=> (readState
     : computationValuedFunction
         (StateT σ computation) Unit σ)) ) =
    identity := by
  simp [writeState, andThenProgram, readState, asProgram]

abbrev Sync := Id

abbrev syncProgram :=
  computationValuedFunction Sync

def materializeSync :
  syncProgram α β → function α β :=
    λ αspβ α => αspβ α

@[simp] theorem sync_id_map
  {α : Type}
  (sα : Sync α) :
    id <$> sα =
      sα := rfl

@[simp] theorem sync_comp_map
  {α β γ : Type}
  (sα : Sync α)
  (αfβ : function α β)
  (βfγ : function β γ) :
    (βfγ ∘ αfβ) <$> sα =
      βfγ <$> αfβ <$> sα := rfl

@[simp] theorem sync_pure_seq
  {α β : Type}
  (sα : Sync α)
  (αfβ : function α β) :
    pure αfβ <*> sα = αfβ <$> sα := rfl

@[simp] theorem sync_map_pure
  {α β : Type}
  (a : α)
  (αfβ : function α β) :
    αfβ <$> (pure a : Sync α) = pure (αfβ a) := rfl

@[simp] theorem sync_seq_pure
  {α β : Type}
  (a : α)
  (sαfβ : Sync (function α β)) :
    sαfβ <*> pure a = (λ αfβ => αfβ a) <$> sαfβ := rfl

@[simp] theorem sync_seq_assoc
  {α β γ : Type}
  (sα : Sync α)
  (sαfβ : Sync (function α β))
  (sβfγ : Sync (function β γ)) :
sβfγ <*> (sαfβ <*> sα) =
  (Function.comp <$> sβfγ) <*> sαfβ <*> sα := rfl

@[simp] theorem sync_pure_bind
  {α β : Type}
  (a : α)
  (αspβ : syncProgram α β) :
    pure a >>= αspβ = αspβ a := rfl

@[simp] theorem sync_bind_map
  {α β : Type}
  (sα : Sync α)
  (sαfβ : Sync (function α β)) :
    (sαfβ >>= λ β => β <$> sα) = sαfβ <*> sα := rfl

@[simp] theorem sync_pure_comp
  {α β : Type}
  (sα : Sync α)
  (αfβ : function α β) :
    sα >>= (λ α => pure (αfβ α)) = αfβ <$> sα := rfl

@[simp] theorem sync_bind_assoc
  {α β γ : Type}
  (sα : Sync α)
  (αspβ : syncProgram α β)
  (βspγ : syncProgram β γ) :
  sα >>= αspβ >>= βspγ =
    sα >>= λ α => αspβ α >>= βspγ := rfl

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

abbrev syncReactiveProgram ρ :=
  ReactiveProgram ρ Sync

def materializeSyncReactive {α β : Type} :
  (syncReactiveProgram β) α β → function α β :=
    λ αsrpβ α =>
       materializeSync (αsrpβ α) id

abbrev SyncReactive ρ := ReactiveT ρ Sync

@[simp] theorem syncReactive_id_map
  {α : Type}
  (srα : SyncReactive ρ α) :
    id <$> srα =
      srα := rfl

@[simp] theorem syncReactive_comp_map
  {α β γ : Type}
  (srα : SyncReactive ρ α)
  (αfβ : function α β)
  (βfγ : function β γ) :
    (βfγ ∘ αfβ) <$> srα =
      βfγ <$> αfβ <$> srα := rfl

@[simp] theorem syncReactive_pure_seq
  {α β : Type}
  (srα : SyncReactive ρ α)
  (αfβ : function α β) :
    pure αfβ <*> srα = αfβ <$> srα := rfl

@[simp] theorem syncReactive_map_pure
  {α β : Type}
  (a : α)
  (αfβ : function α β) :
    αfβ <$> (pure a : SyncReactive ρ α) =
      pure (αfβ a) := rfl

@[simp] theorem syncReactive_seq_pure
  {α β : Type}
  (a : α)
  (srαfβ : SyncReactive ρ (function α β)) :
    srαfβ <*> pure a = (λ αfβ => αfβ a) <$> srαfβ := rfl

@[simp] theorem syncReactive_seq_assoc
  {α β γ : Type}
  (srα : SyncReactive ρ α)
  (srαfβ : SyncReactive ρ (function α β))
  (srβfγ : SyncReactive ρ (function β γ)) :
srβfγ <*> (srαfβ <*> srα) =
  (Function.comp <$> srβfγ) <*> srαfβ <*> srα := rfl

@[simp] theorem syncReactive_pure_bind
  {α β : Type}
  (a : α)
  (αfsrβ : function α (SyncReactive ρ β)) :
    pure a >>= αfsrβ = αfsrβ a := rfl

@[simp] theorem syncReactive_bind_map
  {α β : Type}
  (srα : SyncReactive ρ α)
  (srαfβ : SyncReactive ρ (function α β)) :
    (srαfβ >>= λ β => β <$> srα) = srαfβ <*> srα := rfl

@[simp] theorem syncReactive_pure_comp
  {α β : Type}
  (srα : SyncReactive ρ α)
  (αfβ : function α β) :
    srα >>= (λ α => pure (αfβ α)) = αfβ <$> srα := rfl

@[simp] theorem syncReactive_bind_assoc
  {α β γ : Type}
  (srα : SyncReactive ρ α)
  (αfsrβ : function α (SyncReactive ρ β))
  (βfsrγ : β → SyncReactive ρ γ) :
  srα >>= αfsrβ >>= βfsrγ =
    srα >>= λ α => αfsrβ α >>= βfsrγ := rfl

abbrev Async := Task

instance : Monad Async where
  pure := Task.pure
  bind := Task.bind

instance : MonadAsync Async where
  async := Task.spawn

abbrev asyncProgram :=
  computationValuedFunction Async

def materializeAsync {α β : Type} :
  asyncProgram α β → function α β :=
    λ αaspβ α =>
      (materializeSync αaspβ α).get

abbrev programWithState σ computation :=
  computationValuedFunction (StateT σ computation)

def materializeWithState
    [Monad computation] {α β : Type} :
  programWithState σ computation α β →
    function α (function σ (computation β)) :=
    λ αpwscβ α σ =>
      αpwscβ α σ >>=
        λ (β, _) => pure β

abbrev syncProgramWithState σ :=
  programWithState σ Sync

def materializeSyncWithState {α β : Type} :
  syncProgramWithState σ α β → function α (function σ β) :=
    materializeWithState

abbrev programWithFirstFailure ε computation :=
  computationValuedFunction (FailureT ε computation)

def materializeWithFirstFailure
    [Monad computation] {α β : Type} :
  programWithFirstFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ αpwffcβ α =>
      αpwffcβ α

abbrev syncProgramWithFirstFailure ε :=
  programWithFirstFailure ε Sync

def materializeSyncWithFirstFailure {α β : Type} :
 syncProgramWithFirstFailure ε α β → function α (ε ⊕ β) :=
  materializeWithFirstFailure

abbrev programWithAccumulatedFailure ε computation :=
  computationValuedFunction (FailureT ε computation)

def materializeWithAccumulatedFailure
    [Monad computation]
    [Monoid ε] {α β : Type} :
  programWithAccumulatedFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ αpwafcβ α =>
      αpwafcβ α

abbrev syncProgramWithAccumulatedFailure ε :=
  programWithAccumulatedFailure ε Sync

def materializeSyncWithAccumulatedFailure
    [Monoid ε]
    {α β : Type} :
  syncProgramWithAccumulatedFailure ε α β →
    function α (ε ⊕ β) :=
    materializeWithAccumulatedFailure

#eval materializeSync
  minusOneTimesTwoFunctorial 10

#eval materializeSync
  minusOneTimesTwoSequential 10

#eval materializeSync
  minusTwoPositional ((), 10)

#eval materializeSync
  minusOneTimesTwoPositional ((), 10)

#eval materializeSync
  stackShowingMinusTwoPositional ((), 10)

#eval materializeSync
  stackShowingTwiceMinusOnePositional ((), 10)

#eval materializeSync
  fibonacci 10

#eval materializeSync
  factorial 10

#eval materializeAsync
  parallelFibonacci 10

#eval materializeSyncWithState
  (fibonacciWithState &&& fibonacciWithState) () 10

#eval materializeSyncWithFirstFailure
  addedDivPairWithStringFailure ((10, 5), (8, 2))

#eval materializeSyncWithFirstFailure
  addedDivPairWithStringFailure ((10, 0), (8, 2))

#eval materializeSyncWithFirstFailure
  addedDivPairWithStringFailure ((10, 5), (8, 0))

#eval materializeSyncWithFirstFailure
  addedDivPairWithStringFailure ((10, 0), (8, 0))

#eval materializeSyncWithFirstFailure
  addedDivPairWithStringListFailure ((10, 5), (8, 2))

#eval materializeSyncWithFirstFailure
  addedDivPairWithStringListFailure ((10, 0), (8, 2))

#eval materializeSyncWithFirstFailure
  addedDivPairWithStringListFailure ((10, 5), (8, 0))

#eval materializeSyncWithFirstFailure
  addedDivPairWithStringListFailure ((10, 0), (8, 0))

#eval materializeSyncReactive
  fibonacci 10

#eval materializeSyncReactive
  factorial 10

#eval "-- end --"

end README
