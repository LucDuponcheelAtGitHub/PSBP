namespace ITP

macro_rules
  | `($_ >=> $_) => Lean.Macro.throwError "disabled"

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

def functionAction {α β γ : Type}
    [Functorial program] :
  (β → γ) → (program α β → program α γ) :=
    λ βfγ => (. >-> βfγ)

def programAction
    [Sequential program] :
  program β γ → (program α β → program α γ) :=
    λ βpγ => (. >=> βpγ)

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

def αfβ :
  (α → Bool) →
  (α → β) →
  (α → β) →
  (α → β) :=
    λ αfb t_afβ f_afβ α =>
      if αfb α
        then t_afβ α
        else f_afβ α

-- exercise
def isZeroF: Nat → Bool := (. == 0)

def isOneF : Nat → Bool := (. == 1)

def oneF : Nat → Nat := λ _ => 1

def minusOneF : Nat → Nat := λ n => n - 1

def minusTwoF : Nat → Nat := λ n => n - 2

def addF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n + m

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
      let_ (minusOne >=> factorial) $
        multiply

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

def first
    [Functional program] :
  program (α × β) α :=
    asProgram λ (α, _) => α

def second
    [Functional program] :
  program (α × β) β :=
    asProgram λ (_, β) => β

instance
    [Functional program]
    [Sequential program] :
  Functorial program where
    andThenF {α β γ: Type} :
      program α β → (β → γ) → program α γ :=
        λ αpβ βfγ => αpβ >=> asProgram βfγ

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

abbrev computationValuedFunction
    (computation : (Type → Type)) (α β : Type) :=
  α → computation β

instance [Applicative computation] :
    Functional
      (computationValuedFunction computation) where
  asProgram :=
    λ αfβ => λ α => pure $ αfβ α

instance [Functor computation] :
    Functorial
      (computationValuedFunction computation) where
  andThenF :=
    λ αfcβ βfγ => λ α => βfγ <$> αfcβ α

instance [Applicative computation] :
    Creational
      (computationValuedFunction computation) where
  productSeq :=
    λ αfcβ αfcγ =>
      λ α => .mk <$> αfcβ α <*> αfcγ α

instance [Monad computation] :
    Sequential
      (computationValuedFunction computation) where
  andThenP :=
    λ αfcβ βfcγ =>
      λ α => αfcβ α >>= βfcγ

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
  Parallel (computationValuedFunction computation) where
    bothPar := λ αfcγ βfcδ =>
      λ ⟨α, β⟩ =>
        async (λ (_: Unit) => αfcγ α) >>=
          λ cγ => async (λ (_: Unit) => βfcδ β) >>=
            λ cδ => .mk <$> cγ <*> cδ

abbrev Active := Id

abbrev ActiveProgram :=
  computationValuedFunction Active

def materializeActive :
    ActiveProgram α β → (α → β) :=
  λ αfaβ α => αfaβ α

unsafe def activeFibonacci :=
  materializeActive fibonacci

unsafe def activeFactorial :=
  materializeActive factorial

abbrev ReactiveT
    (ρ : Type)
    (computation: Type → Type)
    (α : Type) :=
  (α → computation ρ) → computation ρ

instance {ρ: Type} :
    Functor (ReactiveT ρ computation) where
  map :=
    λ αfβ rcα =>
      λ γ => rcα (γ ∘ αfβ)

instance {ρ: Type} :
    Applicative (ReactiveT ρ computation) where
  pure := λ α => λ αfcρ => αfcρ α
  seq :=
    λ rcαfβ ufrtρcα =>
      λ βfcρ =>
        rcαfβ $
          (λ αfβ =>
            (ufrtρcα ()) (βfcρ ∘ αfβ))

instance {ρ: Type} :
    Monad (ReactiveT ρ computation) where
  bind :=
    λ rcα αfrtρcβ =>
      λ βfcρ =>
        rcα (λ α =>
        (αfrtρcβ α) βfcρ)

abbrev ReactiveProgram ρ computation :=
  computationValuedFunction (ReactiveT ρ computation)

def materializeReactive {α β : Type} :
    ReactiveProgram β Active α β → α → β :=
  λ αfrtaβcβ α =>
      (αfrtaβcβ α) id

abbrev Reactive ρ := ReactiveT ρ Active

unsafe def reactiveFibonacci :=
  materializeReactive fibonacci

unsafe def reactiveFactorial :=
  materializeReactive factorial

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

def usingInitialStateAsInitialValue
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithState σ program] :
  program σ β → program α β :=
    λ σpβ =>
      readState >=> σpβ

instance [MonadStateOf σ computation] :
    WithState σ
      (computationValuedFunction computation) where
  readState := λ _ => getThe σ
  writeState := set

abbrev ProgramWithState σ computation :=
  computationValuedFunction (StateT σ computation)

def materializeWithState
    [Monad computation] {α β : Type} :
  ProgramWithState σ computation α β →
  (α → σ → computation β) :=
    λ αfstσcβ =>
      λ α =>
        λ σ =>
          StateT.run (αfstσcβ α) σ >>=
            λ (β, _) => pure β

def materializeActiveWithState {α β : Type} :
  ProgramWithState σ Active α β → (α → σ → β) :=
    materializeWithState

unsafe def statefulFibonacci
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit Nat :=
    usingInitialStateAsInitialValue fibonacci >=>
    modifyStateWith (λ σ => σ + 1)

unsafe def statefulFibonacciPair :
  Unit → Nat → (Nat × Nat) :=
    materializeActiveWithState
    (statefulFibonacci &&& statefulFibonacci)

class WithFailure
    (ε : outParam Type)
    (program : Type → Type →Type) where
  failWith {α β : Type} : (α → ε) → program α β

export WithFailure (failWith)

def FailureT
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
          | Sum.inr α  => αfftεcβ α
          | Sum.inl ε  => pure $ .inl ε

instance {ε : Type}
    [Applicative computation] :
  WithFailure ε
    (computationValuedFunction
      (FailureT ε computation)) where
  failWith {α β : Type} :
    (α → ε) →
    computationValuedFunction computation α (ε ⊕ β) :=
    λ αfε =>
      λ α =>
        pure $ Sum.inl $ αfε α

abbrev ProgramWithFailure ε computation :=
  computationValuedFunction (FailureT ε computation)

def materializeWithFailure
    [Monad computation] {α β : Type} :
  ProgramWithFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ αftεcβ α =>
      αftεcβ α

def materializeActiveWithFailure {α β : Type} :
 ProgramWithFailure ε Active α β → α → (ε ⊕ β) :=
  materializeWithFailure

def isNotZeroF: Nat → Bool := (. != 0)

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
        failWith (λ (n, m) => s!"{n}/{m}")

def fastFailingSafeDiv
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program (Nat × Nat) Nat :=
    if_ (second >=> isNotZero) unsafeDiv $
      else_ $
        failWith (λ (n, m) =>
          s!"{n}/{m}")

def fastFailingSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program ((Nat × Nat) × (Nat × Nat)) (Nat × Nat) :=
    (first >=> fastFailingSafeDiv) &&& (second >=>
    fastFailingSafeDiv)

def addFastFailingSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    fastFailingSafeDivProduct >=>
    add

def fastFailingAddSafeDivProduct :
  ((Nat × Nat) × (Nat × Nat)) → (String ⊕ Nat) :=
    materializeActiveWithFailure addFastFailingSafeDivProduct

class Monoid (μ : Type) where
  ν : μ
  combine : μ → μ → μ

export Monoid (ν combine)

infixl:60 " * " => combine

instance : Monoid (List α) where
  ν := .nil
  combine := .append

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

abbrev ProgramWithValidation ε computation :=
  computationValuedFunction (FailureT ε computation)

def materializeWithValidation
    [Monad computation]
    [Monoid ε] {α β : Type} :
  ProgramWithValidation ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ αftεcβ α =>
      αftεcβ α

def materializeActiveWithValidation
    [Monoid ε]
    {α β : Type} :
 ProgramWithValidation ε Active α β → α → (ε ⊕ β) :=
  materializeWithValidation

def combiningSafeDiv
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program (Nat × Nat) Nat :=
    if_ (second >=> isNotZero) unsafeDiv $
      else_ $
        failWith (λ (n, m) =>
          [s!"{n}/{m}"])

def combiningSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) (Nat × Nat) :=
    (first >=> combiningSafeDiv) &&& (second >=>
    combiningSafeDiv)

unsafe def accumulatingSafeDivProduct :
  ((Nat × Nat) × (Nat × Nat)) → (List String ⊕ (Nat × Nat)) :=
    materializeActiveWithValidation combiningSafeDivProduct

def addValidatingSafeDivProduct
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    combiningSafeDivProduct >=>
    add

def accumulatingAddSafeDivProduct :
  ((Nat × Nat) × (Nat × Nat)) → (List String ⊕ Nat) :=
    materializeActiveWithValidation addValidatingSafeDivProduct

instance : Monad Task where
  pure := Task.pure
  bind := Task.bind

instance : MonadAsync Task where
  async := Task.spawn

abbrev TasksSpawningProgram :=
  computationValuedFunction Task

def materializeTasksSpawning {α β : Type} :
  TasksSpawningProgram α β → (α → β) :=
    λ αftβ α => (αftβ α).get

unsafe def tasksSpawningFibonacci :=
  materializeTasksSpawning parallelFibonacci

#eval activeFibonacci 10
#eval activeFactorial 10
#eval reactiveFibonacci 10
#eval reactiveFactorial 10

#eval statefulFibonacciPair () 10

#eval fastFailingAddSafeDivProduct ((10, 5), (8, 2))
#eval fastFailingAddSafeDivProduct ((10, 0), (8, 2))
#eval fastFailingAddSafeDivProduct ((10, 5), (8, 0))
#eval fastFailingAddSafeDivProduct ((10, 0), (8, 0))

#eval accumulatingAddSafeDivProduct ((10, 5), (8, 2))
#eval accumulatingAddSafeDivProduct ((10, 0), (8, 2))
#eval accumulatingAddSafeDivProduct ((10, 5), (8, 0))
#eval accumulatingAddSafeDivProduct ((10, 0), (8, 0))

#eval tasksSpawningFibonacci 10

#eval "---end---"

end ITP
