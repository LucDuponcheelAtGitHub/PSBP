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

--
-- Functional
--

-- exercise
def identity
    [Functional program] :
  program α α :=
    asProgram λ α => α

def dup
    [Functional program] :
  program α (α × α) :=
    asProgram λ α => (α, α)

def first
    [Functional program] :
  program (α × β) α :=
    asProgram λ (α, _) => α

def second
    [Functional program] :
  program (α × β) β :=
    asProgram λ (_, β) => β

def applyAtFirst
    [Functional program] :
  (α → β) → program (α × γ) (β × γ) :=
    λ αfβ => asProgram λ (α, γ) => (αfβ α, γ)

def applyAtSecond
    [Functional program] :
  (β → γ) → program (α × β) (α × γ) :=
    λ βfγ => asProgram λ (α, β) => (α, βfγ β)

def swap
    [Functional program] :
  program (α × β) (β × α) :=
    asProgram λ (a, β) => (β, a)

def left
    [Functional program] :
  program γ (γ ⊕ β) :=
    asProgram .inl

def right
    [Functional program] :
  program β (γ ⊕ β) :=
    asProgram .inr

def assoc
    [Functional program] :
  program ((α × β) × γ) (α × (β × γ)) :=
    asProgram λ ((a, b), c) => (a, (b, c))

--
-- Creational
--

def let_
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program (α × β) γ → program α γ :=
    λ αpβ αaβpγ => identity &&& αpβ >=> αaβpγ

def in_ : α → α := id

-- exercise
def bothSeq
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      (first >=> αpγ) &&& second >=>
        first &&& (second >=> βpδ)

infixl:60 " <&> " => bothSeq

-- exercise
def onlyFirst
    [Functional program]
    [Creational program]
    [Sequential program] :
  program α β → program (α × γ) (β × γ) :=
    λ αpβ => (first >=> αpβ) &&& second

infixl:60 " <&& " => onlyFirst

-- exercise
def onlySecond
    [Functional program]
    [Creational program]
    [Sequential program] :
  program γ δ → program (α × γ) (α × δ) :=
    λ γpδ => first &&& (second >=> γpδ)

infixl:60 " &&> " => onlySecond

-- exercise
def bothSeq'
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      onlyFirst αpγ >=> onlySecond βpδ

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
-- Conditional
--

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
            | Sum.inr _ => sorry) >=> γpα) $
        (asProgram
          (λ γoβ => match γoβ with
            | Sum.inl _ => sorry
            | Sum.inr β => β) >=> βpα)

--
-- Parallel
--

def productPar {α β γ : Type}
    [Functional program]
    [Sequential program]
    [Parallel program] :
  program α β → program α γ → program α (β × γ) :=
   λ αpβ αpγ => dup >=> αpβ |&| αpγ

infixl:60 " &|& " => productPar

-- Positional

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
