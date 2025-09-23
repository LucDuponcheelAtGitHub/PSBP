import PSBP.Abbreviations

class Functional
    (program : Type → Type → Type) where
  asProgram {α β : Type} :
    function α β → program α β

export Functional (asProgram)

def identity
    [Functional program] :
  program α α :=
    asProgram λ α => α

def first
    [Functional program] :
  program (α × β) α :=
    asProgram λ (α, _) => α

def second
    [Functional program] :
  program (α × β) β :=
    asProgram λ (_, β) => β

def duplicate
    [Functional program] :
  program α (α × α) :=
    asProgram λ α => (α, α)

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

def left
    [Functional program] :
  program γ (γ ⊕ β) :=
    asProgram .inl

def right
    [Functional program] :
  program β (γ ⊕ β) :=
    asProgram .inr
