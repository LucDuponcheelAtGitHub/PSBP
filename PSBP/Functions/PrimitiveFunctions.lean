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

-- def diagF : Nat → Nat × Nat :=
--   λ n => (n, n)

def twoF : Nat → Nat :=
  λ _ => 2

def threeF : Nat → Nat :=
  λ _ => 3

def isNotZeroF: Nat → Bool :=
  λ n => n != 0

def unsafeDivF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n / m
