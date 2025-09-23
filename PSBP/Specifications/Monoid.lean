class Monoid (μ : Type) where
  ν : μ
  combine : μ → μ → μ

export Monoid (ν combine)

infixl:60 " * " => combine

