class Sequential
    (program : Type → Type → Type) where
  andThenProgram {α β γ : Type} :
    program α β → program β γ → program α γ

export Sequential (andThenProgram)

macro_rules
  | `($_ >=> $_) => Lean.Macro.throwError "disabled"

infixl:50 " >=> " => andThenProgram

def programAction
    [Sequential program] :
  program β γ → (program α β → program α γ) :=
    λ βpγ αpβ => αpβ >=> βpγ
