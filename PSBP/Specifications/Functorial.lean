import PSBP.Abbreviations

class Functorial
    (program : Type → Type → Type) where
  functionAction {α β γ : Type} :
    function β γ → (program α β → program α γ)

export Functorial (functionAction)

def andThenFunction {α β γ : Type}
    [Functorial program] :
  program α β → function β γ → program α γ :=
    λ αpβ βfγ => functionAction βfγ αpβ

infixl:50 " >-> " => andThenFunction
