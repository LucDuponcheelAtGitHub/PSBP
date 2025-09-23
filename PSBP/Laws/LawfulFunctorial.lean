import PSBP.Specifications.Functorial

class LawfulFunctorial
    (program : Type → Type → Type)
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
