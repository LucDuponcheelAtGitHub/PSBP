import PSBP.Specifications.ProgramSpecifications

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
    (αpβ >-> βfγ >-> γfδ) =
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

class LawfulCreationalIf
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]: Prop where
  conditional_if_sequential
      (αpb : program α Bool)
      (t_apβ : program α β)
      (f_apβ : program α β)
      (βpγ: program β γ) :
    ((if_ αpb t_apβ f_apβ) >=> βpγ) =
      ((if_ αpb (t_apβ >=> βpγ) (f_apβ >=> βpγ)))
