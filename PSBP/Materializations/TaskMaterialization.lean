import PSBP.Structures.ComputationValuedFunction

import PSBP.Implementations.ProgramImplementations

abbrev TaskProgram :=
  FromComputationValuedFunction Task

def materializeTask {α β : Type} :
  TaskProgram α β → (α → β) :=
    λ ⟨αftβ⟩ α => (αftβ α).get

