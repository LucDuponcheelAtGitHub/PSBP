import PSBP.Structures.ComputationValuedFunction

import PSBP.Implementations.ActiveImplementations

abbrev ActiveProgram :=
  FromComputationValuedFunction Active

def materializeActive :
    ActiveProgram α β → (α → β) :=
  λ ⟨αfaβ⟩ α => αfaβ α
