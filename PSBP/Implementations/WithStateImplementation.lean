import Init.Control.State

import PSBP.Specifications.WithStateSpecification

import PSBP.Structures.ComputationValuedFunction

instance [MonadStateOf σ computation] :
    WithState σ
      (FromComputationValuedFunction computation) where
  readState := ⟨λ _ => getThe σ⟩
  writeState := ⟨set⟩
