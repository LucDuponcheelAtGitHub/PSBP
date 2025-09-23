import PSBP.Specifications.WithState

import PSBP.Implementations.ComputationValuedFunction.FunctionalImplementation

import PSBP.Implementations.ComputationValuedFunction.SequentialImplementation

import PSBP.Implementations.ComputationValuedFunction.CreationalImplementation

import PSBP.Implementations.ComputationValuedFunction.WithStateImplementation

class LawfulStateOf
    (σ : Type)
    (computation : Type → Type)
    [Monad computation]
    [MonadStateOf σ computation] : Prop where
  set_getThe :
    ((λ s => set s >>= λ _ => getThe σ) :
       σ → computation σ) =
      pure

export LawfulStateOf (set_getThe)

attribute [simp] set_getThe

--
-- TODO: why does this not work here
-- (it works in README.lean)
--

-- @[simp] theorem withState_write_read
--     [Monad computation]
--     [MonadStateOf σ (StateT σ computation)]
--     [LawfulStateOf σ (StateT σ computation)] :
--   ((writeState >=> (readState
--      : computationValuedFunction
--          (StateT σ computation) Unit σ))) =
--     identity := by
--   simp [writeState, andThenProgram, readState, asProgram]
