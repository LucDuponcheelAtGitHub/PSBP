import PSBP.Implementations.ComputationValuedFunction.ParallelImplementation

import PSBP.Materializations.Sync

abbrev Async := Task

instance : Monad Async where
  pure := Task.pure
  bind := Task.bind

instance : MonadAsync Async where
  async := Task.spawn

abbrev asyncProgram :=
  computationValuedFunction Async

def materializeAsync {α β : Type} :
  asyncProgram α β → function α β :=
    λ αaspβ α =>
      (materializeSync αaspβ α).get
