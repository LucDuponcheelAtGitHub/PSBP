import PSBP.Structures.ComputationValuedFunction

import PSBP.Implementations.ProgramImplementations

instance : Monad Task where
  pure := Task.pure
  bind := Task.bind

instance : MonadAsync Task where
  async := Task.spawn

abbrev TasksSpawningProgram :=
  FromComputationValuedFunction Task

def materializeTasksSpawning {α β : Type} :
  TasksSpawningProgram α β → (α → β) :=
    λ ⟨αftβ⟩ α => (αftβ α).get
