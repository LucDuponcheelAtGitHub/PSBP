import PSBP.Abbreviations

import PSBP.Materializations.Sync

abbrev programWithState σ computation :=
  computationValuedFunction (StateT σ computation)

def materializeWithState
    [Monad computation] {α β : Type} :
  programWithState σ computation α β →
    function α (function σ (computation β)) :=
    λ αpwscβ α σ =>
      αpwscβ α σ >>=
        λ (β, _) => pure β

abbrev syncProgramWithState σ :=
  programWithState σ Sync

def materializeSyncWithState {α β : Type} :
  syncProgramWithState σ α β → function α (function σ β) :=
    materializeWithState
