import PSBP.Abbreviations

import PSBP.Materializations.Sync

import PSBP.Materializations.Transformations.ReactiveT

abbrev syncReactiveProgram ρ :=
  ReactiveProgram ρ Sync

def materializeSyncReactive {α β : Type} :
  (syncReactiveProgram β) α β → function α β :=
    λ αsrpβ α =>
       materializeSync (αsrpβ α) id

abbrev SyncReactive ρ := ReactiveT ρ Sync

@[simp] theorem syncReactive_id_map
  {α : Type}
  (srα : SyncReactive ρ α) :
    id <$> srα =
      srα := rfl

@[simp] theorem syncReactive_comp_map
  {α β γ : Type}
  (srα : SyncReactive ρ α)
  (αfβ : function α β)
  (βfγ : function β γ) :
    (βfγ ∘ αfβ) <$> srα =
      βfγ <$> αfβ <$> srα := rfl

@[simp] theorem syncReactive_pure_seq
  {α β : Type}
  (srα : SyncReactive ρ α)
  (αfβ : function α β) :
    pure αfβ <*> srα = αfβ <$> srα := rfl

@[simp] theorem syncReactive_map_pure
  {α β : Type}
  (a : α)
  (αfβ : function α β) :
    αfβ <$> (pure a : SyncReactive ρ α) =
      pure (αfβ a) := rfl

@[simp] theorem syncReactive_seq_pure
  {α β : Type}
  (a : α)
  (srαfβ : SyncReactive ρ (function α β)) :
    srαfβ <*> pure a = (λ αfβ => αfβ a) <$> srαfβ := rfl

@[simp] theorem syncReactive_seq_assoc
  {α β γ : Type}
  (srα : SyncReactive ρ α)
  (srαfβ : SyncReactive ρ (function α β))
  (srβfγ : SyncReactive ρ (function β γ)) :
srβfγ <*> (srαfβ <*> srα) =
  (Function.comp <$> srβfγ) <*> srαfβ <*> srα := rfl

@[simp] theorem syncReactive_pure_bind
  {α β : Type}
  (a : α)
  (αfsrβ : function α (SyncReactive ρ β)) :
    pure a >>= αfsrβ = αfsrβ a := rfl

@[simp] theorem syncReactive_bind_map
  {α β : Type}
  (srα : SyncReactive ρ α)
  (srαfβ : SyncReactive ρ (function α β)) :
    (srαfβ >>= λ β => β <$> srα) = srαfβ <*> srα := rfl

@[simp] theorem syncReactive_pure_comp
  {α β : Type}
  (srα : SyncReactive ρ α)
  (αfβ : function α β) :
    srα >>= (λ α => pure (αfβ α)) = αfβ <$> srα := rfl

@[simp] theorem syncReactive_bind_assoc
  {α β γ : Type}
  (srα : SyncReactive ρ α)
  (αfsrβ : function α (SyncReactive ρ β))
  (βfsrγ : β → SyncReactive ρ γ) :
  srα >>= αfsrβ >>= βfsrγ =
    srα >>= λ α => αfsrβ α >>= βfsrγ := rfl
