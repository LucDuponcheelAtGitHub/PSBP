import PSBP.Abbreviations

abbrev Sync := Id

abbrev syncProgram :=
  computationValuedFunction Sync

def materializeSync :
  syncProgram α β → function α β :=
    λ αspβ α => αspβ α

@[simp] theorem sync_id_map
  {α : Type}
  (sα : Sync α) :
    id <$> sα =
      sα := rfl

@[simp] theorem sync_comp_map
  {α β γ : Type}
  (sα : Sync α)
  (αfβ : function α β)
  (βfγ : function β γ) :
    (βfγ ∘ αfβ) <$> sα =
      βfγ <$> αfβ <$> sα := rfl

@[simp] theorem sync_pure_seq
  {α β : Type}
  (sα : Sync α)
  (αfβ : function α β) :
    pure αfβ <*> sα = αfβ <$> sα := rfl

@[simp] theorem sync_map_pure
  {α β : Type}
  (a : α)
  (αfβ : function α β) :
    αfβ <$> (pure a : Sync α) = pure (αfβ a) := rfl

@[simp] theorem sync_seq_pure
  {α β : Type}
  (a : α)
  (sαfβ : Sync (function α β)) :
    sαfβ <*> pure a = (λ αfβ => αfβ a) <$> sαfβ := rfl

@[simp] theorem sync_seq_assoc
  {α β γ : Type}
  (sα : Sync α)
  (sαfβ : Sync (function α β))
  (sβfγ : Sync (function β γ)) :
sβfγ <*> (sαfβ <*> sα) =
  (Function.comp <$> sβfγ) <*> sαfβ <*> sα := rfl

@[simp] theorem sync_pure_bind
  {α β : Type}
  (a : α)
  (αspβ : syncProgram α β) :
    pure a >>= αspβ = αspβ a := rfl

@[simp] theorem sync_bind_map
  {α β : Type}
  (sα : Sync α)
  (sαfβ : Sync (function α β)) :
    (sαfβ >>= λ β => β <$> sα) = sαfβ <*> sα := rfl

@[simp] theorem sync_pure_comp
  {α β : Type}
  (sα : Sync α)
  (αfβ : function α β) :
    sα >>= (λ α => pure (αfβ α)) = αfβ <$> sα := rfl

@[simp] theorem sync_bind_assoc
  {α β γ : Type}
  (sα : Sync α)
  (αspβ : syncProgram α β)
  (βspγ : syncProgram β γ) :
  sα >>= αspβ >>= βspγ =
    sα >>= λ α => αspβ α >>= βspγ := rfl
