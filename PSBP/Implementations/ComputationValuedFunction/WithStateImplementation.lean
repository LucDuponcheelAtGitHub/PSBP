import PSBP.Specifications.WithState

instance
    [MonadStateOf σ computation] :
  WithState σ
    (computationValuedFunction computation) where
    readState := λ _ => getThe σ
    writeState := set
