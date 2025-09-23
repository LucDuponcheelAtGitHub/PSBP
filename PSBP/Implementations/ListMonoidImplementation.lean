import PSBP.Specifications.Monoid

instance : Monoid (List α) where
  ν := .nil
  combine := .append
