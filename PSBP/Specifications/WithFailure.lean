import PSBP.Abbreviations

class WithFailure
    (ε : outParam Type)
    (program : Type → Type →Type) where
  failWithFunction {α β : Type} :
    function α ε → program α β
