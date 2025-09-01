import PSBP.Specifications.ProgramSpecifications

import PSBP.Programs.PrimitivePrograms

unsafe def fibonacci
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      else_ $
        if_ isOne one $
          else_ $
            (minusOne >=> fibonacci) &&&
            (minusTwo >=> fibonacci) >=>
            add

-- exercise
unsafe def fibonacci'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      if_ isOne one $
        (minusOne >=> fibonacci') &&&
        (minusTwo >=> fibonacci') >=>
        add

unsafe def factorial
    [Functional program]
    [Sequential program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      else_ $
        let_ (minusOne >=> factorial) $
          in_ $
            multiply

-- exercise
unsafe def factorial'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      let_ (minusOne >=> factorial') $
        multiply

unsafe def parallelFibonacci
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [Parallel program] :
  program Nat Nat :=
    if_ isZero one $
      if_ isOne one $
        (minusOne >=> parallelFibonacci) &|&
        (minusTwo >=> parallelFibonacci) >=>
        add

def twiceMinusOneFunctorial
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >-> addF

def twiceMinusOneSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >=> add
