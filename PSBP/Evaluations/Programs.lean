import PSBP.Implementations.ProgramImplementations

import PSBP.Materializations.ActiveMaterialization
import PSBP.Materializations.ReactiveMaterialization
import PSBP.Materializations.TaskMaterialization

import PSBP.Programs.Programs

#eval
  materializeActive fibonacci 10 ==
    89

#eval
  materializeActive factorial 10 ==
    3628800

#eval
  materializeActive twiceMinusOne01 10 ==
   2 * (10 - 1)

#eval
  materializeActive twiceMinusOne02 10 ==
    2 * (10 - 1)

#eval
  materializeReactive fibonacci 10 ==
    89

#eval
  materializeReactive factorial 10 ==
    3628800

#eval
  materializeReactive twiceMinusOne01 10 ==
    2 * (10 - 1)

#eval
  (materializeReactive twiceMinusOne02 10) ==
    2 * (10 - 1)

#eval
  materializeTask parallelFibonacci 10 ==
  89
