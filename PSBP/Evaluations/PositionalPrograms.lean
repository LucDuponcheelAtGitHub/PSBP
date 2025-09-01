import PSBP.Implementations.ProgramImplementations

import PSBP.Materializations.ActiveMaterialization
import PSBP.Materializations.ReactiveMaterialization

import PSBP.Programs.PositionalPrograms

#eval
  materializeActive somePositionalProgram01 10 ==
    (10 - 2) + 2 * (10 - 1) + 3
#eval
  materializeActive somePositionalProgram02 10 ==
    (10 - 2) + 2 * (10 - 1) + 3

#eval
  materializeActive positionalFactorialOfFibonacci ((), 5) ==
    40320

#eval
  materializeActive positionalSumOfFibonacciAndFactorial ((), 10) ==
    3628889

#eval
  materializeActive positionalFactorialOfFibonacci' ((), 5) ==
    ((((), 5), 8), 40320)

#eval
  materializeActive positionalSumOfFibonacciAndFactorial' ((), 10) ==
    (((((), 10), 89), 3628800), 3628889)

#eval
  materializeReactive somePositionalProgram01 10 ==
    (10 - 2) + 2 * (10 - 1) + 3
#eval
  materializeReactive somePositionalProgram02 10 ==
    (10 - 2) + 2 * (10 - 1) + 3

#eval
  materializeReactive positionalFactorialOfFibonacci ((), 5) ==
    40320

#eval
  materializeReactive positionalSumOfFibonacciAndFactorial ((), 10) ==
    3628889

#eval
  materializeReactive positionalFactorialOfFibonacci' ((), 5) ==
    ((((), 5), 8), 40320)

#eval
  materializeReactive positionalSumOfFibonacciAndFactorial' ((), 10) ==
    (((((), 10), 89), 3628800), 3628889)
