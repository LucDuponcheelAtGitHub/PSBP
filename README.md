# Program Specification Based Programming

**author** Luc Duponcheel

**implementation language** `Lean`

## Warning

Expect frequent changes and/or additions.

## Naming conventions

Every document uses its naming conventions. Below are the two most relevant ones of this document.

- The word "programming" has a general meaning : (the process of) writing code.
- The word "program" has a specific meaing : it refers to`program`, intentionally shown in typewriter font, the name of
the parameter of various programming related `Lean` binary type constructor classes. 

The title of this document uses the naming conventions above.

I hope that the naming conventions above do not lead to any confusion.

## About

This document can be used as a, somewhat special, not to say opinionated, programming course. 

It is not a `Lean` programming course. 

The course is especially useful for mathematicians and/or computer scientists, researchers as well as students, who are
interested in mathematical foundations of programming.

You may wish to skip the introduction sections 
[`PSBP`](https://github.com/LucDuponcheelAtGitHub/PSBP?tab=readme-ov-file#psbp)
and
[Programs versus Computations](https://github.com/LucDuponcheelAtGitHub/PSBP?tab=readme-ov-file#programs-versus-computations) 
and start with section
[The `PSBP` Library](https://github.com/LucDuponcheelAtGitHub/PSBP?tab=readme-ov-file#the-psbp-library),
the section where the programming course starts.

The repository contains a file
[All.lean](https://github.com/LucDuponcheelAtGitHub/PSBP/blob/master/PSBP/All.lean) containing all the code of the
`PSBP` library and all the solutions to the exercises of the course.

You may wish to produce such a file from scratch, copy/pasting library code and writing solutions to exercises yourself.

What is special about the course is that its code is a programming course for `Lean` itself!

When I worked with Doaitse Swierstra at the University of Utrecht, he once told me that, apart from sound proving 
techniques like "proof by induction", there is also this unsound proving technique "proof by intimidation". If no
student complains about the correctness of a proof, then a proof is correct. Of course, Doaitse did not apply this
technique when teaching. `Lean` would be this very demanding student asking you for more and more proof details before
willing to accept the correctness of a proof. But `Lean` would also be this very helpful student that would be able to
infer proof details for you.

The first sections of the course do not always define all concepts they use. Please keep on reading. All concepts will,
eventually, be defined. Starting from section "The `PSBP` library", the course is self-contained and requires, at least
in theory, no previous knowledge (apart from some `Lean` knowledge). In particular, the course does not use concepts
that have not been defined. `Lean` would complain if that would be the case (and you should complain as well).

Let's end this section with a, somewhat offensive (I am sorry), statement that, hopefully, motivates you to keep on
reading the course : if `Lean` can understand the course then you should be able to understand it as well.

## `PSBP` (optional)

`PSBP` is a pointfree effectful functional programming library, written using the `Lean` programming language.

Apart from "functional programming" the statement above uses two important adjectives and one important noun

- adjective "pointfree" (as opposed to "pointful")
- adjectuve "effectful" (as opposed to "effectfree")

The adjectives above will be explained later in the course.

- noun "library" (as opposed to "language")

Hopefully, the nouns above do not need explanation.

In what follows `Lean` will not often explictly be mentioned any more.

`PSBP` can be seen as a programming DSL, a Domain Specific Language for the programming domain.

`PSBP` has a variety of programming related binary type constructor classes, among others `Functional`, `Functorial`,
`Creational`, `Sequential`, `Conditional` and `Parallel`. `PSBP` code, consistently, names their binary type constructor
parameter `program`.

The `PSPB` type constructor classes are specifications, also called interfaces in the programming world. They have 
members that declare basic program specifications and basic program specification combinators to combine program 
specifications to composite program specifications. Derived program specifications and derived program specification 
combinators can then be defined in terms of (declared or defined) program specifications and (declared or defined) 
program specification combinators. As such program specifications are components of a component system.

The `Lean` standard library has a variety of computing related unary type constructor classes, among others `Functor`,
`Applicative` and `Monad`. `PSBP` code consistently names their unary type constructor parameter `computation`.

The `Lean` standard library type constructor classes above are specifications, also called interfaces in the programming 
world. They have members that declare basic computation specifications and basic computation specification combinators 
to combine computation specifications to composite computation specifications. Derived computation specifications and 
derived computation specification combinators can then be defined in terms of (declared or defined) computation s
pecifications and (declared or defined) computation specification combinators. As such computation specifications are
components of a component system.

By now you may be asking why I used "program specification" resp. "computation specification" instead of simply using
"program" resp. "computation". First of all, because program specifications resp. computation specifications are
specifications. Type constructor class instances, also called implementations in the programming world, of the various
type constructor classes, in terms of whose members program specifications resp. computation specifications are written,
need to be given to materialize program specifications resp. computation specifications. It are those materializations
that are called programs resp. computations, now, intentionally, not written in typewriter font.

In what follows, by abuse of language, I will often also use the word "program" (recall, a materialization of a program
specification corresponding to instances of programming related type constructor classes) instead of
"program specification", resp. often use the word "computation" (recall, a materialization of a computation
specification corresponding to instances of computing related type constructor classes) instead of
"computation specification".

Hopefully this abuse of language does not lead to any confusion.

It is instructive to compare this abuse of language with the title,
[Ceci n'est pas une pipe](https://en.wikipedia.org/wiki/The_Treachery_of_Images),
of the painting of René Magritte. The painting itself is, of course, not a pipe, it is a description of a pipe. yet,
when you look at the painting, you might think of it as being a pipe. Much in the same way, when an architect is showing
you the description of a house, for example as a 3D animation, communication, by abuse of language, happens using words
like kitchen resp. bathroom instead of kitchen description resp. bathroom desctiption.

Agreed, specifications and descriptions are not really the same thing. Specifications specify what things should be all
about, while descriptions describe what things are all about. As such a descriptions is a (special kind of)
implementation.

## Programs versus Computations (optional)

The computing related unary type constructor classes of the standard `Lean` library already enable
"Computation Specification Based Programming". 

So why promoting the `PSBP` library enabling "Program Specification Based Programming"?

In short, informally, and very subjective, it is all a matter of taste (you may wish to give up reading the course).

In short, formally, and also subjective, because of my progressive insight into what programming, writing code, is all 
about.

I'll explain this in terms of my personal history as mathematician and programmer (again, you may wish to ignore what 
follows, not being interested in my personal history).

I am a retired mathematician and programmer.

Mathematics is generally agreed upon to be useful to understand the reality we are part of. For example to understand 
(concepts of) problem domains we are confronted with.

Bridge building engineers may benefit form studying appropriate mathematics to understand what bridge requirements are
all about. Vehicle building engineers may benefit form studying appropriate mathematics to understand what vehicle
requirements are all about. Likewise, programming engineers (programmers) may benefit form studying appropriate 
mathematics to understand what program requirements are all about.

I used the word "requirements" because separating specifications from implementations is generally agreed upon to be 
useful to understand problem domains 

A bridge specification states, for example, that it must be able to carry the weight of a number of vehicles. How it is
able to carry that weight is an implementation concern. Maybe one bridge is more pleasing to look at, more durable than,
or less expensive than another one. A vehicle specification states, for example, that it must be able to transport a
number of passengers. How it is able to transport that number of passengers is an implementation concern. Maybe one car 
is more comfortable than, or less fuel consuming than another one. A program specification states, for example, that a 
program must be able to create composite data and to perform conditional logic. How it is able to create composite data 
and to perform conditional logic is an implementation concern. Maybe one program is more CPU effecient, or less RAM 
consuming than another one.

Note that I wrote bridge, resp. car, resp. program instead of of materialization corresponding to an implementation of
the bridge specification, resp. car specification, resp. program specification.

I have always been interested in mathematics, so I followed the typical mathematics oriented education path, from
secondary school, all the way to obtaining a PhD in mathematics. Now I realize that obtaining a PhD in mathematics is
just a beginning. It shows that you are able to use 20% imagination and 80% transpiration to be creative.

I did, among others, mathematics research on
[Non-archimedean induced representations of compact zerodimensional groups](https://www.numdam.org/article/CM_1986__57_1_3_0.pdf).
Not that it matters much for the content of the course. What does matter is that I soon realized that, those days, in
Belgium, mathematics could mainly be done as a backyard ritual. In order to earn money for a living, I decided to become
a programmer. Being addicted to research, I also decided to do computer science research as a late at night hobby
(typically starting at 9PM).

I studied function level programming systems, supported, for example, by the pointfree effectfree functional programming
language [`FP`](https://en.wikipedia.org/wiki/FP_%28programming_language%29).

I published a paper
[Acceptable functional programming systems](https://link.springer.com/article/10.1007/BF00268076)
about a pointfree effectfree functional programming "toy" library. I wrote the paper together with my twin brother Marc,
who is also a mathematician. The library was written using the
[`Pascal`](https://en.wikipedia.org/wiki/Pascal_%28programming_language%29) programming language.

The first functional programming language I used extensively was
[`Miranda`](https://en.wikipedia.org/wiki/Miranda_%28programming_language%29).
`Miranda` turned out to be a perfect language to learn (not to say become addicted to) functional programming.

The next functional programming language I used extensively was `Gofer`, later evolving to
[`Hugs`](https://www.haskell.org/hugs/).
`Gofer` was the first functional programming language supporting type constructor classes. They are appropriate to write
the computing related specifications of effectful pointful functional programming libraries. The mathematical
foundations of those specifications are [monads](https://en.wikipedia.org/wiki/Monad_%28functional_programming%29).

I published the following papers

1. [Composing monads](https://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf),
2. [On the expressive power of constructor classes](https://link.springer.com/chapter/10.1007/978-1-4471-3573-9_3),
3. [Deterministic error-correcting combinator parsers](https://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/error-correcting.pdf),
4. [Using catamorphisms, subtypes and monad transformers for writing modular functional interpeters](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=1cdebf7b88435a38156f62df1a7189b6d8ca3fe3).

The first paper was written, as a late night hobby, together with Mark P. Jones, the author of `Gofer`. What is
special about that paper is that we had never physically met each other. Those were the early days of the internet. We
wrote the paper (and the code that came with it) together by sending emails with attachments to each other. In fact, it
turned out to be an efficient way to work together. While one of us was sleeping (in Europe resp. the USA) the other one
was working (in the USA resp. Europe). The paper contained equational proofs. Those days, they were simply encoded as 
lists of expressions representing equational proof steps. `Gofer` did not have a formal proof correctness reviewing
mechanism like `Lean` has, but, type correctness provided at least some degree of formal proof correctness confidence.

The other papers were written while I was working, for two years, at the University of Utrecht. That was a unique
experience for me for which I am, forever, greatful to Doaitse Swierstrsa. Erik Meijer, those years also working at the
University of Utrecht, invited me for a lecture, as such introducing me to Doaitse. Apart from being an outstanding
computer scientist, Doaitse was the best people manager I have ever worked for. Erik does not need any introduction. He
became a living legend. Also Graham Hutton worked at the University of Utrecht when I was working there. I was in good
company.

The second paper was written together with Erik Meijer and was presented at a conference organized by the Glasgow
University, the center of Functional Programming in the UK (Philip Wadler and Simon Peyton Jones worked there).

The third paper was written together with Doaitse Swierstra. It turned out to be a motivation for two type
constructor classes,
[arrows](https://link.springer.com/chapter/10.1007/11546382_2) and
[applicatives](https://www.researchgate.net/publication/215446169_Applicative_Programming_with_Effects),
based upon mathematical foundations. Arrow based libraries are pointfree effectful functional programming libraries.
Applicative based libraries are pointful effectful functional programming libraries. The relationship between the
mathematical foundations was explored in
[Monads, Arrows and Applicatives](https://homepages.inf.ed.ac.uk/wadler/papers/arrows-and-idioms/arrows-and-idioms.pdf).

The fourth paper was based on
[Monad transformers and modular interpreters](https://dl.acm.org/doi/pdf/10.1145/199448.199528).
I added catamorphisms and subtypes to make interpreter alternatives reusable. The paper has never been published.
Anyway, somehow, surprisingly, the paper turned out to be available online and it has been cited several times.

All this brings me to my progressive insight that motivates me to do this `PSBP` project.

What are the most appropriate mathematical foundations and corresponding type classes for effectful functional
programming? The more powerful they are the less implementation flexibility they have. For example monadic parsers
can parse context sensitive grammars, while applicative parsers cannot, but, applicative parsers allow for more flexible
error handling than monadic parsers when parsing context free grammars.

Programming is also about elegance and programming libraries are also about ease of use. Pointfree programming is more 
elegant than pointful programming and pointfree programming libraries are easier of use than pointful programming 
libraries. Of course this is a matter of taste.

The `Applicative` specification and the `Monad` specification specify computation capabilities. Think of computations as
effectful expressions. They are operational artifacts. They do not really have a meaning in the mathematical sense and
cannot be given a meaningful name. How, for example, would you name expression `x * x` in `λ x => x * x` using
meaningful names? Just like expressions are evaluated to yield a value, computations, are, somehow, executed to yield a
value, but, somehow executing computations may also perform side effects along the way. The "somehow" in the previous
sentence is important, because it depends on the materialization corresponding to instances of the type constructor
classes in terms of whose members the computations (recall, more precisely, computation specificatons) have been
written.

The programming related specifications of the course specify, not surprisingly, program capabilities. Think of programs
as effectful functions. They are denotational artifacts. They do have a meaning in the mathematical sense and can be
given a meaningful name. For example `λ x => x * x` can be given the meaningful name `square`. Of course functions and
programs can also be looked at as operational artifacts. Just like functions, programs, by, somehow, running them, 
transform an initial value to a final value, but, somehow running them may perform side effects along the way. The 
"somehow" in the previous sentence is important, because it depends on the materialization corresponding to instances of 
the type constructor classes in terms of whose members the programs (recall, more precisely, program specificaton) have 
been written.

By the way, a value can be an basic-value or a composite-value, repesented as a (nested) tuple. As such values are
components of a component system.

It is more natural to think denotationally, about "what", than to think operationally, about "how".

Let's try to illustrate this with some `Lean` code fragments.

I use a few naming conventions

- Functions of type `α → computation β` are called `αfcβ`.

- Programs of type `program α β` are called `αpβ`.

I simply use the word "class" instead of "type class", "type constructor class", "unary type constructor class",
"binary type constructor class" and so on ... .

Consider

```lean
bind :
  {α β : Type} →
  computation α → (α → computation β) → computation β
```

with an associativity law (`>>=` is infix notation for `bind`)

```lean
bind_assoc
  (cα : computation α)
  (αfcβ : α → computation β)
  (βfcγ : β → computation γ) :
    cα >>= αfcβ >>= βfcγ =
      cα >>= λ α => αfcβ α >>= βfcγ
```

Consider

```lean
andThen
  {α β γ : Type} :
  program α β → program β γ → program α γ
```

with an associativity law (`>=>` is infix notation for `andThen`)

```lean
andThen_assoc
  (αpβ : program α β)
  (βpγ : program β γ)
  (γpδ : program γ δ) :
  (αpβ >=> βpγ) >=> γpδ =
    αpβ >=> (βpγ >=> γpδ)
```

Let's first consider syntax.

I can more easily remember the definition of `andThen_assoc` than the definition of `bind_assoc`.

What about you?

Let's next consider semantics.

I can more easily explain `>=>` and `andThen_assoc` than `>>=` and `bind_assoc`.

### Exercise

*Exercise*

Explain both `>=>` and `andThen_assoc`, resp. `>>=` and `bind_assoc`.

*Hint* : (you may wish to ignore the hint) :

I think of a function as transforming an initial value yielding a final value. Likewise, I think of a program as
transforming an initial value yielding a final value, potentially performing side effects along the way.

I think of evaluating an expression as yielding a value. Likewise, I think of executing a computation as yielding a   
value, potentially performing side effects along the way.

You can, for your explanation, ignore side effects for now. Of course you may wish to explain side effects as well.

### Solution

<details>

`>=>` can be explained as:

transforming an initial value of type `α`, using a program of type `program α β`, yielding an intermediate value of type
`β`, and then transforming that intermediate value, using a program of type `program β γ`, yielding a final value of
type `γ`.

`andThen_assoc` can be explained as:

first transforming an initial value of type `α`, using `αpβ >=> βpγ`, yielding an intermediate value of type `γ`, and
then transforming that intermediate value, using `γpδ`, yields the same final value as first transforming the initia
value of type `α`, using `αpβ`, to an intermediate value of type `β`, and then transforming that intermediate value,
using `βpγ >=> γpδ`.

`>>=` can be explained as:

executing an inner computation of type `computation α` yielding an intermediate value of type `α`, and then binding that
intermediate value to an outer computation valued function of type `α → computation β`, yields an outer computation of
type `computation β` (that, when executing it, yields a value of type `β`)

or

executing an inner computation of type `computation α` yielding an intermediate value of type `α`, and then transforming
that intermediate value using an outer computation valued function of type `α → computation β`, yields an outer
computation value of type `computation β` (that, when executing it, yields a value of type `β`)

`bind_assoc` can be explained as:

executing an inner computation of `cα` yielding a first intermediate value, and then transforming that intermediate
value using an intermediate computation valued function `αfcβ`, yielding an intermediate computation value that, when
executing it, yields a second intermediate value, and then transforming that intermediate value using a final
computation valued function `βfcγ` yields the same final outer computation value as executing the inner computation of
`cα` yielding a first intermediate value, and then transforming that intermediate value using the computation valued
function that first transforms that first intermediate value using the intermediate computation valued function `αfcβ`,
yielding an intermediate computation value that, when executing it, yields a second intermediate value, and then
transforming that intermediate value using a final computation valued function `βfcγ`.

</details>

### Programs and computations as components

As far as being components of a component system is concerned, I also like programs more than computations.

Let's have a closer look at the associativity law code fragments

```lean
(αpβ >=> βpγ) >=> γpδ =
  αpβ >=> (βpγ >=> γpδ)
```

and

```lean
cα >>= αfcβ >>= βfcγ =
  cα >>= λ α => αfcβ α >>= βfcγ
```

Programs are closed components, while computations are open components. 

Computation `cα` needs to be opened to access the value `α` yielded by executing it, so that it can be transformed using
computation valued function `αfcβ`. Programming with computations is pointful programming.

Programs do not need to be opened. Programming with programs is pointfree programming. Think of using them as playing
with Lego ariifacts.

It is a more elegant and easier to program pointfree than to program pointful. Likewise, It is more elegant and easier
to reason in terms of pointfree laws than to reason in terms of pointful laws. Of course, this is a matter of taste.
Nevertheless I hope to convince you.

### Positional Programming

It is possible, and sometimes more elegant, to use programs positionally. Positional programming is similar to pointful
programming. It is is useful for writing sequential recipe-like programs, where, starting from an initial value
(often a composite-value), intermediate values (typically basic-values) are created, and, together with the initial
value, are passed as a composite value to the next step of the recipe-like program, until a final value is yielded. The
initial value and intermediate values are accessed positionally. The creation of the intermediate values can involve
general programs. A sequential recipe-like program glues programs together. For effectful programs, the order in which
programs are glued together really matters.

It is instructive to think about gluing together programs as being similar to gluing together executables using an
operating system script, for example, a `bash` script of a `Linux` operating system.

## The `PSBP` library

For readability reasons, we define the `function` abbreviation.

```lean
abbrev function α β := α → β
```

The `PSBP` library type constructor classes are binary type constructor classes.

Recall that we simply called them classes.

We use few naming conventions

- Functions of type `function α β` are called `αfβ`.

- Programs of type `program α β` are called `αpβ`

Much in the same way we will also use names like `αaβ`, where the `a` stands for "and", and names like `αoβ`, where the
`o` stands for "or".

We use few showing conventions

- Functions of type `function α β` are shown as

```
      α ╭---╮ β
    >───┼αfβ┼───>
        ╰---╯
```

- Programs of type `program α β` are shown as

```
      α ╭───╮ β
    >───┼αpβ┼───>
        ╰───╯
```

By abuse of language, the requirements of the `PSBP` classes are written as statements.

It is instructive to compare this with using a statement like "a train can bring us form A to B" instead of using the
requirement "a train should be able to bring us from A to B".

### `class Functional`

Functions can, somehow, be used as programs. Functions that are used as programs are effectfree. We define functions as
programs in a formal way using `class Functional`.

```lean
class Functional
    (program : Type → Type → Type) where
  asProgram {α β : Type} :
    function α β → program α β

export Functional (asProgram)
```

```
        ╭───────╮
      α | ╭---╮ | β
    >───┼─┼αfβ┼─┼───>
        | ╰---╯ |
        ╰───────╯
```

Note that `functional` is an abbreviation while `program` is the parameter of `class Functional`.

The "somehow" in the sentence above is important. Functions can be programs "as-is", but do not need to be programs
"as-is".

### Exercise (`identity` using `asProgram`)

*Exercise*

Define `identity`, an, although simple, extremely useful function as a program.

*Hint*

Functions can be defined using lambda expressions.

Replace `sorry` in `identityExercise` below.

```lean
def identityExercise
    [Functional program] :
  program α α :=
    sorry
```

### Solution (`identity`)

<details>

```lean
def identity
    [Functional program] :
  program α α :=
    asProgram λ α => α
```

</details>

### `class Functorial`

Functions can act upon programs. Functions act in an effectfree way upon programs. We define functions acting upon
programs in a formal way using `class Functorial`.

```lean
class Functorial
    (program : Type → Type → Type) where
  functionAction {α β γ : Type} :
    function β γ → (program α β → program α γ)

export Functorial (functionAction)
```

### Exercise (`andThenFunction` using `functionAction`)

*Exercise*

Define `andThenFunction`, sequentially composing a program with a function, using `functionAction`.

*Hint*

Replace `sorry` in `andThenFunctionExercise` below.

```lean
def andThenFunctionExercise {α β γ : Type}
    [Functorial program] :
  program α β → function β γ → program α γ :=
    sorry 
```

### Solution (`andThenFunction`)

<details>

```
def andThenFunction {α β γ : Type}
    [Functorial program] :
  program α β → function β γ → program α γ :=
    λ αpβ βfγ => functionAction βfγ αpβ
```

```
        ╭───────────────╮
      α | ╭───╮ β ╭---╮ | γ
    >───┼─┤αpβ├─>─┼βpγ┼─┼───>
        | ╰───╯   ╰---╯ |
        ╰───────────────╯
```

`andThenFunction` also has infix notation `>->`.

```lean
infixl:50 " >-> " => andThenFunction
```

</details>


### `class Sequential`

Programs can be sequentially combined. We define programs that are sequentially combined in a formal way using
`class Sequential`.

```lean
class Sequential
    (program : Type → Type → Type) where
  andThenProgram {α β γ : Type} :
    program α β → program β γ → program α γ

export Sequential (andThenProgram)

macro_rules
  | `($_ >=> $_) => Lean.Macro.throwError "disabled"

infixl:50 " >=> " => andThenProgram
```

```
        ╭───────────────╮
      α | ╭───╮ β ╭───╮ | γ
    >───├─┤αpβ├─>─┤βpγ├─├───>
        | ╰───╯   ╰───╯ |
        ╰───────────────╯
```

`andThenProgram` also has infix notation `>=>`.

The `macro_rules` is necessary to avoid an operator naming conflict.

### Exercise (`programAction` using `andThenProgram`)

Sequentially combining programs can be seen as a second program acting upon a first program. The difference with 
`Functorial` is that the second program can be effectful. Side effects are performed from left to right. Moreover, side 
effects of the second program can depend on the final value yielded by the first program.

*Exercise*

Define `programAction`, a second program acting upon a first program, using `andThenProgram`.

*Hint*

Replace `sorry` in `programActionExercise` below.

```lean
def programActionExercise
    [Sequential program] :
  program β γ → (program α β → program α γ) :=
    sorry
```

### Solution (`programAction`)

<details>

```lean
def programAction
    [Sequential program] :
  program β γ → (program α β → program α γ) :=
    λ βpγ αpβ => αpβ >=> βpγ
```

</details>

### Exercise (`Functorial` using `Functional` and `Sequential`)

Together with `Functional`, `Sequential` is more powerful than `Functorial`.

*Exercise*

Define a `Functorial` instance using `Functional` and `Sequential`.

*Hint*

Recall that functions can be used as programs.

Replace `sorry` in the `Functorial` implementation below.

```lean
instance
    [Functional program]
    [Sequential program] :
  Functorial program where
    functionAction {α β γ: Type} :
      function β γ → (program α β → program α γ) :=
        sorry
```

### Solution (`Functorial` using `Functional` and `Sequential`)

<details>

```lean
instance
    [Functional program]
    [Sequential program] :
  Functorial program where
    functionAction {α β γ: Type} :
      function β γ → (program α β → program α γ) :=
        λ βfγ αpβ => αpβ >=> asProgram βfγ
```

</details>

### `class Creational`

Programs can be combined to, sequentially, create product values. Their side effects are performed from left to right.
We define programs that, sequentially, produce product values in a formal way using `class Creational`.

```lean
class Creational
    (program : Type → Type → Type) where
  sequentialProduct {α β γ : Type} :
    program α β → program α γ → program α (β × γ)

export Creational (sequentialProduct)

infixl:60 " &&& " => sequentialProduct
```

```
        ╭──────────╮
        |   ╭───╮  | β
        │ 1─|αpβ|──┼───>
      α | | ╰───╯  |
    >───┼─┼        │ ×
        | | ╭───╮  |
        │ 2─|αpγ|──┼───>
        |   ╰───╯  | γ
        ╰──────────╯
```

`sequentialProduct` also has infix notation `&&&`.

### Exercise (`bothSequential`)

*Exercise*

Define `bothSequential` (and corresponding `infixl:60 " <&> "`) using `sequentialProduct` (infix `&&&`).

*Hint*

Replace `sorry` in `bothSeqExercise` below.

```lean
def bothSeqExercise
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    sorry
```

Define `first` and `second` and, somehow, use them, together with `>=>`, from `Sequential`, and `&&&`, from
`Creational`.

```lean
def first
    [Functional program] :
  program (α × β) α :=
    sorry
```

```lean
def second
    [Functional program] :
  program (α × β) β :=
    sorry
```

This exercise is an example of a "getting the types right puzzle". Those puzzles are not always easy to solve. 

### Solution (`bothSequential` using `sequentialProduct`)

<details>

Let

```lean
def first
    [Functional program] :
  program (α × β) α :=
    asProgram λ (α, _) => α
```

and

```lean
def second
    [Functional program] :
  program (α × β) β :=
    asProgram λ (_, β) => β
```

in

```lean
def bothSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      (first >=> αpγ) &&& second >=>
        first &&& (second >=> βpδ)

infixl:60 " <&> " => bothSequential
```

</details>

### Exercise (`bothSequential'` using `onlyFirst` and `onlySecond`)

*Exercise*

Define `onlyFirst` and `onlySecond` using `first` and `second`.

Define `bothSequential'` using `onlyFirst` and `onlySecond`. 

Perhaps you already defined `onlyFirst` and `onlySecond` (perhaps using other names) to solve the
the previous exercise. If so, rename them using `onlyFirst` and `onlySecond` so that they can also be reused later using
those names.

*Hint*

Replace `sorry` in `onlyFirst`, `onlySecond` and `bothSeqExercise` below.

```lean
def onlyFirst
    [Functional program]
    [Creational program]
    [Sequential program] :
  program α β → program (α × γ) (β × γ) :=
    sorry
```

```lean
def onlySecond
    [Functional program]
    [Creational program]
    [Sequential program] :
  program γ δ → program (α × γ) (α × δ) :=
    sorry
```

```lean
def bothSequential'
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      onlyFirst αpγ >=> onlySecond βpδ
```

### Solution (`bothSequential'` using `onlyFirst` and `onlySecond`)

<details>

```lean
def onlyFirst
    [Functional program]
    [Creational program]
    [Sequential program] :
  program α β → program (α × γ) (β × γ) :=
    λ αpβ => (first >=> αpβ) &&& second
```

```lean
def onlySecond
    [Functional program]
    [Creational program]
    [Sequential program] :
  program γ δ → program (α × γ) (α × δ) :=
    λ γpδ => first &&& (second >=> γpδ)
```

```lean
def bothSequential'
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      onlyFirst αpγ >=> onlySecond βpδ
```

</details>

### Writing programs

Programs are written in terms of members of classes like the ones defined so far. The class members are not defined,
they are declared. Classes are specifications. Programs written in terms of their members are program specifications,
but, by abuse of language we call them programs.

It is challenging to limit the expressiveness of class combinations in terms of whose members programs are written. 

More specification expressivenes implies less implementation flexibility.

### Using `Functorial` with `Functional`, and `Creational`

The combination of `Functional`, `Functorial` and `Creational` is sufficiently expressive to write interesting programs
(recall, program specifications) with. They are more flexible as far as implementation is concerned than the ones using
`Functional`, `Functorial` and `Sequential`.

Below are two programs, `minusOneTimesTwoFunctorial` and `minusOneTimesTwoSequential`.

Let

```lean
def minusOneF : Nat → Nat := λ n => n - 1

def addF : Nat × Nat → Nat := λ ⟨n, m⟩ => n + m
```

and

```lean
def minusOne
    [Functional program] :
  program Nat Nat :=
    asProgram minusOneF

def add
    [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram addF
```

in

```lean
def minusOneTimesTwoFunctorial
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >-> addF

def minusOneTimesTwoSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusOne >=> add
```

`minusOneTimesTwoSequential` uses the more powerful `Sequential` while `minusOneTimesTwoFunctorial` uses the less 
powerful `Functorial`. Using `Sequential` is, in this case, an unnecessary overkill because all programs involved are 
effectfree.

That being said, you may argue that what you have read so far is also an unnecessary overkill because, after all, so
far, we only defined effectfree programs. Think of replacing `minusOne` and `minusTwo` by a effectful programs. Having 
more implementation flexibility when dealing with side effects can really be useful. A standard example is accumulating 
error handling when processing a form. Another example is error correction when parsing a document.

### `def let_`

Using the `let_` combinator an intermediate value can be constructed that is available for later use.

```lean
def let_
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program (α × β) γ → program α γ :=
    λ αpβ αaβpγ => identity &&& αpβ >=> αaβpγ
```

The `a` in `αaβpγ` stands for "and".

Think of `let_` as a library level keyword.

How to read `program α β → program (α × β) γ → program α γ`?

If an initial value of type `α` can be transformed to a final value of type `β`, and, if that final value of type `β`
can, as an intermediate value of type `β`, together with the initial value of type `α` be transformed to a final value
of type `γ`, then an initial value of type `α` can be transformed to a final value of type `γ`.

The statement above does not use the word "function" or the word "program".

Using functions instead of programs (as in `Creational`) the type above can be programmed as below

```lean
def creational_let {α β γ : Type} :
    function α β → function (α × β) γ → function α γ :=
  λ αfβ αaβfγ α =>
    let β := αfβ α
    αaβfγ (α, β)
```

Compare this with how to read `program α β → program α γ → program α γ`.

Using functions instead of programs (as in `Sequential`) the type above can be programmed as below

```lean
def sequential_let {α β γ : Type} :
    function α β → function β γ → function α γ :=
  λ αfβ βfγ α =>
    let β := αfβ α
    βfγ β
```

- In the definition of `creational_let`, the initial value of type `α` is used by `αaβfγ`.
- In the definition of `sequential_let`, the initial value of type `α` is not used by `βfγ`.

Being able to formalize this difference using specifications, in this case `Creational` and `Sequential`, is an example
of gaining progressive insight into what programming is all about. In this case into what "intermediate value creation"
is all about.

### Exercise (`sequentialProduct'` using `let_`)

*Exercise*

Define `sequentialProduct'`, an alternative version of `sequentialProduct`, using `let_`.

*Hint*

Yet another "getting the types right puzzle".

Replace `sorry` in `productSeqExercise'` below.

```lean
def productSeqExercise'
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program α γ → program α (β × γ) :=
    sorry
```

### Solution (`sequentialProduct'` using `let_`)

<details>

```lean
def sequentialProduct'
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program α γ → program α (β × γ) :=
    λ αpβ αpγ =>
      let_ αpβ $
        let_ (first >=> αpγ) $
          asProgram λ ((_, β), γ) => (β, γ)
```

</details>

### `class Conditional`

Programs can be combined to consume sum values. Only the left one and its side effects or the right one and its side 
effects is used. We define programs that consume sum values in a formal way using `class Conditional`.

```lean
class Conditional
    (program : Type → Type → Type) where
  sum {α β γ : Type} :
    program γ α → program β α → program (γ ⊕ β) α

export Conditional (sum)

infixl:55 " ||| " => sum
```

```

        ╭──────────╮
      γ |  ╭───╮   |
    >───┼──|γpα|─╮ |
        |  ╰───╯ | | α
      ⊕ |        ┼─┼───>
        |  ╭───╮ | |
    >───┼──|βpα|─╯ |
      β |  ╰───╯   |
        ╰──────────╯
```

`sum` also has infix notation `|||`.

### `def if_`

Using the `if_` combinator conditional boolean logic can be expressed.

```lean
def if_
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program α Bool →
  program α β →
  program α β →
  program α β :=
    λ αpb t_apβ f_apβ =>
      let_ αpb $
        in_ $
          asProgram (
            λ αab => match αab with
              | ⟨α, true⟩ => .inl α
              | ⟨α, false⟩ => .inr α
          ) >=>
          t_apβ ||| f_apβ
```

The `b` in `αpb` stands for `Bool`, the `t` in `t_ap` stands for `true`, and the `f` in `f_ap` stands for `false`.

Think of `if_` as a library level keyword.

How to read `program α Bool → program α β → program α β → program α β`?

If there are two ways to transform an initial value of type `α` to a final value of type `β` available, then, 
depending on the final Boolean value the initial value is transformed to, one of both ways is chosen to transform the
initial value to a final value, obtaining a way to transform an initial value of type `α` to a final value of type `β`.

The statement above does not use the word "function" or the word "program".

Using functions instead of programs (as in `Conditional`) the type above can be programmed as below

```lean
def conditional_if :
  function α Bool →
  function α β →
  function α β →
  function α β :=
    λ αfb t_afβ f_afβ α =>
      if αfb α
        then t_afβ α
        else f_afβ α
```

### Exercise (`sum'` using `if_`)

*Exercise*

Define `sum'`, an alternative version of `sum`, using `if_`.

*Hint*

Yet another "getting the types right puzzle".

Replace `sorry` in `sumExercise'` below.

When matching with a value of type `γ ⊕ β` You may need to use `sorry` (think of it as undefined).

```lean
def sum'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program γ α → program β α → program (γ ⊕ β) α :=
    sorry
```

### Solution (`sum'` using `if_`)

<details>

```lean
def sum'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program γ α → program β α → program (γ ⊕ β) α :=
    λ γpα βpα =>
      if_ (asProgram
            (λ γoβ => match γoβ with
              | .inl _ => True
              | .inr _ => False))
        (asProgram
          (λ γoβ => match γoβ with
            | .inl γ => γ
            | .inr β => sorry) >=> γpα) $
        asProgram
          (λ γoβ => match γoβ with
            | .inl _ => sorry
            | .inr β => β) >=> βpα
```

The `o` in `γoβ` stands for "or".

</details>

### Mathematics related remark

Let's, from a mathematics point of view, have a look at the members of the classes defined so far.

Let's start with 

- `sequentialProduct : program α β → program α γ → program α (β × γ)`
and
- `sum : program γ α → program β α → program (γ ⊕ β) α`

Polynomials, combinations of products (`×`) and sums (`⊕`), are part of the mathematical foundation of programming. 

Let's continue with

- `asProgram : function α β → program α β`

Exponentials (`→`) are part of the mathematical foundation of programming.

Let's end with

- `functionAction : function β γ → (program α β → program α γ)`
and
- `programAction : program β γ → (program α β → program α γ)`

and correponding derived definitions

Transformations, actions transforming programs to programs, are part of the mathematical foundation of programming.

### `class Positional`

```lean
class Positional
    (program : Type → Type → Type) where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ

export Positional (at_)

infixl:45 " @ " => at_
```

`at_` also has infix notation `@`.

The `σ` stands for "(runtime) stack", and `σ × β` stands for `β` pushed onto `σ`.

### Some positions

Let's define some positions on a runtime stack `σ`.

```lean
def positionOne
    [Functional program] :
  program (σ × α) α :=
    asProgram  λ (_, α) => α

def positionTwo
    [Functional program] :
  program ((σ × β) × α) β :=
    asProgram λ ((_, β), _) => β

def positionOneAndTwo
    [Functional program] :
  program ((σ × β) × α) (α × β) :=
    asProgram λ ((_, β), α) => (α, β)

-- ...
```

`positionOne` and `positionTwo` are basic-value positions. `positionOneAndTwo` is a composite-value position.

### `instance Positional`

The `at_` library level keyword of `Positional` can be defined using `Functional`, `Sequential` and `Creational`.

```lean
instance
    [Functional program]
    [Sequential program]
    [Creational program] :
    Positional program where
  at_ {σ α β γ : Type} :
    program α β →
    program σ α →
    program (σ × β) γ →
    program σ γ :=
      λ αpβ σpα =>
        let_ (σpα >=> αpβ)
```

Think of `σpα` as accessing a (composite-)value, `α`, on a runtime stack, `σ`. Think of `αpβ` as transforming that
(composite-)value to `β`. `let_` then pushes `β` on `σ` obtaining a runtime stack`σ × β`. 

`at_` states that, if it possible to transform a value `α`, accessed on the runtime stack `σ`, to an intermediate value
`β`, and to transform `σ × β` to `γ`, then it is possible to transform the runtime stack `σ` to `γ`.

### `minusTwoPositional`

Below is a positional program, `minusTwoPositional`. It uses only uses `positionOne`. For the first `minusOne` it is the 
position of the initial value that is pushed on the runtime stack. For the second `minusOne` it is the position of the 
final value of `minusOne` that is pushed on the runtime stack. 

```lean
def minusTwoPositional
    [Functional program]
    [Sequential program]
    [Creational program] :
  program (σ × Nat) Nat :=
    minusOne @ positionOne $
      minusOne @ positionOne $
        positionOne
```

Runtime stack positions go up starting from their use site.

`Positional` is not part of the list of required class instances because it can be inferred.

### `minusOneTimesTwoPositional`

Below is a positional program, `minusOneTimesTwoPositional`. It uses three positions. For the first `minusOne`
it is the position of the initial value that is pushed on the runtime stack. For the second `minusOne` it is also the 
position of the initial value that is pushed on the runtime stack, for `add` it is a composite-value consisting of the 
position of the final value of the first `minusOne` that is pushed on the runtime stack and the position of the final 
value of the second `minusOne` that is pushed on the runtime stack

```lean
def minusOneTimesTwoPositional
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program (σ × Nat) Nat :=
    minusOne @ positionOne $
      minusOne @ positionTwo $
        add @ positionOneAndTwo $
          positionOne
```

`Positional` is not part of the list of required class instances because it can be inferred.

### Showing the runtime stack

It is instructive to show the runtime stack using `identity`. This is done in `stackShowingMinusTwoPositional` and
`stackShowingTwiceMinusOnePositional` below.

```lean
def stackShowingMinusTwoPositional
    [Functional program]
    [Creational program]
    [Sequential program] :
  program (σ × Nat) (((σ × Nat) × Nat) × Nat) :=
    minusOne @ positionOne $
      minusOne @ positionOne $
        identity
```

```lean
def stackShowingTwiceMinusOnePositional
    [Functional program]
    [Creational program]
    [Sequential program] :
  program (σ × Nat) ((((σ × Nat) × Nat) × Nat) × Nat) :=
    minusOne @ positionOne $
      minusOne @ positionTwo $
        add @ positionOneAndTwo $
          identity
```

### Exercise (extra primitive programs)

*Exercise*

Define extra `Nat` type based programs `isZero`, `isOne`, `one`, `minusTwo`, and `multiply`.

Program `one` is the constant `1` function, used as a program,

### Solution (extra primitive programs)

<details>

Primitive functions

```lean
def isZeroF: Nat → Bool := (. == 0)

def isOneF : Nat → Bool := (. == 1)

def oneF : Nat → Nat := λ _ => 1

def minusTwoF : Nat → Nat := λ n => n - 2

def multiplyF : Nat × Nat → Nat := λ ⟨n, m⟩ => n * m
```

Some of the primitive functions use operation sections.

Primitive programs

```lean
def isZero
    [Functional program] :
  program Nat Bool :=
    asProgram isZeroF

def isOne
    [Functional program] :
  program Nat Bool :=
    asProgram isOneF

def one
    [Functional program] :
  program Nat Nat :=
    asProgram oneF

def minusTwo
    [Functional program] :
  program Nat Nat :=
    asProgram minusTwoF

def multiply
    [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram multiplyF
```

</details>

### `fibonacci`

Program `fibonacci` is defined as follows

```lean
unsafe def fibonacci
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      if_ isOne one $
        (minusTwo >=> fibonacci) &&&
        (minusOne >=> fibonacci) >=>
        add
```

### `factorial`

Program `factorial` is defined as follows

```lean
unsafe def factorial
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      (identity) &&&
      (minusOne >=> factorial) >=>
      multiply
```

### Exercise `factorial'`

*Exercise*

Define `factorial'` using `let_`.

### Solution `factorial'`

<details>

```lean
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
```

</details>

### About `unsafe`

`Lean` cannot infer that `fibonacci` and `factorial` can be used in a safe way. We used the `unsafe`, but, maybe, there
are more appropriate ways to solve this issue. Note that `fibonacci` and `factorial` are program specifications. They
need to be materialized before they can be used. Much in the same way, specifications of side effects need to be
materialized before they can be used. 

It is instructive to compare this with a painting of something going wrong. It is safe to hang the painting on your   
wall. Nothing will go wrong.

### `class Parallel`

Programs can be combined to, in parallel, to transform initial product values to final product values. The order in
which values are transformed is not guarantied. More important, also the order in which their side effects are performed
is not guarantied. We define programs that, in parallel, transform initial product values to final product values in a
formal way by defining `class Parallel`.

```lean
class Parallel (program : Type → Type → Type) where
  bothParallel {α β γ δ : Type} :
  program α γ → program β δ → program (α × β) (γ × δ)

export Parallel (bothParallel)

infixl:60 " |&| " => bothParallel
```

```
            ╭─────────╮
          α |  ╭───╮  | γ
        >───┼──|αpγ|──┼───>
            |  ╰───╯  |
          × |         │ ×
            |  ╭───╮  |
        >───┼──|βpδ|──┼───>
          β |  ╰───╯  | δ
            ╰─────────╯
```

`bothParallel` also has infix notation `|&|`.

### Exercise (`parallelProduct`)

*Exercise*

Define `parallelProduct` (and corresponding `infixl:60 " &|& " `) using `bothParallel` (infix `|&|`).

*Hint*

Replace `sorry` in `parallelProduct` below.

```lean
def parallelProduct {α β γ : Type}
    [Functional program]
    [Sequential program]
    [Parallel program] :
  program α β → program α γ → program α (β × γ) :=
   sorry
```

*Hint*

You will need an appropriate generic primitive program.

### Solution (`parallelProduct`)

<details>

Let

```lean
def duplicate
    [Functional program] :
  program α (α × α) :=
    asProgram λ α => (α, α)
```

in

```lean
def parallelProduct {α β γ : Type}
    [Functional program]
    [Sequential program]
    [Parallel program] :
  program α β → program α γ → program α (β × γ) :=
   λ αpβ αpγ => dup >=> αpβ |&| αpγ

infixl:60 " &|& " => parallelProduct
```

</details>


### `parallelFibonacci`

Program `parallelFibonacci` is defined as follows

```lean
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
```

The only difference between `fibonacci` and `parallelFibonacci` is that `fibonacci` uses the sequential product 
operation `&&&` while `parallelFibonacci` uses the parallel product operation `&|&`.

### About `$`

You may have questions about the usage of the `$` operation. It is an operation with low precedence that avoids using
parentheses (`(` and `)`). Of course it is also possible to use nested parentheses as in `fibonacci'`,

```lean
unsafe def fibonacci'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one (
      if_ isOne one (
        (minusOne >=> fibonacci') &&&
        (minusTwo >=> fibonacci') >=>
        add
      )
    )
```

and as in `factorial''` (there is no need to nest parentheses for `factorial''`).

```lean
unsafe def factorial''
    [Functional program]
    [Sequential program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one (
      let_ (minusOne >=> factorial'')
        multiply
    )
```

It is all a matter of taste.

### Types don't tell everything

Consider the four basic members or derived deinitions below

- `sequentialProduct {α β γ : Type} : program α β → program α γ → program α (β × γ)`
- `bothSequential {α β γ : Type} : program α γ → program β δ → program (α × β) (γ × δ)`

and

- `parallelProduct {α β γ : Type} : program α β → program α γ → program α (β × γ)`
- `bothParallel {α β γ δ : Type} : program α γ → program β δ → program (α × β) (γ × δ)`

The types of `sequentialProduct` and `parallelProduct` resp. `bothSequential` and `bothParallel` are the same.

There is a choice to be made now.

1. Should we treat the difference between the `Sequential` and `Parallel` versions as a specification concern?
2. Should we treat the difference between the `Sequential` and `Parallel` versions as an implementation concern?

`PSBP` goes for choice 1. , if only because the order in which side effects are performed matter.

## Programming With State

### `class WithState σ`

`PSBP` enables programming with state using the `WithState` class below.

```lean
class WithState
    (σ : outParam Type)
    (program : Type → Type → Type) where
  readState {α : Type} : program α σ
  writeState : program σ Unit

export WithState (readState writeState)
```

The `σ` above stands for state.

### `def modifyStateWithFunction`

Below is `modifyStateWithFunction`, a useful programming capability.

```lean
def modifyStateWithFunction
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  function σ σ → program α α :=
    λ σfσ =>
      let_ ((readState >=> asProgram σfσ) >=> writeState) $
        first
```

`modifyStateWithFunction` modifies the state using a function `σfσ : function σ σ`.

### Exercise (`modifyStateWithProgram`)

*Exercise*

Define `modifyStateWithProgram`.

### Solution (`modifyStateWithProgram`)

```lean
def modifyStateWithProgram
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  program σ σ → program α α :=
    λ σpσ =>
      let_ ((readState >=> σpσ) >=> writeState) $
        first
```

### `usingInitialStateAsInitialValue`

```lean
def usingInitialStateAsInitialValue
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithState σ program] :
  program σ β → program α β :=
    λ σpβ =>
      readState >=> σpβ
```

`usingInitialStateAsInitialValue` uses the initial state as initial value.

### `fibonacciWithStatePair`

`fibonacciWithStatePair` below uses an initial state as initial value and modifies that state by incrementing it.

Let

```lean
unsafe def fibonacciWithState
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit Nat :=
    usingInitialStateAsInitialValue fibonacci >=>
    modifyStateWithFunction (λ σ => σ + 1)
```

in

```lean
unsafe def fibonacciWithStatePair
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit (Nat × Nat) :=
    fibonacciWithState &&& fibonacciWithState
```

## Programming With Failure

### `WithFailure ε`

`PSBP` enables programming with failure using the `WithFailure` class below.

```lean
class WithFailure
    (ε : outParam Type)
    (program : Type → Type →Type) where
  failWithFunction {α β : Type} : function α ε → program α β

export WithFailure (failWithFunction)
```

Failure is, typically, either handled by only keeping the first failure encountered (fast failing), or handled by
accumulating all failures encountered (validating).

We define accumulation in a formal way using `class Monoid`.

```lean
class Monoid (μ : Type) where
  ν : μ
  combine : μ → μ → μ

export Monoid (ν combine)

infixl:60 " * " => combine
```

For accumulating all failures encountered the neutral element `ν` is not used, this means that only a semigroup is
required instead of a monoid.

### `addedDivPairWithStringFailure` and `addedDivPairWithStringListFailure`

`addedDivPairWithStringFailure` will illustrate adding two intermediate division values, fast failing with a string.

`addedDivPairWithStringListFailure` will illustrate adding two intermediate division values, validating with a list of
strings.

There is a choice to be made now.

1. Should we treat the difference between fast failing and validating as a specification concern?
2. Should we treat the difference between fast failing and validating as an implementation concern?

`PSBP` goes for choice 2. 

Implementations with a type `ε` that is not defined to be a `Monoid` instance are fast failing. Implementations with a
type `ε` that is defined to be a `Monoid` instance are validating. 

Division fails when the denominator of the division is zero. So we start with `unsafeDiv` and `isNotZero`. 

```lean
def unsafeDiv [Functional program] :
  program (Nat × Nat) Nat := asProgram λ ⟨n, m⟩ => n / m
  
def isNotZero [Functional program] :
  program Nat Bool := asProgram (. != 0)
```

We continue with what `addedDivPairWithStringFailure` and `addedDivPairWithStringListFailure` have in common.

```lean
def divWithFailureUsing
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithFailure ε program] :
  function (Nat × Nat) ε →
    program (Nat × Nat) Nat :=
    λ nfε =>
      if_ (second >=> isNotZero) unsafeDiv $
        failWithFunction nfε

def divPairWithFailureUsing
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithFailure ε program] :
  function (Nat × Nat) ε →
    program ((Nat × Nat) × (Nat × Nat)) (Nat × Nat) :=
    λ nfε =>
      let safeDiv := divWithFailureUsing nfε
      (first >=> safeDiv) &&& (second >=>
      safeDiv)

def addedDivPairWithFailureUsing
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithFailure ε program] :
  function (Nat × Nat) ε →
    program ((Nat × Nat) × (Nat × Nat)) Nat :=
    λ nfε =>
      divProductWithFailureUsing nfε >=>
        add
```

We continue with what `addedDivPairWithStringFailure` and `addedDivPairWithStringListFailure` do not have in common.

```lean
def addedDivPairWithStringFailure
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    addedDivPairWithFailureUsing
      λ (n, m) => s!"{n}/{m}"

def addedDivPairWithStringListFailure
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    addedDivPairWithFailureUsing
      λ (n, m) => [s!"{n}/{m}"]
```

We end with the `Monoid` instance that will make `addedDivPairWithStringListFailure` validating.

```lean
instance : Monoid (List α) where
  ν := .nil
  combine := .append
```

## Laws

`Lean` shines for it's support for laws and theorems.

### `class LawfulFunctional`

`Functional` comes with laws.

```lean
class LawfulFunctional
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program] : Prop where
  functional_identity :
    (asProgram id : program α α) =
      identity
  functional_sequential
      (αfβ : α → β)
      (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ : program α γ) =
      asProgram (βfγ ∘ αfβ)
```

The `functional_identity` law relates function identity and program identity, and is `True` by definition.

The `functional_sequential` law relates function sequential combination and program sequential combination.

Not that, sometimes, the `Lean` type inference system needs some help with a type annotation.

### `class LawfulFunctorial`

`Functorial` comes with laws.

```lean
class LawfulFunctorial
    (program : Type → Type → Type)
    [Functorial program] : Prop where
  functorial_identity
    (αpβ : program α β) :
    (αpβ >-> id) =
      αpβ
  functorial_sequential
      (αpβ : program α β)
      (βfγ : β → γ)
      (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ) =
      (αpβ >-> (γfδ ∘ βfγ))
```

The `functorial_identity` law states sequential combination of a program with the identity function at leaves the
program intact.

The `functorial_sequential` law relates function sequential combination and function action sequential combination.

### `class LawfulSequential`

`Functorial` comes with laws.

```lean
class LawfulSequential
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program] : Prop where
  sequential_right_identity
      (αpβ : program α β) :
    (αpβ >=> identity) =
      αpβ
  sequential_left_identity
      (αpβ : program α β) :
    (identity >=> αpβ) =
      αpβ
  sequential_associativity
      (αpβ : program α β)
      (βpγ : program β γ)
      (γpδ : program γ δ) :
    ((αpβ >=> βpγ) >=> γpδ) =
      (αpβ >=> (βpγ >=> γpδ))
```

The `sequential_right_identity` law states that the sequential combination of a program with the identity program at
right leaves the program intact.

The `sequential_left_identity` law states that the sequential combination of a program with the identity program at
left leaves the program intact.

The `sequential_associativity` law states that the sequential combination of programs is associative.

### `class LawfulCreational`

`Creational` comes with laws.

They are all using `onlyFirst`. They are the `arrow` laws (`Sequential` is required, `Conditional` is not required).

Let

```lean
def applyAtFirst
    [Functional program] :
  function α β → program (α × γ) (β × γ) :=
    λ αfβ => asProgram λ (α, γ) => (αfβ α, γ)
```

and

```lean
def applyAtSecond
    [Functional program] :
  function β γ → program (α × β) (α × γ) :=
    λ βfγ => asProgram λ (α, β) => (α, βfγ β)
```

in

```lean
def assoc
    [Functional program] :
  program ((α × β) × γ) (α × (β × γ)) :=
    asProgram λ ((a, b), c) => (a, (b, c))
```

```lean
class LawfulCreational
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_onlyFirst_asProgram
      (αfβ : α → β) :
    (onlyFirst (asProgram αfβ)
      : program (α × γ) (β × γ)) =
      applyAtFirst αfβ
  creational_onlyFirst_sequential
      (αpβ : program α β)
      (βpγ : program β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      program (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ)
  creational_onlyFirst_first
      (αpβ : program α β) :
    (onlyFirst αpβ >=> (first : program (β × γ) β)) =
      ((first : program (α × γ) α) >=> αpβ)
  creational_onlyFirst_applyAtSecond
      (αpβ : program α β)
      (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ) =
      (applyAtSecond γfδ >=> onlyFirst αpβ)
  creational_onlyFirst_assoc
      (αpβ : program α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc
      : program ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ)
```

### `class LawfulConditional`

`Conditional` comes with laws.

Let

```lean
def left
    [Functional program] :
  program γ (γ ⊕ β) :=
    asProgram .inl
```

and

```lean
def right
    [Functional program] :
  program β (γ ⊕ β) :=
    asProgram .inr
```

in

```lean
class LawfulConditional
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Conditional program] : Prop where
  conditional_left
      (γpα : program γ α)
      (βpα : program β α) :
    (left >=> γpα ||| βpα) =
      γpα
  conditional_right
      (γpα : program γ α)
      (βpα : program β α) :
    (right >=> γpα ||| βpα) =
      βpα
```

### Exercise (alternative `functorial_sequential` law)

*Exercise*

Define an alternativce `functorial_sequential`, not using `αpβ`, but using `>->` operation sections instead.

*Hint*

You may need to help the `Lean` type inference system with a type annotation.

<details>

```lean
class AlternativeLawfulFunctorial
    (program : Type → Type → Type)
    [Functorial program] : Prop where
  functorial_sequential
      (βfγ : β → γ)
      (γfδ : γ → δ) :
    (((. >-> γfδ) ∘ (. >-> βfγ)) :
      program α β → program α δ) =
     (. >-> (γfδ ∘ βfγ))
```

</details>

### Exercise (extra `Creational` law)

*Exercise*

Replace `sorry` in `extraLawfulCreationalExercise` below.

```lean
class ExtraLawfulCreationalExercise
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_productSeq
      (αpβ : program α β)
      (αpγ : program α γ)
      (βfδ : β → δ)
      (γfε : γ → ε) :
    (αpβ &&& αpγ >=> (asProgram βfδ <&> asProgram γfε)) =
      sorry
```

### Solution (extra `Creational` law)

<details>

```lean
class ExtraLawfulCreational
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_productSeq
      (αpβ : program α β)
      (αpγ : program α γ)
      (βfδ : β → δ)
      (γfε : γ → ε) :
    (αpβ &&& αpγ >=> (asProgram βfδ <&> asProgram γfε)) =
      ((αpβ >=> asProgram βfδ) &&& (αpγ >=> asProgram γfε))
```

</details>

### Exercise (extra `Creational` law question)

*Exercise*

Is the following law a reasonable one (open question)?

```lean
class ExtraLawfulCreationalQuestion
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_productSeq
      (αpβ : program α β)
      (αpγ : program α γ)
      (βpδ : program β δ)
      (γpε : program γ ε) :
    (αpβ &&& αpγ >=> (βpδ <&> γpε)) =
      ((αpβ >=> βpδ) &&& (αpγ >=> γpε))
```

### Solution (extra `Creational` law question)

<details>

The order of the side effects performed by the programs involved matters. Therefore, because of it's distributive
nature, this law is unlikely to be a valid one for most implementations.

In fact, for the computation valued function implementation (see next section).

- the order of the left hand side is `αpβ`, `αpγ`, `βpδ`, `γpε`
- the order of the right hand side is `αpβ`, `βpδ`, `αpγ`,`γpε`

</details>

### Exercise (extra `Creational` law for `let_`)

*Exercise*

Replace `sorry` in `lawfulCreationalLetExercise` below.

Is the law a reasonable one (open question)?

```lean
class LawfulCreationalLetExercise
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_let_sequential
      (αpβ : program α β)
      (αaβpγ : program (α × β) γ)
      (γpδ : program γ δ) :
    ((let_ αpβ αaβpγ) >=> γpδ) =
      sorry
```

### Solution (extra `Creational` law for `let_`)

<details>

```lean
class LawfulCreationalLet
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program] : Prop where
  creational_let_sequential
      (αpβ : program α β)
      (αaβpγ : program (α × β) γ)
      (γpδ : program γ δ) :
    ((let_ αpβ αaβpγ) >=> γpδ) =
      (let_ αpβ (αaβpγ >=> γpδ))
```

The order of the side effects performed by the programs involved matters. In this case it's sequential nature does not
matter. Therefore this law is likely to be a valid one for most implementations.

</details>

### Exercise (extra `Conditional` law for `if_`)

*Exercise*

Replace `sorry` in `LawfulCreationalIfExercise` below.

```lean
class LawfulCreationalIfExercise
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] : Prop where
  conditional_if_sequential
      (αpb : program α Bool)
      (t_apβ : program α β)
      (f_apβ : program α β)
      (βpγ: program β γ) :
    ((if_ αpb t_apβ f_apβ) >=> βpγ) =
      sorry
```

Is the law a reasonable one (open question)?

### Solution (extra `Conditional` law for `if_`)

<details>

```lean
class LawfulCreationalIfExercise
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] : Prop where
  conditional_if_sequential
      (αpb : program α Bool)
      (t_apβ : program α β)
      (f_apβ : program α β)
      (βpγ: program β γ) :
    ((if_ αpb t_apβ f_apβ) >=> βpγ) =
      ((if_ αpb (t_apβ >=> βpγ) (f_apβ >=> βpγ)))
```

</details>

The order of the side effects performed by the programs involved matters. In this case it's sequential nature does not 
matter. Therefore this law is likely to be a valid one for most implementations.

### `class LawfulWithState`

```lean
class LawfulWithState
    (program : Type → Type → Type)
    [Functional program]
    [Sequential program]
    [WithState σ program] : Prop where
  withState_write_read :
    ((writeState : program σ Unit) >=> readState) =
      identity
```

Writing a value as current state and then reading the current state yields that value.

## Computation Valued Functions

### `abbrev computationValuedFunction`

Using computation valued functions is a generic way to implement programming related classes in terms of the computing
related classes.

```lean
abbrev computationValuedFunction
    (computation : Type → Type) (α β : Type) :=
  function α (computation β)
```

### `instance Functional`

```lean
instance
    [Applicative computation] :
  Functional
    (computationValuedFunction computation) where
    asProgram :=
      λ αfβ α => pure $ αfβ α
```

### `instance Functorial`

```lean
instance
    [Functor computation] :
  Functorial
    (computationValuedFunction computation) where
    functionAction :=
      λ βfγ =>
        λ αfcβ α => βfγ <$> αfcβ α
```

### `instance Sequential`

```lean
instance
    [Monad computation] :
  Sequential
    (computationValuedFunction computation) where
    andThenProgram :=
      λ αfcβ βfcγ α =>
        αfcβ α >>= βfcγ
```

### `instance Creational`

```lean
instance
    [Applicative computation] :
  Creational
    (computationValuedFunction computation) where
  sequentialProduct :=
    λ αfcβ αfcγ α =>
      .mk <$> αfcβ α <*> αfcγ α
```

### `instance Conditional`

```lean
def foldSum {γ β α : Type}
    (γfα : γ → α)
    (βfα : β → α)
    (sum : γ ⊕ β) : α :=
  match sum with
  | .inl tc => γfα tc
  | .inr tb => βfα tb

instance :
  Conditional
    (computationValuedFunction computation) where
    sum :=
      λ γfγα βfγα =>
        foldSum γfγα βfγα
```

### `instance Parallel`

`Parallel` is implemented using the computing related class `MonadAsync`.

```lean
class MonadAsync
    (computation : Type → Type) where
  async {α : Type} (ufα : Unit → α) : computation α

export MonadAsync (async)

instance
    [Monad computation]
    [MonadAsync computation] :
  Parallel
    (computationValuedFunction computation) where
    bothParallel :=
      λ αfcγ βfcδ ⟨α, β⟩ =>
        async (λ (_: Unit) => αfcγ α) >>=
          λ cγ => async (λ (_: Unit) => βfcδ β) >>=
            λ cδ => .mk <$> cγ <*> cδ
```

The `u` in `ufα` stands for `Unit`.

### `instance WithState σ`

`WithState σ` is implemented using the standard computing related class `MonadStateOf`.

```lean
instance
    [MonadStateOf σ computation] :
  WithState σ
    (computationValuedFunction computation) where
    readState := λ _ => getThe σ
    writeState := set
```

### `instance WithFailure ε`

First we define the computation transformation class `FailureT`.

```lean
abbrev FailureT
    (ε : Type)
    (computation : Type → Type)
    (β : Type) : Type :=
  computation (ε ⊕ β)
```

For fast failing we define a `Monad` instance.

```lean
instance
    [Monad computation] :
  Monad (FailureT ε computation) where
    map {α β : Type} :
      (α → β) →
      (computation (ε ⊕ α) → computation (ε ⊕ β)) :=
        λ αfβ cεoα =>
          cεoα >>= λ (εoα : ε ⊕ α) => match εoα with
            | .inr α => pure $ .inr (αfβ α)
            | .inl ε => pure $ .inl ε
    pure {α : Type} :
      α → computation (ε ⊕ α) :=
        λ α =>
          pure $ .inr α
    bind {α β : Type} :
        (computation (ε ⊕ α) →
          (α → computation (ε ⊕ β)) →
          computation (ε ⊕ β)) :=
      λ cεoα αfftεcβ =>
        cεoα >>= λ εoα => match εoα with
          | .inr α  => αfftεcβ α
          | .inl ε  => pure $ .inl ε
```

For validating we define a `Monoid` based `Applicative` instance.

```lean
instance
    [Functor computation] :
  Functor (FailureT ε computation) where
    map {α β : Type} :
      (α → β) →
      (computation (ε ⊕ α) → computation (ε ⊕ β)) :=
      λ αfβ cεoα =>
        (λ εoα =>
          match εoα with
            | .inl ε => .inl ε
            | .inr α => .inr (αfβ α)) <$> cεoα

instance
    [Applicative computation]
    [Monoid ε] :
  Applicative (FailureT ε computation) where
    pure {α : Type} :
      α → computation (ε ⊕ α) :=
      λ α =>
        pure $ .inr α
    seq {α β : Type} :
      (computation (ε ⊕ (α → β))) →
      ((Unit → computation (ε ⊕ α)) →
      computation (ε ⊕ β)) :=
      λ cεoαfβ ufftεcα =>
        let cεoα :=
          (ufftεcα ())
        let εoαfεoαfβfεoβ {α β : Type} :
          (ε ⊕ α) → (ε ⊕ (α → β)) → (ε ⊕ β) :=
            λ εoα εoαfβ =>
              match εoα with
                | .inl ε =>
                  match εoαfβ with
                    | .inr _  => .inl ε
                    | .inl ε' => .inl (ε' * ε)
                | .inr α =>
                  match εoαfβ with
                    | .inr αfβ  => .inr (αfβ α)
                    | .inl ε' => .inl ε'
        εoαfεoαfβfεoβ <$> cεoα <*> cεoαfβ
```

`WithFailure ε` is implemented using `FailureT`.

```lean
instance
    [Applicative computation] :
  WithFailure ε
    (computationValuedFunction
      (FailureT ε computation)) where
    failWithFunction {α β : Type} :
      function α ε →
      computationValuedFunction computation α (ε ⊕ β) :=
      λ αfε α =>
        pure $ .inl $ αfε α
```

## Theorems

The laws of the various classes need to be proved for the various instances.

First we prove them using `calc` and next we let `Lean` prove them for us using `by simp`.

### `Functional` theorems

Theorem `functional_identity` below is proved by definition using `calc` and
`rfl`.

```lean
@[simp] theorem functional_identity
  {α : Type}
    [Applicative computation] :
    (identity :
      computationValuedFunction computation α α)
      = asProgram id :=
  calc
    identity
        = asProgram id
          := rfl
```
Theorem `functional_sequential'` also uses `congrArg` and `funext`.

Theorem `functional_sequential'` also uses the `pure_bind` law of `LawfulMonad`.

```lean
theorem functional_sequential'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : function α β)
  (βfγ : function β γ) :
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) :=
  calc
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
    _   = λ α => (pure $ αfβ α) >>= λ β => pure $ βfγ β
          := rfl
    _   = λ α => pure $ βfγ (αfβ α)
          := funext λ α =>
               pure_bind (αfβ α) (λ β => pure $ βfγ β)
    _   = (asProgram (βfγ ∘ αfβ) :
            computationValuedFunction computation α γ)
          := rfl
```

Theorem `functional_sequential` uses `by simp` to let `Lean` do the heavy lifting.

```lean
@[simp] theorem functional_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : function α β)
  (βfγ : function β γ) :
    (asProgram αfβ >=> asProgram βfγ :
      computationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) := by
  simp[asProgram, andThenProgram]
```

It is necessary the unfold the definitions involved.

Note that `functional_sequential'` is not annotated by `@[simp]` so that `functional_sequential` cannot use it. 

### `Functorial` theorems

Theorem `functorial_identity'` uses the `id_map` law of `LawfulFunctor`.

```lean
theorem functorial_identity'
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >-> id)
      = αpβ :=
  calc
    (αpβ >-> id)
        = λ α => id <$> αpβ α
          := rfl
    _   = λ α => αpβ α
          := funext λ α => id_map (αpβ α)
    _   = αpβ
          := rfl
```

Theorem `functorial_identity` uses `by simp` to let `Lean` do the heavy lifting.

```lean
@[simp] theorem functorial_identity
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >-> id)
      = αpβ := by
  simp[functionAction, andThenFunction]
```

Theorem `functorial_sequential'` uses the `id_map` law of `LawfulFunctor`.

```lean
theorem functorial_sequential'
  {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : computationValuedFunction computation α β)
  (βfγ : function β γ)
  (γfδ : function γ δ) :
    ((αpβ >-> βfγ) >-> γfδ)
      = (αpβ >-> (γfδ ∘ βfγ)) :=
  calc
    (αpβ >-> βfγ) >-> γfδ
    _   = λ α => γfδ <$> βfγ <$> αpβ α
          := rfl
    _   = λ α => (γfδ ∘ βfγ) <$> αpβ α
          := funext λ α =>
               Eq.symm (comp_map βfγ γfδ (αpβ α))
    _   = (αpβ >-> (γfδ ∘ βfγ))
          := rfl
```

Theorem `functorial_sequential` uses `by simp` to let `Lean` do the heavy
lifting.

```lean
@[simp] theorem functorial_sequential
  {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : computationValuedFunction computation α β)
  (βfγ : function β γ)
  (γfδ : function γ δ) :
    ((αpβ >-> βfγ) >-> γfδ)
      = (αpβ >-> (γfδ ∘ βfγ)) := by
  simp[functionAction, andThenFunction, comp_map]
```

### `Sequential` theorems

Theorem `sequential_right_identity'` uses the `bind_pure_comp` law of `LawfulMonad` and the `comp_map` law of
`LawfulFunctor`.

```lean
theorem sequential_right_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >=> asProgram id)
      = αpβ :=
    calc
      (αpβ >=> asProgram id)
          = λ α => αpβ α >>= λ β => pure (id β)
            := rfl
      _   = λ α => id <$> αpβ α
            := funext λ α => bind_pure_comp id (αpβ α)
      _   = λ α => αpβ α
            := funext λ α => id_map (αpβ α)
      _   = αpβ
            := rfl
```

Theorem `sequential_right_identity` uses `by simp` to let `Lean` do the heavy lifting.

```lean
@[simp] theorem sequential_right_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (αpβ >=> asProgram id)
      = αpβ := by
  simp[andThenProgram, asProgram]
```

Theorem `sequential_left_identity'` uses the `pure_bind` law of `LawfulMonad`.

```lean
theorem sequential_left_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (asProgram id >=> αpβ)
      = αpβ :=
  calc
    (asProgram id >=> αpβ)
        = λ α => pure α >>= αpβ
          := rfl
    _   = λ α => αpβ α
          := (funext λ α => pure_bind α αpβ)
    _   = αpβ
          := rfl
```

Theorem `sequential_left_identity` uses `by simp` to let `Lean` do the heavy lifting.

```lean
@[simp] theorem sequential_left_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (asProgram id >=> αpβ)
    = αpβ := by
  simp[asProgram, andThenProgram]
```

Theorem `sequential_associative'` uses the `pure_assoc` law of `LawfulMonad`.

```lean
theorem sequential_associative'
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ)
  (γpδ : computationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ
       : computationValuedFunction computation α δ) =
    (αpβ >=> (βpγ >=> γpδ)) :=
  calc
    ((αpβ >=> βpγ) >=> γpδ
       : computationValuedFunction computation α δ)
       = ((λ α => αpβ α >>= βpγ) >=> γpδ)
         := rfl
    _   = λ α => αpβ α >>= (λ β => βpγ β >>= γpδ)
          := funext λ α => bind_assoc (αpβ α) βpγ γpδ
    _   = (λ α => αpβ α >>= (βpγ >=> γpδ))
          := rfl
    _   = (αpβ >=> (βpγ >=> γpδ))
          := rfl
```

Theorem `sequential_associative` uses `by simp` to let `Lean` do the heavy lifting.

```lean
@[simp] theorem sequential_associative
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ)
  (γpδ : computationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ) =
      (αpβ >=> (βpγ >=> γpδ)) := by
  simp[andThenProgram]
```

### `Creational` theorems

By now you probably agree that `calc` based proofs can become tedious.

In what follows, I will, mostly, only show the `by simp` based proofs.

```lean
@[simp] theorem creational_onlyFirst_asProgram
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β) :
    (onlyFirst (asProgram αfβ) :
      computationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by
    simp [ onlyFirst, first, andThenProgram, asProgram,
    sequentialProduct, second]
```

```lean
@[simp] theorem creational_onlyFirst_sequential
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (βpγ : computationValuedFunction computation β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      computationValuedFunction
        computation (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ :
        computationValuedFunction
          computation (α × δ) (γ × δ)) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second]
```

```lean
@[simp] theorem creational_onlyFirst_first
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (onlyFirst αpβ >=> first :
      computationValuedFunction computation (α × γ) β) =
      (first >=> αpβ) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second]
```

```lean
@[simp] theorem creational_onlyFirst_applyAtSecond
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β)
  (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ) =
      (applyAtSecond γfδ >=> onlyFirst αpβ) := by
  simp[onlyFirst, andThenProgram, applyAtSecond, asProgram,
  sequentialProduct, first, second]
```

```lean
@[simp] theorem creational_onlyFirst_assoc
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : computationValuedFunction computation α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc :
      computationValuedFunction
        computation ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ) := by
  simp[onlyFirst, andThenProgram, asProgram,
  sequentialProduct, first, second, assoc]
```

### `Conditional` theorems

```lean
@[simp] theorem conditional_left
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (γpα : computationValuedFunction computation γ α)
  (βpα : computationValuedFunction computation β α) :
    (left >=> γpα ||| βpα
      : computationValuedFunction computation γ α) =
      γpα := by
  simp[left, asProgram, andThenProgram, sum, foldSum]
```

```lean
@[simp] theorem conditional_right
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (γpα : computationValuedFunction computation γ α)
  (βpα : computationValuedFunction computation β α) :
    (right >=> γpα ||| βpα
      : computationValuedFunction computation β α) =
      βpα := by
  simp[right, asProgram, andThenProgram, sum, foldSum]
```

### `WithState` theorems

`theorem withState_write_read` is proved using the `set_getThe` law of `class LawfulStateOf`.

```lean
class LawfulStateOf (σ : Type) 
    (computation : Type → Type)
    [Monad computation]
    [MonadStateOf σ computation] : Prop where
  set_getThe :
    ((λ s => set s >>= λ _ => getThe σ) :
       σ → computation σ) =
      (pure)

export LawfulStateOf (set_getThe)

attribute [simp] set_getThe
```

in

```lean
@[simp] theorem withState_write_read
    [Monad computation]
    [MonadStateOf σ computation]
    [LawfulStateOf σ computation] :
  ((writeState >=> readState) :
      FromComputationValuedFunction computation σ σ) =
    identity := by simp [andThenP, identity, asProgram]
```

## Materializations

### `syncProgram`

There is no extra work to be done for synchronous computation materializations. The computing related implementations of
`Id` can be used.

```lean
abbrev Sync := Id

abbrev syncProgram :=
  computationValuedFunction Sync

def materializeSync :
  syncProgram α β → function α β :=
    λ αspβ α => αspβ α
```

### `syncReactiveProgram`

There is a bit more work to be done for, `ReactiveT` computation transformation class based, computation
materializations. The `Functor`, `Applicative` and `Monad` instances for `ReactiveT` need to be defined.

Let

```lean
abbrev ReactiveT
    (ρ : Type)
    (computation: Type → Type)
    (α : Type) :=
  (α → computation ρ) → computation ρ

instance :
  Functor (ReactiveT ρ computation) where
    map :=
      λ αfβ rcα γ =>
        rcα (γ ∘ αfβ)

instance :
  Applicative (ReactiveT ρ computation) where
    pure := λ α αfcρ =>
      αfcρ α
    seq :=
      λ rcαfβ ufrtρcα βfcρ =>
        rcαfβ $
          λ αfβ =>
            ufrtρcα () (βfcρ ∘ αfβ)

instance :
  Monad (ReactiveT ρ computation) where
    bind :=
      λ rcα αfrtρcβ βfcρ =>
        rcα λ α => αfrtρcβ α βfcρ

abbrev ReactiveProgram ρ computation :=
  computationValuedFunction (ReactiveT ρ computation)

abbrev syncReactiveProgram ρ :=
  ReactiveProgram ρ Sync

def materializeSyncReactive {α β : Type} :
  (syncReactiveProgram β) α β → function α β :=
    λ αsrpβ α =>
       materializeSync (αsrpβ α) id
```

### `asyncProgram`

There is a bit more work to be done for asynchronous computation materializations. The `Monad` and `MonadAsync` 
instances of `Task` need to be defined.

```lean
abbrev Async := Task

instance : Monad Async where
  pure := Task.pure
  bind := Task.bind

instance : MonadAsync Async where
  async := Task.spawn

abbrev asyncProgram :=
  computationValuedFunction Async

def materializeAsync {α β : Type} :
  asyncProgram α β → function α β :=
    λ αaspβ α =>
      (materializeSync αaspβ α).get
```

### `syncProgramWithState σ`

There is no work to be done for, `StateT σ` computation transformation class based, computation materializations. 

```lean
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
```

### `syncProgramWithFirstFailure ε`

There is no work to be done for, `FailureT ε` computation transformation class based, computation materializations. 

```lean
abbrev programWithFirstFailure ε computation :=
  computationValuedFunction (FailureT ε computation)

def materializeWithFirstFailure
    [Monad computation] {α β : Type} :
  programWithFirstFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ αpwffcβ α =>
      αpwffcβ α

abbrev syncProgramWithFirstFailure ε :=
  programWithFirstFailure ε Sync

def materializeSyncWithFirstFailure {α β : Type} :
 syncProgramWithFirstFailure ε α β → function α (ε ⊕ β) :=
  materializeWithFirstFailure
```

### `syncProgramWithAccumulatedFailure ε`

There is no work to be done for, `FailureT ε` computation transformation class based, computation materializations. 

```lean
abbrev programWithAccumulatedFailure ε computation :=
  computationValuedFunction (FailureT ε computation)

def materializeWithAccumulatedFailure
    [Monad computation]
    [Monoid ε] {α β : Type} :
  programWithAccumulatedFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ αpwafcβ α =>
      αpwafcβ α

abbrev syncProgramWithAccumulatedFailure ε :=
  programWithAccumulatedFailure ε Sync

def materializeSyncWithAccumulatedFailure
    [Monoid ε]
    {α β : Type} :
  syncProgramWithAccumulatedFailure ε α β →
    function α (ε ⊕ β) :=
    materializeWithAccumulatedFailure
```

## Evaluation examples


The `PSBP/README.lean` file contains evaluation examples.



















