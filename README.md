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
[Programs versus Computations](https://github.com/LucDuponcheelAtGitHub/PSBP?tab=readme-ov-file#programs-versus-computations) that come before section
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

The `PSPB` type constructor classes are specifications, also called interfaces in the programming world. They have members that declare basic program specifications and basic program specification combinators to combine program specifications to composite program specifications. Derived program specifications and derived program specification combinators can then be defined in terms of (declared or defined) program specifications and (declared or defined) program specification combinators. As such program specifications are components of a component system.

The `Lean` standard library has a variety of computing related unary type constructor classes, among others `Functor`,
`Applicative` and `Monad`. `PSBP` code consistently names their unary type constructor parameter `computation`.

The `Lean` standard library type constructor classes above are specifications, also called interfaces in the programming world. They have members that declare basic computation specifications and basic computation specification combinators to combine computation specifications to composite computation specifications. Derived computation specifications and derived computation specification combinators can then be defined in terms of (declared or defined) computation specifications and (declared or defined) computation specification combinators. As such computation specifications are
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

In short, formally, and also subjective, because of my progressive insight into what programming, writing code, is all about.

I'll explain this in terms of my personal history as mathematician and programmer (again, you may wish to ignore what follows, not being interested in my personal history).

I am a retired mathematician and programmer.

Mathematics is generally agreed upon to be useful to understand the reality we are part of. For example to understand 
(concepts of) problem domains we are confronted with.

Bridge building engineers may benefit form studying appropriate mathematics to understand what bridge requirements are
all about. Vehicle building engineers may benefit form studying appropriate mathematics to understand what vehicle
requirements are all about. Likewise, programming engineers (programmers) may benefit form studying appropriate mathematics to understand what program requirements are all about.

I used the word "requirements" because separating specifications from implementations is generally agreed upon to be useful to understand problem domains 

A bridge specification states, for example, that it must be able to carry the weight of a number of vehicles. How it is
able to carry that weight is an implementation concern. Maybe one bridge is more pleasing to look at, more durable than,
or less expensive than another one. A vehicle specification states, for example, that it must be able to transport a
number of passengers. How it is able to transport that number of passengers is an implementation concern. Maybe one car is more comfortable than, or less fuel consuming than another one. A program specification states, for example, that a program must be able to create composite data and to perform conditional logic. How it is able to create composite data and to perform conditional logic is an implementation concern. Maybe one program is more CPU effecient, or less RAM consuming than another one.

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

Programming is also about elegance and programming libraries are also about ease of use. Pointfree programming is more elegant than pointful programming and pointfree programming libraries are easier of use than pointful programming libraries. Of course this is a matter of taste.

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
programs can also be looked at as operational artifacts. Just like functions, programs, by, somehow, running them, transform an initial value to a final value, but, somehow running them may perform side effects along the way. The "somehow" in the previous sentence is important, because it depends on the materialization corresponding to instances of the type constructor classes in terms of whose members the programs (recall, more precisely, program specificaton) have been written.

By the way, a value can be an atomic-value or a composite-value, repesented as a (nested) tuple. As such values are
components of a component system.

It is more natural to think denotationally, about "what", than to think operationally, about "how".

Let's try to illustrate this with some `Lean` code fragments.

I use a few naming conventions

- Functions of type `α → computation β` are called `αfcβ`.

- Programs of type `program α β` are called `αpβ`.

I simply use the word "class" instead of "type class", "type constructor class", "unary type constructor class",
"binary type constructor class" and so on ... .

The computing related class `Bind` has a member

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

The programming related class `Sequential` has a member

```lean
andThenP
  {α β γ : Type} :
  program α β → program β γ → program α γ
```

with an associativity law (`>=>` is infix notation for `andThenP`)

```lean
sequential_associativity
  (αpβ : program α β)
  (βpγ : program β γ)
  (γpδ : program γ δ) :
  (αpβ >=> βpγ) >=> γpδ =
    αpβ >=> (βpγ >=> γpδ)
```

Let's first consider syntax.

I can more easily remember the definition of `sequential_associativity` than the definition of `bind_assoc`.

What about you?

Let's next consider semantics.

I can more easily explain `andThenP` and `sequential_associativity` than `bind` and `bind_assoc`.

### Exercise

*Exercise* :

Explain both `andThenP` and `sequential_associativity`, resp. `bind` and `bind_assoc`.

*Hint* : (you may wish to ignore the hint) :

I think of a function as transforming an initial value yielding a final value. Likewise, I think of a program as
transforming an initial value yielding a final value, potentially performing side effects along the way.

I think of evaluating an expression as yielding a value. Likewise, I think of executing a computation as yielding a   
value, potentially performing side effects along the way.

You can, for your explanation, ignore side effects for now. Of course you may wish to explain side effects as well.

### Solution

<details>

Let's first deal with `Sequential`.

`andThenP` can be explained as:

transforming an initial value of type `α`, using a program of type `program α β`, yielding an intermediate value of type
`β`, and then transforming that intermediate value, using a program of type `program β γ`, yielding a final value of
type `γ`.

`sequential_associativity` can be explained as:

first transforming an initial value of type `α`, using `αpβ >=> βpγ`, yielding an intermediate value of type `γ`, and
then transforming that intermediate value, using `γpδ`, yields the same final value as first transforming the initia
value of type `α`, using `αpβ`, to an intermediate value of type `β`, and then transforming that intermediate value,
using `βpγ >=> γpδ`.

Let's second deal with `Bind`.

`bind` can be explained as:

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

It is a more elegant and easier to program pointfree than to program pointful. Likewise, It is more elegant and easier to reason in terms of pointfree laws than to reason in terms of pointful laws. Of course, this is a matter of taste. Nevertheless I hope to convince you.

### Positional Programming

It is possible, and sometimes more elegant, to use programs positionally. Positional programming is similar to pointful
programming. It is is useful for writing sequential recipe-like programs, where, starting from an initial value
(often a composite-value), intermediate values (typically basic-values) are created, and, together with the initial
value, are passed as a composite value to the next step of the recipe-like program, until a final value is yielded. The
initial value and intermediate values are accessed positionally. The creation of the intermediate values can involve
general programs. A sequential recipe-like program glues programs together. For effectful programs, the order in which
programs are glued together really matters.

It is instructive to think about gluing together programs as being similar to gluing together executables using an operating system script, for example, a `bash` script of a `Linux` operating system.

## The `PSBP` library

The `PSBP` library type constructor classes are binary type constructor classes.

Recall that I simply called them classes.

I use few naming conventions

- Functions of type `α → β` are called `αfβ`.

- Programs of type `program α β` are called `αpβ`

Much in the same way I will also use names like `αaβ`, where the `a` stands for "and", and names like `αoβ`, where the
`o` stands for "or".

I use few showing conventions

- Functions of type `α → β` are shown as

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

By abuse of language, the specification requirements of the `PSBP` classes are written as statements.

It is instructive to compare this with using a statement like "a train can drive form A to B" instead of using the
requirement "a train should be able to drive from A to B".

### `class Functional`

Functions can, somehow, be used as programs. Functions that are used as programs are effectfree. We define functions as
programs in a formal way by defining `class Functional`.

```lean
class Functional
    (program : Type → Type → Type) where
  asProgram {α β : Type} :
    (α → β) → program α β

export Functional (asProgram)
```

```
        ╭───────╮
      α | ╭---╮ | β
    >───┼─┼αfβ┼─┼───>
        | ╰---╯ |
        ╰───────╯
```

The "somehow" in the sentence above is important. Functions are not programs "as-is".

## `class Functorial`

Functions can act upon programs. Functions act in an effectfree way upon programs. We define functions acting upon
programs in a formal way by defining `class Functorial`.

```lean
class Functorial
    (program : Type → Type → Type) where
  andThenF {α β γ : Type} :
    program α β → (β → γ) → program α γ

export Functorial (andThenF)

infixl:50 " >-> " => andThenF
```

```
        ╭───────────────╮
      α | ╭───╮ β ╭---╮ | γ
    >───┼─┤αpβ├─>─┼βpγ┼─┼───>
        | ╰───╯   ╰---╯ |
        ╰───────────────╯
```

`andThenF` also has infix notation `>->`.

### `class Sequential`

Programs can be sequentially combined. Sequentially combining programs can be seen as a second program acting upon a
first program. The difference with `Functorial` is that the second program may be effectful. Side effects are performed
from left to right. Moreover, side effects of the second program can depend on the final value yielded by the first
program. We define programs that are sequentially combined in a formal way by defining`class Sequential`.

```lean
class Sequential
    (program : Type → Type → Type) where
  andThenP {α β γ : Type} :
    program α β → program β γ → program α γ

export Sequential (andThenP)

infixl:50 " >=> " => andThenP
```

```
        ╭───────────────╮
      α | ╭───╮ β ╭───╮ | γ
    >───├─┤αpβ├─>─┤βpγ├─├───>
        | ╰───╯   ╰───╯ |
        ╰───────────────╯
```

`andThenP` also has infix notation `>=>`.

### `class Creational`

Programs can be combined to, sequentially, create product values. Their side effects are performed from left to right.
We define programs that, sequentially, produce product values in a formal way by defining `class Creational`.

```lean
class Creational
    (program : Type → Type → Type) where
  productSeq {α β γ : Type} :
    program α β → program α γ → program α (β × γ)

export Creational (productSeq)

infixl:60 " &&& " => productSeq
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

`productSeq` also has infix notation `&&&`.

### `class Conditional`

Programs can be combined to consume sum values. Only the left one and its side effects or the right one and its side 
effects is used. We define programs that consume sum values in a formal way by defining `class Conditional`.

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

### Mathematics related remark

Let's, from a mathematics point of view, have a look at the members of the classes defined so far.

Let's start with 

- `productSeq : program α β → program α γ → program α (β × γ)`
and
- `sum : program γ α → program β α → program (γ ⊕ β) α`

Somehow, polynomials, combinations of products (`×`) and sums (`⊕`), are part of the mathematical foundation of
programming. 

Let's continue with

- `asProgram : (α → β) → program α β`

Somehow, exponentials (`→`) are part of the mathematical foundation of programming.

Let's end with

- `andThenF : program α β → (β → γ) → program α γ`
and
- `andThenP : program α β → program β γ → program α γ`

and correponding derived definitions

```lean
def functionAction {α β γ : Type}
    [Functorial program] :
  (β → γ) → (program α β → program α γ) :=
    λ βfγ => (. >-> βfγ)
```

and

```lean
def programAction
    [Sequential program] :
  program β γ → (program α β → program α γ) :=
    λ βpγ => (. >=> βpγ)
```

Somehow, actions, transforming programs to programs, are part of the mathematical foundation of programming.

### Programming related remark

Programming is, on the one hand, about transforming values to values and, on the other hand, about performing side
effects. The members defined so far are compatible with performing side effects as far as not having an impact on the
order in which side effects are performed.

### `class Parallel`

Programs can be combined to, in parallel, to transform initial product values to final product values. The order in which values are transformed is not guarantied. More important, also the order in which their side effects are performed
is not guarantied. We define programs that, in parallel, transform initial product values to final product values in a
formal way by defining `class Parallel`.

```lean
class Parallel (program : Type → Type → Type) where
  bothPar {α β γ δ : Type} :
  program α γ → program β δ → program (α × β) (γ × δ)

export Parallel (bothPar)

infixl:60 " |&| " => bothPar
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

`bothPar` also has infix notation `|&|`.

### Writing programs

Programs are written in terms of members of classes like the ones defined so far. The class members are not defined,
they are declared. Classes are specifications. Programs written in terms of their members are program specifications,
but, by abuse of language we call them programs.

It is challenging to limit the expressiveness of class combinations in terms of whose members programs are written. More
specification expressivenes implies less implementation flexibility.

### `fibonacci` and `factorial`

It turns out that we ready now for defining `fibonacci` and `factorial`.

### Exercise (`identity`)

The `identity` program is a simple, but extremely useful function as a program.

*Exercise* :

Define `identity`.

*Hint* :

Recall that functions can be used as programs. Simple functions can be defined using simple lambda expressions.

You need to declare using `Functional` as follows `def identity [Functional program] : program α α`.

### Solution (`identity`)

<details>

```lean
def identity
    [Functional program] :
  program α α :=
    asProgram λ α => α
```

</details>

### `def let_`

Using the `let_` combinator an intermediate value can be constructed that is available for later use.

```lean
def let_
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α β → program (α × β) γ → program α γ :=
    λ αpβ αaβpγ => identity &&& αpβ >=> αaβpγ

 def in_ : α → α := id
```

Think of `let_` as a library level keyword.

`in_` is also a library level keyword that, depending on your taste, may make programs more readable.

How to read `program α β → program (α × β) γ → program α γ`?

If an initial value of type `α` can be transformed to a final value of type `β`, and, if that final value of type `β`
can, as an intermediate value of type `β`, together with the initial value of type `α` be transformed to a final value
of type `γ`, then an initial value of type `α` can be transformed to a final value of type `γ`.

The statement above does not use the word "function" or the word "program".

Using functions instead of programs (as in `Creational`) the type above can be programmed as below

```lean
def creational_αfγ {α β γ : Type} :
    (α → β) → ((α × β) → γ) → (α → γ) :=
  λ αfβ αaβfγ α =>
    let β := αfβ α
    αaβfγ (α, β)
```

Compare this with how to read `program α β → program α γ → program α γ`.

Using functions instead of programs (as in `Sequential`) the type above can be programmed as below

```lean
def sequential_αfγ {α β γ : Type} :
    (α → β) → (β → γ) → (α → γ) :=
  λ αfβ βfγ α =>
    let β := αfβ α
    βfγ β
```

- In the definition of `creational_αfγ`, the initial value of type `α` is used by `αaβfγ`.
- In the definition of `sequential_αfγ`, the initial value of type `α` is not used by `βfγ`.

Being able to formalize this difference using specifications, in this case `Creational` and `Sequential`, is an example
of gaining progressive insight into what programming is all about. In this case into what "intermediate value creation"
is all about.

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

def else_ : α → α := id
```

Think of `if_` as a library level keyword.

`else_` is also a library level keyword that, depending on your taste, may make programs more readable.

How to read `program α Bool → program α β → program α β → program α β`?

If there are two ways to transform an initial value of type `α` to a final value of type `β` available, then, 
depending on the final Boolean value the initial value is transformed to, one of both ways is chosen to transform the
initial value to a final value, obtaining a way to transform an initial value of type `α` to a final value of type `β`.

The statement above does not use the word "function" or the word "program".

Using functions instead of programs (as in `Conditional`) the type above can be programmed as below

```lean
def αfβ :
  (α → Bool) →
  (α → β) →
  (α → β) →
  (α → β) :=
    λ αfb t_afβ f_afβ α =>
      if αfb α
        then t_afβ α
        else f_afβ α
```

### Exercise (primitive programs)

For readability and reusability reasons it is useful to first define some primitive functions and corresponding
primitive programs.

*Exercise* :

Define `Nat` type based programs `isZero`, `isOne`, `one`, `minusOne`, `minusTwo`, `add` and `multiply`.

*Hint* :

Recall that functions can be used as programs. Simple functions can be defined using simple lambda expressions or as
separate `def`s.

- The names `isZero`, `isOne`, `minusOne` and `minusTwo` speak for themselves.
- The names `add` and `multiply` speak for themselves, their initial value type is `Nat × Nat`.
- Program `one` is the constant `1` function, used as a program,

### Solution (primitive programs)

<details>

Primitive functions

```lean
def isZeroF: Nat → Bool :=
  λ n => n == 0

def isOneF : Nat → Bool :=
  λ n => n == 1

def oneF : Nat → Nat :=
  λ _ => 1

def minusOneF : Nat → Nat :=
  λ n => n - 1

def minusTwoF : Nat → Nat :=
  λ n => n - 2

def addF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n + m

def multiplyF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n * m
```

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

def minusOne
    [Functional program] :
  program Nat Nat :=
    asProgram minusOneF

def minusTwo
    [Functional program] :
  program Nat Nat :=
    asProgram minusTwoF

def add
    [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram addF

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
      else_ $
        if_ isOne one $
          else_ $
            (minusOne >=> fibonacci) &&&
            (minusTwo >=> fibonacci) >=>
            add
```

### Exercise `fibonacci'`

*Exercise* :

Define `fibonacci'`, similar to `fibonacci`, but not using `else_`.

### Solution `fibonacci'`

<details>

```lean
unsafe def fibonacci'
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one $
      if_ isOne one $
        (minusOne >=> fibonacci') &&&
        (minusTwo >=> fibonacci') >=>
        add
```

</details>

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
      else_ $
        let_ (minusOne >=> factorial) $
          in_ $
            multiply
```

### Exercise `factorial'`

*Exercise* :

Define `factorial'`, similar to `factorial`, but not using `else_` and `in_`.

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

The `unsafe` keyword is used because the definitions above do not type check without them. Lean cannot prove that
`fibonacci` and `factorial` can be used in a safe way. Note that they are program specifications. They need to be
materialized before they can be used. Much in the same way, specifications of side effects are descriptions of
side effects. They need to be materialized to be performed. 

It is instructive to compare this with a painting of something going wrong. It is safe to hang the painting on your   
wall. Nothing will go wrong.

### `parallelFibonacci`

Program `parallelFibonacci` is defined as follows

Let

```lean
def dup
    [Functional program] :
  program α (α × α) :=
    asProgram λ α => (α, α)
```

and

```lean
def productPar {α β γ : Type}
    [Functional program]
    [Sequential program]
    [Parallel program] :
  program α β → program α γ → program α (β × γ) :=
   λ αpβ αpγ => dup >=> αpβ |&| αpγ

infixl:60 " &|& " => productPar
```

in

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

The only difference between `fibonacci` and `parallelFibonacci` is that `fibonacci` uses `&&&` while
`parallelFibonacci` uses `&|&`.


### About `$`

You may have questions about the usage of the `$` operation. It is an operation with low precedence that avoids using
parentheses (`(` and `)`). Of course it is also possible to use nested parentheses as in

```lean
unsafe def fibonacci''
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

and to use parentheses (in this example there is no need to nest them) as in

```lean
unsafe def factorial''
    [Functional program]
    [Sequential program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program Nat Nat :=
    if_ isZero one (
      let_ (minusOne >=> factorial')
        multiply
    )
```

It is all a matter of taste.

### Exercise (`bothSeq` using `productSeq`)

The basic member of `Creational` is `productSeq`. The basic member of `Parallel` is `bothPar`. `productPar`
(infix `&|&`) is defined using `|&|` (infix `bothPar`).

*Exercise* :

Define `bothSeq` (and infix `<&>`) using `productSeq` (infix `&&&`). The type of `bothSeq`
is `program α γ → program β δ → program (α × β) (γ × δ)`.

*Hint* :

Define `first` of type `program (α × β) α` and `second` of type `program (α × β) β` and, somehow, use them, together
with `>=>`, from `Sequential`, and `&&&`, from `Creational`.

This exercises is an examples of a "getting the types right puzzle".

### Solution (`bothSeq` using `productSeq`)

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

```lean
def bothSeq
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      (first >=> αpγ) &&& second >=>
        first &&& (second >=> βpδ)

infixl:60 " <&> " => bothSeq
```

</details>

### Exercise (`bothSeq'` using `onlyFirst` and `onlySecond`)

*Exercise* :

Define `onlyFirst` of type ` program α β → program (α × γ) (β × γ)`, and `onlySecond` of type
`program γ δ → program (α × γ) (α × δ)`, and define `bothSeq'` using `onlyFirst` and `onlySecond`.

*Hint* :

Perhaps you already defined `onlyFirst` and `onlySecond` (perhaps using other names) to solve the
"getting the types right puzzle" of the previous exercise. Anyway, define them using names `onlyFirst` and `onlySecond`
so that they can also be reused later using those names.

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
def bothSeq'
    [Functional program]
    [Sequential program]
    [Creational program] :
  program α γ → program β δ → program (α × β) (γ × δ) :=
    λ αpγ βpδ =>
      onlyFirst αpγ >=> onlySecond βpδ
```

</details>
e
### Types don't tell everything

Consider the four basic members or derived deinitions below

- `productSeq {α β γ : Type} : program α β → program α γ → program α (β × γ)`
- `bothSeq {α β γ : Type} : program α γ → program β δ → program (α × β) (γ × δ)`

and

- `productPar {α β γ : Type} : program α β → program α γ → program α (β × γ)`
- `bothPar {α β γ δ : Type} : program α γ → program β δ → program (α × β) (γ × δ)`

The types of `productSeq` and `productPar` resp. `bothSeq` and `bothPar` are the same.

There is a choice to be made now.

1. Should we treat the difference between the `Seq` and `Par` version as a specification concern?

or

2. Should we treat the difference between the `Seq` and `Par` version as an implementation concern?

`PSBP` goes for the former, if only because the order in which side effects are ferformed matter,

### Exercise (`Functorial` using `Functional` and `Sequential`)

You may wonder why neither `fibonacci` nor `factorial` use `Functorial`.

*Exercise* :

Define a `Functorial` instance using `Functional` and `Sequential`.

*Hint* :

Recall that functions can be used as programs.

### Solution (`Functorial` using `Functional` and `Sequential`)

<details>

```lean
instance
    [Functional program]
    [Sequential program] :
  Functorial program where
    andThenF {α β γ: Type} :
      program α β → (β → γ) → program α γ :=
        λ αpβ βfγ => αpβ >=> asProgram βfγ
```

</details>

### Using `Functorial` with `Functional`, and `Creational`

So why introducing `Functorial` in the first place? Well, the combination of `Functorial` with `Functional` and
`Creational` is sufficiently expressive to write interesting programs with. They are more flexible as far as
implementation and corresponding materialization is concerned than the ones using `Sequential`.

Below are two programs, `twiceMinusOneFunctorial` and `twiceMinusOneSequential`.

```lean
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
```

`twiceMinusOneSequential` uses the more powerful of `Sequential` while `twiceMinusOneFunctorial` uses the less powerful
`Functorial`. Using `Sequential` is, in this case, an unnecessary overkill because all programs involved are effectfree.

That being said, you may argue that what you have read so far is also an unnecessary overkill because, after all, so far, I only defined effectfree programs. But think of replacing `minusOne` and/ or `minusTwo` by an effectful program
Having more implementation and corresponding materialization flexibility when dealing with side effects can really be useful. A standard example is more flexible error handling when processing a submitted web form. Another example is error correction when parsing a document.

### Exercise (`productSeq'` using `let_`)

*Exercise* :

Define `productSeq'`, an alternative version of `productSeq`, using `let_`.

*Hint* :

Yet another "getting the types right puzzle".

### Solution (`productSeq'` using `let_`)

<details>

```lean
def productSeq'
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

### Exercise (`sum'` using `if_`)

*Exercise* :

Define `sum'`, an alternative version of `sum`, using `if_`.

*Hint* :

Yet another "getting the types right puzzle".

When matching with a value of type `γ ⊕ β` You may need to use `sorry` (think of it as undefined).

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
              | Sum.inl _ => True
              | Sum.inr _ => False))
        (asProgram
          (λ γoβ => match γoβ with
            | Sum.inl γ => γ
            | Sum.inr β => sorry) >=> γpα) $
        asProgram
          (λ γoβ => match γoβ with
            | Sum.inl _ => sorry
            | Sum.inr β => β) >=> βpα
```

</details>

## Laws

### `class LawfulFunctional`

`Functional` comes with laws.

```lean
class LawfulFunctional
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

The `functional_identity` law relates function identity and program identity, and is `True` per definition.

The `functional_sequential` law relates function sequential combination and program sequential combination.

### `class LawfulFunctorial`

`Functorial` comes with laws.

```lean
class LawfulFunctorial
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

The `functorial_identity` law states that the identity function action on a program leaves the program intact.

The `functorial_sequential` law relates function sequential combination and function action sequential combination.

### `class LawfulSequential`

`Functorial` comes with laws.

```lean
class LawfulSequential
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

They all involve `onlyFirst`. They are the `arrow` laws (`Sequential` is involved, `Conditional` is not involved).

Let

```lean
def applyAtFirst
    [Functional program] :
  (α → β) → program (α × γ) (β × γ) :=
    λ αfβ => asProgram λ (α, γ) => (αfβ α, γ)
```

and

```lean
def applyAtSecond
    [Functional program] :
  (β → γ) → program (α × β) (α × γ) :=
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

### Exercise (extra `Creational` law)

*Exercise*

Make the extra `Creational` law below complete

```lean
class InCompleteExtraLawfulCreational
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

Is the following law a valid one (open question)?

```lean
class ExtraLawfulCreationalQuestion
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

The order of the side effects performed by the programs involved matters.
Therefore, because of it's distributive nature, this law is unlikely to be a valid one for most implementations.

In fact, for the computation valued function implementation (see next section).

- the order of the left hand side is `αpβ`, `αpγ`, `βpδ`, `γpε`
- the order of the right hand side is `αpβ`, `βpδ`, `αpγ`,`γpε`

</details>

### Exercise (extra `Creational` law for `let_`)

*Exercise*

Make the extra `Creational` law for `let_` below complete.

Is the law a valid one (open question)?

```lean
class IncompleteLawfulCreationalLet
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

The order of the side effects performed by the programs involved matters.
In this case it's sequential nature does not matter, therefore this law is likely to be a valid one for most
implementations.

</details>

### Exercise (extra `Conditional` law for `if_`)

*Exercise*

Define an extra `Conditional` law for `if_`, similar to the extra `Creational` law for `let_`.

Is the law a valid one (open question)?

### Solution (extra `Conditional` law for `if_`)

<details>

```lean
class LawfulCreationalIf
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

The order of the side effects performed by the programs involved matters.
In this case it's sequential nature does not matter, therefore this law is likely to be a valid one for most
implementations.

</details>

## Computation Valued Functions

Using computation valued functions is a generic way to implement the program related classes in terms of the computation
related classes.

```lean
structure FromComputationValuedFunction
    (computation : (Type → Type)) (α β : Type) where
  toComputationValuedFunction : α → computation β

instance [Applicative computation] :
    Functional
      (FromComputationValuedFunction computation) where
  asProgram :=
    λ αfβ => ⟨λ α => pure $ αfβ α⟩

instance [Functor computation] :
    Functorial
      (FromComputationValuedFunction computation) where
  andThenF :=
    λ ⟨αfcβ⟩ βfγ => ⟨λ α => βfγ <$> αfcβ α⟩

instance [Applicative computation] :
    Creational
      (FromComputationValuedFunction computation) where
  productSeq :=
    λ ⟨αfcβ⟩ ⟨αfcγ⟩ =>
      ⟨λ α => .mk <$> αfcβ α <*> αfcγ α⟩

instance [Monad computation] :
    Sequential
      (FromComputationValuedFunction computation) where
  andThenP :=
    λ ⟨αfcβ⟩ ⟨βfcγ⟩ =>
      ⟨λ α => αfcβ α >>= βfcγ⟩

def foldSum {γ β α : Type}
    (γfα : γ → α)
    (βfα : β → α)
    (sum : γ ⊕ β) : α :=
  match sum with
  | .inl tc => γfα tc
  | .inr tb => βfα tb

instance :
    Conditional
      (FromComputationValuedFunction computation) where
  sum :=
    λ ⟨γfγα⟩ ⟨βfγα⟩ =>
      ⟨foldSum γfγα βfγα⟩
```

Dealing with parallelism is a bit more involved. It uses the asynchronous computing related class `MonadAsync`.

```lean
class MonadAsync
    (computation : Type → Type) where
  async {α : Type} (ufα : Unit → α) : computation α

export MonadAsync (async)

instance
    [Monad computation]
    [MonadAsync computation] :
  Parallel (FromComputationValuedFunction computation) where
    bothPar := λ ⟨αfcγ⟩ ⟨βfcδ⟩ =>
      ⟨λ ⟨α, β⟩ =>
        async (λ (_: Unit) => αfcγ α) >>=
          λ cγ => async (λ (_: Unit) => βfcδ β) >>=
            λ cδ => .mk <$> cγ <*> cδ⟩
```

A word of warning, The code above used `⟨` and `⟩`, different from `(` and `)`, to asemble and disassemble `structure`'s
like `FromComputationValuedFunction`.

## Theorems

The laws of the various classes need to be proved for the various instances. First we prove them and next we let `Lean`
prove them for us.

### `Functional` theorems

Theorem `functional_identity` below is proved by definition using `calc` and
`rfl`.

```lean
@[simp] theorem functional_identity
  {α : Type}
    [Applicative computation] :
    (identity :
      FromComputationValuedFunction computation α α)
      = asProgram id :=
  calc
    identity
        = asProgram id
          := rfl
```
Theorem `functional_sequential'` also uses `congrArg` and `funext`.

Theorem `functional_sequential'` uses the `pure_bind` law of `LawfulMonad`.

```lean
theorem functional_sequential'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β)
  (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) :=
  calc
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
        = (⟨λ α => pure $ αfβ α⟩ >=> ⟨λ β => pure $ βfγ β⟩)
          := rfl
    _   = ⟨λ α => (pure $ αfβ α) >>= λ β => pure $ βfγ β⟩
          := rfl
    _   = ⟨λ α => pure $ βfγ (αfβ α)⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α =>
                 pure_bind (αfβ α) (λ β => pure $ βfγ β))
    _   = ⟨λ α => pure $ (βfγ ∘ αfβ) α⟩
          := rfl
    _   = asProgram (βfγ ∘ αfβ)
          := rfl
```

Theorem `functional_sequential` uses `by simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem functional_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β)
  (βfγ : β → γ) :
    (asProgram αfβ >=> asProgram βfγ :
      FromComputationValuedFunction computation α γ)
      = asProgram (βfγ ∘ αfβ) := by
  simp[asProgram, andThenP]
```

Note that `functional_sequential` is not annotated by `@[simp]` so that `functional_sequential'` cannot use it. As a
consequence it is necessary to tell `Lean` to unfold everything in order for it to see the real definitions involved.

### `Functorial` theorems

Theorem `functorial_identity'` the `id_map` law of `LawfulFunctor`.

```lean
theorem functorial_identity'
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >-> id :
      FromComputationValuedFunction computation α β)
      = αpβ := by
  let αfcβ := αpβ.toComputationValuedFunction
  calc
    (αpβ >-> id)
        = ⟨λ α => id <$> αfcβ α⟩
          := rfl
    _   = ⟨λ α => αfcβ α ⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α => id_map (αfcβ α))
    _   = ⟨αfcβ⟩
          := rfl
```

Theorem `functorial_identity` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem functorial_identity
  {α β : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >-> id :
      FromComputationValuedFunction computation α β)
      = αpβ := by
    simp[andThenF]
```

Theorem `functorial_sequential'` uses the `comp_map` law of `LawfulFunctor`.

```lean
theorem functorial_sequential'
  {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βfγ : β → γ)
  (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ :
      FromComputationValuedFunction computation α δ)
      = (αpβ >-> (γfδ ∘ βfγ)) := by
  let αfcβ := αpβ.toComputationValuedFunction
  calc
    ((αpβ >-> βfγ) >-> γfδ)
        = (⟨λ α => βfγ <$> αfcβ α⟩ >-> γfδ)
          := rfl
    _   = ⟨λ α => γfδ <$> (λ α => βfγ <$> αfcβ α) α⟩
          := rfl
    _   = ⟨λ α => γfδ <$> βfγ <$> αfcβ α⟩
          := rfl
    _   = ⟨λ α => (γfδ ∘ βfγ) <$> αfcβ α⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α =>
                 Eq.symm (comp_map βfγ γfδ (αfcβ α)))
    _   = (αpβ >-> (γfδ ∘ βfγ))
          := rfl
```

Theorem `functorial_sequential` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem function_sequential
    (αfβ : α → β)
    (βfγ : β → γ)
    (a : α):
  ((βfγ ∘ αfβ): α → γ) =
    λ α => βfγ (αfβ α) := by
      rfl

@[simp] theorem functorial_sequential
    {α β γ δ : Type}
    [Functor computation]
    [LawfulFunctor computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βfγ : β → γ)
  (γfδ : γ → δ) :
    ((αpβ >-> βfγ) >-> γfδ :
      FromComputationValuedFunction computation α δ)
      = (αpβ >-> (γfδ ∘ βfγ)) := by
        simp[andThenF, function_sequential]
```

### `Sequential` theorems

Theorem `sequential_right_identity'` uses the `bind_pure_comp` law of `LawfulMonad` and the `comp_map` law of
`LawfulFunctor`.

```lean
theorem sequential_right_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (αpβ >=> asProgram id :
      FromComputationValuedFunction computation α β)
    = αpβ := by
   let αfcβ := αpβ.toComputationValuedFunction
   calc
      (αpβ >=> asProgram id :
          FromComputationValuedFunction computation α β)
          = ⟨λ α => αfcβ α >>= λ β => pure (id β)⟩
            := rfl
      _   = ⟨λ α => id <$> αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => bind_pure_comp id (αfcβ α))
      _   = ⟨λ α => αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => id_map (αfcβ α))
      _   = αpβ
            := rfl
```

Theorem `sequential_right_identity` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem sequential_right_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    ((αpβ >=> asProgram id) :
      FromComputationValuedFunction computation α β)
      = αpβ := by simp[andThenP]
```

Theorem `sequential_left_identity'` uses the `pure_bind` law of `LawfulMonad`.

```lean
@[simp] theorem sequential_left_identity'
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (asProgram id >=> αpβ :
      FromComputationValuedFunction computation α β)
    = αpβ := by
    let αfcβ := αpβ.toComputationValuedFunction
    calc
      (asProgram id >=> ⟨αfcβ⟩ :
        FromComputationValuedFunction computation α β)
          = ⟨λ α => pure α >>= αfcβ⟩
            := rfl
      _   = ⟨λ α => αfcβ α⟩
            := congrArg
                 FromComputationValuedFunction.mk
                 (funext λ α => pure_bind α αfcβ)
      _   = ⟨αfcβ⟩
            := rfl
```

Theorem `sequential_left_identity` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem sequential_left_identity
  {α β : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (asProgram id >=> αpβ :
      FromComputationValuedFunction computation α β)
    = αpβ := by simp[andThenP]
```

Theorem `sequential_associative'` uses the `pure_assoc` law of `LawfulMonad`.

```lean
@[simp] theorem sequential_associative'
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ)
  (γpδ : FromComputationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ) =
      (αpβ >=> (βpγ >=> γpδ)) := by
  let αfcβ : α → computation β :=
    αpβ.toComputationValuedFunction
  let βfcγ : β → computation γ :=
    βpγ.toComputationValuedFunction
  let γfcδ : γ → computation δ :=
    γpδ.toComputationValuedFunction
  let βfcδ : β → computation δ :=
    λ β => βfcγ β >>= γfcδ
  calc
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ)
        = (⟨λ α => αfcβ α >>= βfcγ⟩ >=> ⟨γfcδ⟩)
          := rfl
    _   = ⟨λ α => αfcβ α >>= βfcγ >>= γfcδ⟩
          := rfl
    _   = ⟨λ α => αfcβ α >>= (λ β => βfcγ β >>= γfcδ)⟩
          := congrArg
               FromComputationValuedFunction.mk
               (funext λ α => bind_assoc (αfcβ α) βfcγ γfcδ)
    _   = (⟨λ α => αfcβ α >>= βfcδ⟩ :
            FromComputationValuedFunction computation α δ)
          := rfl
    _   = (αpβ >=> (βpγ >=> γpδ):
            FromComputationValuedFunction computation α δ)
          := rfl
```

Theorem `sequential_associative` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem sequential_associative
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ)
  (γpδ : FromComputationValuedFunction computation γ δ) :
    ((αpβ >=> βpγ) >=> γpδ :
      FromComputationValuedFunction computation α δ) =
      (αpβ >=> (βpγ >=> γpδ)) := by simp[andThenP]
```

### `Creational` theorems

Theorem `creational_onlyFirst_asProgram'` uses the `pure_bind` law of `LawfulMonad` and the `map_pure` law of
`LawfulApplicative`.

```lean
theorem creational_onlyFirst_asProgram'
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β) :
    (onlyFirst (asProgram αfβ) :
      FromComputationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by
  calc
    (onlyFirst (asProgram αfβ))
        = onlyFirst ⟨λ α => pure $ αfβ α⟩ :=
        rfl
    _   = ((first :
           FromComputationValuedFunction
             computation (α × γ) α) >=>
            (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = (((asProgram λ (α, _) => α) :
            FromComputationValuedFunction
              computation (α × γ) α) >=>
              (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = ((⟨λ (α, _) => pure α⟩ :
            FromComputationValuedFunction
              computation (α × γ) α) >=>
              (⟨λ α => pure $ αfβ α⟩)) &&& second :=
        rfl
    _   = (⟨λ (α, _) => pure α >>= (λ α => pure $ αfβ α)⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        rfl
    _   = (⟨(λ (α, _) => pure $ αfβ α)⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        congrArg
          (λ (αfcβ : α → computation β) =>
            ((⟨λ (α, _) => αfcβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second))
          (funext (λ α =>
            pure_bind α (λ α => pure $ αfβ α)))
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&& second :=
        rfl
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&&
              (asProgram (λ (_, γ) => γ) :
                FromComputationValuedFunction
                  computation (α × γ) γ) :=
        rfl
    _   = (⟨λ (α, _) => pure $ αfβ α⟩ :
            FromComputationValuedFunction
              computation (α × γ) β) &&&
              (⟨λ (_, γ) => pure $ γ⟩ :
                FromComputationValuedFunction
                  computation (α × γ) γ) :=
        rfl
    _   = (⟨λ (α, γ) =>
            (Prod.mk <$> (pure $ αfβ α)) <*> (pure $ γ)⟩ :
             FromComputationValuedFunction
               computation (α × γ) (β × γ)) :=
        rfl
    _   = (⟨λ (α, γ) =>
            (pure $ Prod.mk (αfβ α)) <*> (pure $ γ)⟩ :
             FromComputationValuedFunction
             computation (α × γ) (β × γ)) :=
        congrArg
         (FromComputationValuedFunction.mk ∘
           ((λ αfβaγ =>
             λ (α, γ) => αfβaγ α <*> (pure $ γ)) :
             (α → computation (γ → (β × γ))) →
               ((α × γ) → computation (β × γ))))
          (funext λ α => (map_pure (Prod.mk) (αfβ α)))
    _   = (⟨λ (α, γ) => Prod.mk (αfβ α) <$> (pure $ γ)⟩) :=
        congrArg
         FromComputationValuedFunction.mk
          (funext λ (α, γ) =>
            (pure_seq (Prod.mk (αfβ α)) (pure $ γ)))
    _   = (⟨λ (α, γ) => pure (Prod.mk (αfβ α) γ)⟩) :=
        congrArg
         FromComputationValuedFunction.mk
          (funext λ (α, γ) =>
            (map_pure (Prod.mk (αfβ α)) γ))
    _   = (⟨λ (α, γ) => pure $ (αfβ α, γ)⟩) :=
        rfl
    _   = (asProgram (λ (α, γ) => (αfβ α, γ))) :=
        rfl
```

Theorem `creational_onlyFirst_asProgram` uses `simp` to let `Lean` do the heavy lifting

```lean
@[simp] theorem creational_onlyFirst_asProgram
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αfβ : α → β) :
    (onlyFirst (asProgram αfβ) :
      FromComputationValuedFunction
        computation (α × γ) (β × γ)) =
      (asProgram (λ (α, γ) => (αfβ α, γ))) := by simp [
        onlyFirst, asProgram, productSeq, first, second
        ]
```

By now you probably agree that `calc` based proofs can become tedious.

In what follows, I will, mostly, only show the `simp` based proofs.

```lean
@[simp] theorem creational_onlyFirst_sequential
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (βpγ : FromComputationValuedFunction computation β γ) :
    (onlyFirst (αpβ >=> βpγ) :
      FromComputationValuedFunction
        computation (α × δ) (γ × δ)) =
      (onlyFirst αpβ >=> onlyFirst βpγ :
        FromComputationValuedFunction
          computation (α × δ) (γ × δ)) := by simp[
            onlyFirst, andThenP, asProgram, productSeq,
            first, second
            ]
```

```lean
@[simp] theorem creational_onlyFirst_first
  {α β γ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (onlyFirst αpβ >=> first :
      FromComputationValuedFunction computation (α × γ) β) =
      (first >=> αpβ) := by simp[
        onlyFirst, andThenP, asProgram, productSeq, first,
        second
        ]
```

```lean
@[simp] theorem creational_onlyFirst_applyAtSecond
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β)
  (γfδ : γ → δ) :
    (onlyFirst αpβ >=> applyAtSecond γfδ) =
      (applyAtSecond γfδ >=> onlyFirst αpβ) := by simp[
        onlyFirst, andThenP, applyAtSecond, asProgram,
        productSeq, first, second
        ]
```

```lean
@[simp] theorem creational_onlyFirst_assoc
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (αpβ : FromComputationValuedFunction computation α β) :
    (onlyFirst (onlyFirst αpβ) >=> assoc :
      FromComputationValuedFunction
        computation ((α × γ) × δ) (β × (γ × δ))) =
      (assoc >=> onlyFirst αpβ) := by simp[
        onlyFirst, andThenP, asProgram, productSeq, first,
        second, assoc
        ]
```

### `Conditional` theorems

```lean
@[simp] theorem conditional_left
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (γpα : FromComputationValuedFunction computation γ α)
  (βpα : FromComputationValuedFunction computation β α) :
    (left >=> γpα ||| βpα
      : FromComputationValuedFunction computation γ α) =
      γpα := by
       simp[left, asProgram, andThenP, foldSum]
```

```lean
@[simp] theorem conditional_right
  {α β γ δ : Type}
    [Monad computation]
    [LawfulMonad computation]
  (γpα : FromComputationValuedFunction computation γ α)
  (βpα : FromComputationValuedFunction computation β α) :
    (right >=> γpα ||| βpα
      : FromComputationValuedFunction computation β α) =
      βpα := by
       simp[right, asProgram, andThenP, foldSum]
```

## `ActiveProgram`

There is not a lot of work to be done for active computation implementations. The `Functor`, `Applicative` and `Monad`
implementations of `Id` can be used.

```lean
abbrev Active := Id

abbrev ActiveProgram α β :=
  FromComputationValuedFunction Active α β

def materializeActive :
    ActiveProgram α β → (α → β) :=
  λ ⟨αfaβ⟩ α => (αfaβ α).run
```

We can now run programs in an active way.

```lean
unsafe def activeFibonacci :=
  materializeActive fibonacci

#eval activeFibonacci 10
```

```lean
info: PSBP/All.lean:931:0: 89
```

```lean
unsafe def activeFactorial :=
  materializeActive factorial

#eval activeFactorial 10
```

```lean
info: PSBP/All.lean:936:0: 3628800
```

## `ReactiveProgram`

There is much more work to be done for reactive computation implementations. They are callback handler based, a.k.a.
continuation based.

```lean
structure ReactiveT
    (ρ : Type)
    (computation: Type → Type)
    (α : Type) where
  runReactiveT : (α → computation ρ) → computation ρ

abbrev Reactive ρ := ReactiveT ρ Active

instance {ρ: Type} :
    Functor (ReactiveT ρ computation) where
  map :=
    λ αfβ ⟨rcα⟩ =>
      ⟨λ γ => rcα (γ ∘ αfβ)⟩

instance {ρ: Type} :
    Applicative (ReactiveT ρ computation) where
  pure := λ α => ReactiveT.mk (λ αfcρ => αfcρ α)
  seq :=
    λ ⟨rcαfβ⟩ ufrtρcα =>
      ⟨λ βfcρ =>
        rcαfβ $
          (λ αfβ =>
            (ufrtρcα ()).runReactiveT (βfcρ ∘ αfβ))⟩

instance {ρ: Type} :
    Monad (ReactiveT ρ computation) where
  bind :=
    λ ⟨rcα⟩ αfrtρcβ =>
      ⟨λ βfcρ =>
        rcα (λ α =>
        (αfrtρcβ α).runReactiveT βfcρ)⟩

abbrev ReactiveProgram ρ computation :=
  FromComputationValuedFunction (ReactiveT ρ computation)

def materializeReactive {α β : Type} :
    ReactiveProgram β Active α β → α → β :=
  λ ⟨αfrtaβcβ⟩ α =>
      (αfrtaβcβ α).runReactiveT id
```

The `ρ` stands for the result of callback handling.

We can now run programs in a reactive way.

```lean
unsafe def reactiveFibonacci :=
  materializeReactive fibonacci

#eval reactiveFibonacci 10
```

```lean
info: PSBP/All.lean:981:0: 89
```

```lean
unsafe def reactiveFactorial :=
  materializeReactive factorial

#eval reactiveFactorial 10
```

```lean
info: PSBP/All.lean:986:0: 3628800
```

We did not change the definition of our programs, we only materialized them in another way!

# `TasksProgram`

There is a bit more work to be done for asynchrounous computation based implementations. They spawn tasks.

```lean
instance : Monad Task where
  pure := Task.pure
  bind := Task.bind

instance : MonadAsync Task where
  async := Task.spawn

abbrev TasksProgram :=
  FromComputationValuedFunction Task

def materializeTasks {α β : Type} :
  TasksProgram α β → (α → β) :=
    λ ⟨αftβ⟩ α => (αftβ α).get
```

We can now run parallel programs in a tasks spawning way.

```lean
unsafe def tasksFibonacci :=
  materializeTasks fibonacci

#eval tasksFibonacci 10
```

```lean
info: PSBP/All.lean:1005:0: 89
```

We only slightly change the definition of our program and materialized if in another way!

You may wish to also try

```lean
#eval tasksFibonacci 24
```

and have a look at how it keeps all threads of your OS busy (e.g. on `Linux` using `htop`).

## Positional Programming

### Using `Functorial`

Pointfree programming, like is done for `fibonacci` and `factorial` may be a elegant, but `PSBP` also enables, and,
for reasons of elegance, sometimes needs, positional programming. Let's first start with `Functorial` based positional
programming. Suppose we want to run the function that transforms an initial (argument) value `n` of type `Nat` to the
final (result) value `(n-2) + 2 * (n-1) + 3`. `somePositionalProgramFunctorial` below could be a solution.

Let

```lean
def twoF : Nat → Nat := λ _ => 2

def threeF : Nat → Nat := λ _ => 3
```

and

```lean
def two [Functional program] :
  program Nat Nat :=
    asProgram twoF

def three [Functional program] :
  program Nat Nat :=
    asProgram threeF
```
in

```lean
def somePositionalProgramFunctorial
    [Functional program]
    [Functorial program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusTwo &&& two &&& three >->
      λ (((n1, n2), n3), n4) =>
        n2 + n3 * n1 + n4
```

We can now run this positional program.

```lean
def twentyNineFunctorial :=
  materializeActive somePositionalProgramFunctorial 10

#eval twentyNineFunctorial
```

```lean
info: PSBP/All.lean:1042:0: 29
```

You may argue that, as for as the acting function involved is concerned, we are back to pointful programming. Well,
somehow you are right, but notice that all `n`'s involved have indices (`1`, `2`, `3` and `4`). They can be thought of
as positions. So we are essentially accessing values at positions of multi-values. For this example the multi-value
involved is homogeneous but it might as well be a heterogeneous one. More about this later.

### Using `Sequential`

`someProgram02` below could also be a solution. `someProgram02` makes use of `Sequential`.

```lean
def somePositionalProgramSequential
    [Functional program]
    [Sequential program]
    [Creational program] :
  program Nat Nat :=
    minusOne &&& minusTwo &&& two &&& three >=>
      asProgram (λ (((n1, n2), n3), n4) =>
        n2 + n3 * n1 + n4)
```

We can now run this positional program.

```lean
def twentyNineSequential :=
  materializeActive somePositionalProgramSequential 10

#eval twentyNineSequential
```

```lean
info: PSBP/All.lean:1047:0: 29

```

As already mentioned before, using `Sequential` is, in this case, an overkill.

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

The `σ` stands for (runtime) "stack", and `σ × β` stands for `β` pushed onto `σ`. More about this later.

### `instance Positional`

The `at_` library level keyword of `Positional` can be defined in terms of `Functional`, `Sequential` and `Creational`.

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

Think of `σpα` as accessing a (multi-)value, `α`, on a runtime stack, `σ`. Think of `αpβ` as transforming that
(multi-)value to `β`. `let_` then pushes `β` on `σ` obtaining a runtime stack`σ × β`. So, if it possible to transform the runtime stack `σ` to `α`, to transform `α` to an intermediate value `β` and to transform `σ × β` to `γ`, then it
is possible to transform `σ` to `γ`.

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

`positionOne` and `positionTwo` are basic-value positions. `positionOneAndTwo` is a multi-value position.

### `positionalFactorialOfFibonacci`

Below is a positional program, `positionalFactorialOfFibonacci`. It uses only uses `positionOne`. Positions go up
starting from their use site.

```lean
unsafe def positionalFactorialOfFibonacci
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program (σ × Nat) Nat :=
    fibonacci @ positionOne $
      factorial @ positionOne $
        positionOne
```

`Positional` is not part of the list of required class instances because it can be inferred.

We can now run this positional program with an initial value pushed on the runtime stack.

```lean
unsafe def factorialOfFibonacci: (Unit × Nat) → Nat :=
  materializeActive positionalFactorialOfFibonacci

#eval factorialOfFibonacci ((), 5)
```

```lean
info: PSBP/All.lean:1115:0: 40320
```

### `positionalSumOfFibonacciAndFactorial`

Below is a positional program, `positionalSumOfFibonacciAndFactorial`. It uses only uses three positions. Positions go
up starting from their use site.

```lean
unsafe def positionalSumOfFibonacciAndFactorial
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program] :
  program (σ × Nat) Nat :=
    fibonacci @ positionOne $
      factorial @ positionTwo $
        add @ positionOneAndTwo $
          positionOne
```

`Positional` is not part of the list of required class instances because it can be inferred.

We can now run this positional program with an initial value pushed on the runtime stack.

```lean
unsafe def sumOfFibonacciAndFactorial: (Unit × Nat) → Nat :=
  materializeActive positionalSumOfFibonacciAndFactorial

#eval sumOfFibonacciAndFactorial ((), 5)
```

```lean
info: PSBP/All.lean:1120:0: 128
```

### Showing the runtime stack

It is instructive to show the runtime stack using `identity`. This is done in `positionalFactorialOfFibonacci'` and
`positionalSumOfFibonacciAndFactorial'` below.

```lean
unsafe def positionalFactorialOfFibonacci'
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program (σ × Nat) (((σ × Nat) × Nat) × Nat) :=
    fibonacci @ positionOne $
      factorial @ positionOne $
        identity
```

```lean
unsafe def positionalSumOfFibonacciAndFactorial'
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program] :
  program (σ × Nat) ((((σ × Nat) × Nat) × Nat) × Nat) :=
    fibonacci @ positionOne $
      factorial @ positionTwo $
        add @ positionOneAndTwo $
          identity
```

We can now run those runtime stack showing positional program with an initial value pushed on the runtime stack.


```lean
unsafe def factorialOfFibonacci' :
    (Unit × Nat) → (((Unit × Nat) × Nat) × Nat) :=
  materializeActive positionalFactorialOfFibonacci'

#eval factorialOfFibonacci' ((), 5)
```

```lean
info: PSBP/All.lean:1147:0: ((((), 5), 8), 40320)
```

```lean
unsafe def sumOfFibonacciAndFactorial' :
    (Unit × Nat) → ((((Unit × Nat) × Nat) × Nat) × Nat) :=
  materializeActive positionalSumOfFibonacciAndFactorial'

#eval sumOfFibonacciAndFactorial ((), 5)
```

```lean
info: PSBP/All.lean:1153:0: (((((), 5), 8), 120), 128)
```

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

#### `def modifyStateWith`

Below is `modifyStateWith`, a useful programming capability.

```lean
def modifyStateWith
    [Functional program]
    [Sequential program]
    [Creational program]
    [WithState σ program] :
  (σ → σ) → program α α :=
    λ σfσ =>
      let_ ((readState >=> asProgram σfσ) >=> writeState) $
        in_ $
          first
```

`modifyStateWith` modifies the state using a function.

### `withInitialStateAsInitialValue`

```lean
def withInitialStateAsInitialValue
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithState σ program] :
  program σ τ → program α τ :=
    λ σpτ =>
      readState >=> σpτ
```

Given a program `σpτ`, `withInitialStateAsInitialValue` transforms it to use the initial state as initial value.

### `instance WithState σ`

`WithState σ` is implemented in terms of `MonadStateOf`.

```lean
instance [MonadStateOf σ computation] :
    WithState σ
      (FromComputationValuedFunction computation) where
  readState := ⟨λ _ => getThe σ⟩
  writeState := ⟨set⟩
```

### `ProgramWithState`

```lean
abbrev ProgramWithState σ computation :=
  FromComputationValuedFunction (StateT σ computation)

def materializeWithState
    [Monad computation] {α β : Type} :
  ProgramWithState σ computation α β →
  α →
  σ →
  computation β :=
    λ ⟨αfstσcβ⟩ =>
      λ α =>
        λ σ =>
          StateT.run (αfstσcβ α) σ >>=
            λ (β, _) => pure β

def materializeActiveWithState {α β : Type} :
  ProgramWithState σ Active α β → α → σ → β :=
    materializeWithState
```

### `class LawfulWithState`

```lean
class LawfulWithState
    [Functional program]
    [Sequential program]
    [WithState σ program] : Prop where
  withState_write_read :
    ((writeState : program σ Unit) >=>
      (readState : program Unit σ)) =
      identity
```

### theorem `withState_write_read`

Let

```lean
class LawfulStateOf (σ : Type) (computation : Type → Type)
    [Monad computation]
    [MonadStateOf σ computation] : Prop where
  stateOf_write_read :
    ((λ s => set s >>= λ _ => getThe σ) :
       σ → computation σ) =
      (pure)

export LawfulStateOf (stateOf_write_read)

attribute [simp] stateOf_write_read
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

### `statefulFibonacciPair`

Program `statefulFibonacciPair` below shows the effectfulness of programs with state by using the
initial state as initial (argument) value and modifying it.

Let

```lean
unsafe def fibonacciWithState
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithState Nat program] :
  program Unit Nat :=
    withInitialStateAsInitialValue fibonacci >=>
    modifyStateWith (λ σ => σ + 1)
```

We can now run this program with state.

```lean
unsafe def statefulFibonacciPair :
  Unit → Nat → (Nat × Nat) :=
    materializeActiveWithState
    (fibonacciWithState &&& fibonacciWithState)

#eval statefulFibonacciPair () 10
```

```lean
info: PSBP/All.lean:1253:0: (89, 144)
```

## Programming With Failure

### `WithFailure ε`

`PSBP` enables programming with failure using the `WithFailure` class below.

```lean
class WithFailure
    (ε : outParam Type)
    (program : Type → Type →Type) where
  failureWith {α β : Type} : (α → ε) → program α β

export WithFailure (failureWith)
```

### `instance WithFailure ε` (fast failure)

`WithFailure ε` is implemented in terms of `FailureT`, which is defined in terms of `⊕`. Given an initial (argument)
value, a program with failure may transform it to a final failure (result) value (at left) or a final succedd (result)
value (at right).

```lean
structure FailureT
    (ε : Type)
    (computation : Type → Type)
    (β : Type) : Type where
  toComputationOfSum : computation (ε ⊕ β)
```

```lean
instance [Monad computation] :
    Monad (FailureT ε computation) where
  map :=
  λ αfβ ⟨cεoα⟩  =>
    ⟨cεoα >>= λ εoα => match εoα with
      | (.inr α) => pure $ .inr (αfβ α)
      | (.inl ε) => pure $ .inl ε⟩
  pure :=
    λ α =>
      .mk (pure (Sum.inr α))
  bind :=
    λ ⟨cεoα⟩ αfftεcβ =>
      ⟨cεoα >>= λ εoα => match εoα with
        | .inr α  => (αfftεcβ α).toComputationOfSum
        | .inl ε  => pure (.inl ε)⟩

instance {ε : Type}
    [Applicative computation] :
  WithFailure ε
    (FromComputationValuedFunction
      (FailureT ε computation)) where
  failureWith :=
    λ αfε =>
      ⟨λ α =>
        ⟨pure $ Sum.inl $ αfε α⟩⟩
```

### `ProgramWithFailure`

```lean
abbrev ProgramWithFailure ε computation :=
  FromComputationValuedFunction (FailureT ε computation)

def materializeWithFailure
    [Monad computation] {α β : Type} :
  ProgramWithFailure ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ ⟨αftεcβ⟩ α =>
      (αftεcβ α).toComputationOfSum

def materializeActiveWithFailure {α β : Type} :
 ProgramWithFailure ε Active α β → α → (ε ⊕ β) :=
  materializeWithFailure
```

`instance Monad (FailureT ε computation)` and `materializeActiveWithFailure` above cause programs to a fail fast when a
first exception has been encountered. The examples below illustrate this.

### `safeDiv`

Let

```lean
def isNotZeroF: Nat → Bool :=
  λ n => n != 0

def unsafeDivF : Nat × Nat → Nat :=
  λ ⟨n, m⟩ => n / m
```

and

```lean
def isNotZero [Functional program] :
  program Nat Bool :=
    asProgram isNotZeroF

def unsafeDiv [Functional program] :
  program (Nat × Nat) Nat :=
    asProgram unsafeDivF
```

in

```lean
def safeDiv
    [Functional program]
    [Sequential program]
    [Creational program]
    [Conditional program]
    [WithFailure String program] :
  program (Nat × Nat) Nat :=
    if_ (second >=> isNotZero) unsafeDiv $
      else_ $
        failureWith (λ (n, m) => s!"{n}/{m}")
```

We can now run this program with fast failure.


```lean
unsafe def failFastSafeDiv :
  (Nat × Nat) → (String ⊕ Nat) :=
    materializeActiveWithFailure safeDiv

#eval failFastSafeDiv (10, 5)

#eval failFastSafeDiv (10, 2)

#eval failFastSafeDiv (10, 0)
```

```lean
info: PSBP/All.lean:1338:0: Sum.inr 2
info: PSBP/All.lean:1340:0: Sum.inr 5
info: PSBP/All.lean:1342:0: Sum.inl "10/0"
```

### `twiceSafeDiv`

```lean
def twiceSafeDiv
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure String program] :
  program ((Nat × Nat) × Nat) Nat :=
    (first >=> safeDiv) &&& second >=> safeDiv
```

We can now run this program with fast failure.

```lean
unsafe def failFastTwiceSafeDiv :
  ((Nat × Nat) × Nat) → (String ⊕ Nat) :=
    materializeActiveWithFailure twiceSafeDiv

#eval failFastTwiceSafeDiv ((10, 5), 2)

#eval failFastTwiceSafeDiv ((10, 0), 2)

#eval failFastTwiceSafeDiv ((10, 5), 0)
```

```lean
info: PSBP/All.lean:1357:0: Sum.inr 1
info: PSBP/All.lean:1359:0: Sum.inl "10/0"
info: PSBP/All.lean:1361:0: Sum.inl "2/0"
```

### `instance WithFailure ε` (validation)

What about accumulating exceptions instead of failing fast?

Accumulation is specified using a `Monoid` type class. For now, the neutral element `ν` is used, this means that only a
semigroup is required instead of a monoid.

```lean
class Monoid (μ : Type) where
  ν : μ
  combine : μ → μ → μ

export Monoid (ν combine)

infixl:60 " * " => combine
```

Accumulation can, for example, be implemented using the `List α` type.

```lean
instance : Monoid (List α) where
  ν := []
  combine := .append
```

```lean
instance
    [Functor computation] :
  Functor (FailureT ε computation) where
    map :=
     λ αfβ ⟨cεoα⟩ =>
       ⟨(λ εoα =>
           match εoα with
            | .inl ε => .inl ε
            | .inr α => .inr (αfβ α)) <$> cεoα
       ⟩

instance
    [Applicative computation]
    [Monoid ε] :
  Applicative (FailureT ε computation) where
    pure :=
      λ α =>
        ⟨pure $ .inr α⟩
    seq :=
      λ ⟨cεoαfβ⟩ ufftεcα =>
        let cεoα :=
          (ufftεcα ()).toComputationOfSum
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
        ⟨εoαfεoαfβfεoβ <$> cεoα <*> cεoαfβ⟩
```

### `ProgramWithValidation`

```lean
abbrev ProgramWithValidation ε computation :=
  FromComputationValuedFunction (FailureT ε computation)

def materializeWithValidation
    [Monad computation]
    [Monoid ε] {α β : Type} :
  ProgramWithValidation ε computation α β →
  α →
  computation (ε ⊕ β) :=
    λ ⟨αftεcβ⟩ α =>
      (αftεcβ α).toComputationOfSum

def materializeActiveWithValidation
    [Monoid ε] {α β : Type} :
 ProgramWithValidation ε Active α β → α → (ε ⊕ β) :=
  materializeWithValidation
```

`instance Functor (FailureT ε computation)`, `instance Applicative (FailureT ε computation)` and
`materializeActiveWithValidation` above cause programs to accumulate all exceptions that are encountered.

The examples below illustrate this accumulation behavior.

### `accumulatingSafeDiv` revisited

```lean
def accumulatingSafeDiv
    [Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program (Nat × Nat) Nat :=
    if_ (second >=> isNotZero) unsafeDiv $
      else_ $
        failureWith (λ (n, m) =>
          [s!"{n}/{m}"])
```

### `accumulatingSafeDivProduct`

```lean
def accumulatingSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) (Nat × Nat) :=
    (first >=> accumulatingSafeDiv) &&& (second >=>
    accumulatingSafeDiv)
```

We can now run this program with validation.

```lean
unsafe def validatingSafeDivProduct :
  ((Nat × Nat) × (Nat × Nat)) → (List String ⊕ (Nat × Nat)) :=
    materializeActiveWithValidation accumulatingSafeDivProduct

#eval validatingSafeDivProduct ((10, 5), (8, 2))

#eval validatingSafeDivProduct ((10, 0), (8, 2))

#eval validatingSafeDivProduct ((10, 5), (8, 0))

#eval validatingSafeDivProduct ((10, 0), (8, 0))
```

```lean
info: PSBP/All.lean:1454:0: Sum.inr (2, 4)
info: PSBP/All.lean:1456:0: Sum.inl ["10/0"]
info: PSBP/All.lean:1458:0: Sum.inl ["8/0"]
info: PSBP/All.lean:1460:0: Sum.inl ["10/0", "8/0"]
```

### `addAccumulatingSafeDivProduct`

```lean
def addAccumulatingSafeDivProduct
[Functional program]
    [Creational program]
    [Sequential program]
    [Conditional program]
    [WithFailure (List String) program] :
  program ((Nat × Nat) × (Nat × Nat)) Nat :=
    accumulatingSafeDivProduct >=>
    add
```
We can now run this program with validation.

```lean
unsafe def validatingSafeDivProduct :
  ((Nat × Nat) × (Nat × Nat)) → (List String ⊕ (Nat × Nat)) :=
    materializeActiveWithValidation accumulatingSafeDivProduct

#eval validatingSafeDivProduct ((10, 5), (8, 2))

#eval validatingSafeDivProduct ((10, 0), (8, 2))

#eval validatingSafeDivProduct ((10, 5), (8, 0))

#eval validatingSafeDivProduct ((10, 0), (8, 0))
```

```lean
info: PSBP/All.lean:1476:0: Sum.inr 6
info: PSBP/All.lean:1478:0: Sum.inl ["10/0"]
info: PSBP/All.lean:1480:0: Sum.inl ["8/0"]
info: PSBP/All.lean:1482:0: Sum.inl ["10/0", "8/0"]
```







