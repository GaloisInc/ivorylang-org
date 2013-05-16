# Fib Loop Walkthrough

This is an informal tutorial in which we'll  present an Ivory program for
computing Fibonocci numbers, and walk through it line-by-line as a way of
discussing some of the basics of the Ivory language.

Ivory is embedded in the Haskell language. Since Ivory reuses the syntax
and type system of Haskell, you'll need to be comfortable with the Haskell
language before learning Ivory.

If you don't know Haskell, don't panic. Haskell can be difficult to learn
at first because some of the concepts and vocabulary are unfamiliar to many
programmers. However, we hope you'll 

There are a lot of good resources on the web for learning the Haskell. We
have recommended beginners read [Learn You A Haskell][lyah] followed by
working through at least some of [Real World Haskell][rwh]. Before going
much further, let's check-in on some concepts that should at least be
familar:

[lyah]: http://learnyouahaskell.com
[rwh]: http://book.realworldhaskell.org
[spb]: http://github.com/galoisinc/smaccmpilot-build

* Installing GHC and cabal without the Haskell Platform. You'll need GHC
  7.6.2. [See the prerequisites page for more on the topic](prerequisites.html).
* Navigating Haskell source files. You can [clone the smaccmpilot-build][spb]
  repository and open up the file defining the `Ivory.Language` module.
  (You'll need to, since its the only definitive document of the Ivory Language
  right now.)
* Good understanding of the type system. You are comfortable defining your
  own typeclasses.
* Monads & their syntax. Specifically, familiarity with the ST
  monad is helpful!

Helpful but not too hard to pick up in the context of Ivory:

* Type literals (Strings and Naturals)
* Type level lists

------------------

Here's an Ivory procedure which computes Fibonocci numbers by mutating values
on the stack in a loop. (We've annotated the program text with line numbers.)

```haskell
1.  fib_loop :: Def ('[Ix 1000] :-> Uint32)
2.  fib_loop  = proc "fib_loop" $ \ n -> body $ do
3.    a <- local (ival 0)
4.    b <- local (ival 0)
5.
6.    n `times` \ _ -> do
7.      a' <- deref a
8.      b' <- deref b
9.      store a b'
10.     store b (a' + b')
11.
12.  result <- deref a
13.  ret result
```

------------------------------------------------------------------

Let's break this program down line by line.

```haskell
1.  fib_loop :: Def ('[Ix 1000] :-> Uint32)
```

This is the type signature for `fib_loop`, indicating that it is an
Ivory `Def`. `Def` is an Ivory procedure, which is analogous to a C function.
All Ivory code must eventaully be part of a `Def` in order to be compiled.

`Def` is a type constructor with the argument `'[Ix 1000] :-> Uint32`.  Every
Def will have an argument with the form `a :-> b` : the `:->` symbol is an
infixed type constructor.  The first argument to `:->` is always a type level
list (note the leading bracket is prefixed with a single quote `'`) containing
zero or more types for the procedure arguments. The second argument to the
`:->` constructor is the return type of the procedure.

So, in this example, the `fib_loop` procedure takes a single argument of type
`Ix 1000` and returns a value of type `Uint32`.

The type `Ix 1000` is the type constructor `Ix` applied to the type level
natural `1000`. Ix stands for *Index* it is used to construct a type for a
value which is always less than the argument (and never less than 0). So, the
Ivory type `Ix 1000` is for a value in the range `0 =< value < 1000`.

We use `Ix` types anytime we need to perform a looping operation so that we
ensure the loop's execution is bounded, and anytime we need to index into an
array to make sure we only access memory within the array's bounds. More on
this later.

The return type of the `fib_loop` procedure is `Uint32`. This is what you'd
expect from the C world: an unsigned 32 bit integer. Ivory has unsigned and
signed integers (type names starting with `Uint` and `Sint`, respectively) with
bit sizes 8, 16, 32, and 64. This is designed to mimic the C `stdint.h` types.

Ivory `Def`s don't have to return a value. A procedure may have a return type
`()` (pronounced "unit") to indicate the procedure won't be returning a value.
This is equivelant to a function with a `void` return type in C.

------------------------------

Ready for the next line?

```haskell
2.  fib_loop  = proc "fib_loop" $ \ n -> body $ do
```

This starts off our definition of the `fib_loop` procedure.

The function `proc` takes a string and a function for the procedure body and
returns a `Def`. The string we pass to proc - here, `"fib_loop"`, is the name
the procedure will have when compiled. So, we know that the Ivory `Def` called
`fib_loop` will also be compiled to a C function called `fib_loop`.

The second argument to `proc` is an anonymous haskell function which takes an
argument `n` and returns a the value from `body` on. The type of `n` is given
by the type arguments to the `Def` declared above. We noted that the procedure
argument types were given as `'[Ix 1000]`, so here proc expects a function
from a single value (here, `n`) with type `Ix 1000`.

Procedures can take many arguments. If `fib_loop` had the type `Def ('[Ix 1000,
Sint8] :-> Uint32)` we would need to give `proc` a function with two arguments
: say, `\ n somechar -> body $ do ...`. In this example, `n` would still take
type `Ix 1000`, and the second argument `somechar` would take type `Sint8`.

Once we have an anonymous function to take care of the procedure arguments,
we construct a procedure `body`. The Ivory function `body` takes an argument
of type `Ivory s ()`. The `Ivory s` type is a Monad, so we construct the value
`Ivory s ()` with a `do` block. And, why does


