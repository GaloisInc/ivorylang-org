# Ivory Tutorial

This is an informal tutorial on the Ivory Language. We will present an Ivory
program for computing Fibonacci numbers, and walk through it line-by-line as a
way of discussing some of the basics of the Ivory language.

Ivory is embedded in the Haskell language: this means that Ivory reuses the
syntax and type system of Haskell. It is best if you are comfortable with the
Haskell language before learning Ivory.

If you don't know Haskell, don't panic. There are a lot of good resources on the
web for learning the Haskell. We have recommended beginners read [Learn You A
Haskell][lyah] followed by working through at least some of [Real World
Haskell][rwh].

Before going much further, let's check in on some Haskell concepts that should
at least be familar:

[lyah]: http://learnyouahaskell.com
[rwh]: http://book.realworldhaskell.org
[spb]: http://github.com/galoisinc/smaccmpilot-build

* Installing GHC and cabal. Ivory supports GHC 7.6.3 and the 7.8 series.
* Good understanding of the type system. You are comfortable defining your
  own typeclasses, and perhaps you've dealt with existential quantification.
* Monad concepts and syntax; Imperative style in Haskell. You understand how to
  use the `Data.IORef` library.

------------------

If you'd like to follow along with this tutorial in `ghci`, you can [get the
Ivory repository on github][github], and [find this tutorial][tutorial-github]
in the [ivory-examples][ivory-examples] package.

[github]: http://github.com/galoisinc/ivory
[tutorial-github]: http://github.com/GaloisInc/ivory/blob/master/ivory-examples/examples/FibTutorial.hs
[ivory-examples]: http://github.com/GaloisInc/ivory/blob/master/ivory-examples/ivory-examples.cabal

------------------

Here's an Ivory procedure which computes Fibonacci numbers by mutating values
on the stack in a loop.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Ivory.Language
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

fib_loop :: Def ('[Ix 1000] :-> Uint32)
fib_loop  = proc "fib_loop" $ \ n -> body $ do
  a <- local (ival 0)
  b <- local (ival 1)

  n `times` \ _ -> do
    a' <- deref a
    b' <- deref b
    store a b'
    store b (a' + b')

  result <- deref a
  ret result

fib_tutorial_module :: Module
fib_tutorial_module = package "fib_tutorial" $ do
  incl fib_loop

main :: IO ()
main = C.compile [ fib_tutorial_module ]

```

------------------------------------------------------------------

Let's break this program down line by line.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
```

The Ivory language type system uses the DataKinds and TypeOperators extensions
to Haskell. You'll need to use these Language pragmas in any program defining
an Ivory `Def`.

In other parts of the Ivory language, you'll need the QuasiQuotes and
FlexibleInstances extensions as well. This particular program doesn't require
them.

--------

```haskell
import Ivory.Language
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
```

The `Ivory.Language` module exports the public interface of the Ivory language.
The `Ivory.Compile.C.CmdlineFrontend` module provides a function `compile` which
will allow us to run this Haskell program as a command-line application for
compiling the Ivory programs contained within.

--------

OK, time for some actual Ivory code.

```haskell
fib_loop :: Def ('[Ix 1000] :-> Uint32)
```

This is the type signature for `fib_loop`, indicating that it is an
Ivory `Def`. `Def` is an Ivory procedure, which is analogous to a C function.
All Ivory code must eventaully be part of a `Def` in order to be compiled.

`Def` is a type constructor with the argument `'[Ix 1000] :-> Uint32`.  Every
Def will have an argument with the form `a :-> b` : the `:->` symbol is an
infixed type constructor.  The argument type of `:->` is always a type level
list (note the leading bracket is prefixed with a single quote `'`) containing
zero or more types for the procedure arguments. The return type of the
`:->` constructor is the return type of the procedure.

So, in this example, the `fib_loop` procedure takes a single argument of type
`Ix 1000` and returns a value of type `Uint32`.

The type `Ix 1000` is the type constructor `Ix` applied to the type level
natural `1000`. Ix stands for *Index*, and constructs a type for a value which is
never less than than zero and always less than the argument to Ix.
So, `Ix 1000` is the type for a value in the range `0 =< value < 1000`.

We use `Ix` types anytime we need to perform a looping operation so that we
ensure the loop's execution is bounded, and anytime we need to index into an
array to make sure we only access memory within the array's bounds. More on
this later.

The return type of the `fib_loop` procedure is `Uint32`. This is what you'd
expect from the C world: an unsigned 32 bit integer. Ivory has unsigned and
signed integers (type names starting with `Uint` and `Sint`, respectively) with
bit sizes 8, 16, 32, and 64. This is designed to mimic the C `stdint.h` types.

Ivory `Def`s don't have to return a value: they may have return type `()`. This
is equivelant to a function with a `void` return type in C.

------------------------------

Ready for the next line?

```haskell
fib_loop  = proc "fib_loop" $ \ n -> body $ do
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
of type `Ivory eff ()`. The `Ivory eff` type is a Monad, so we construct the value
`Ivory eff ()` with a `do` block.

The `Ivory` constructor's first argument will typically be written as
`eff`, or sometimes `s`. We call this the "Effect Scope". This is the part of
the type which allows us to enforce memory safety and function return type
safety. It works much in the same way the `s` parameter works in the `ST s a`
monad. Read more on the [haskell wiki][haskellwikist] or in [the paper
introducing this technique][lfst].


[haskellwikist]: http://www.haskell.org/haskellwiki/Monad/ST
[lfst]:http://research.microsoft.com/en-us/um/people/simonpj/papers/lazy-functional-state-threads.ps.Z

-----

We've given a lot of background so far. The rest of the program is easier, we
promise.

```haskell
   a <- local (ival 0)
   b <- local (ival 1)
```

These are the first lines in the `Ivory eff` monad introduced above.

The function `local` takes an initial value and creates mutable variable on
the stack. It gives an Ivory `Ref` to that stack variable. So, here `a` and
`b` get a value of type `Ref s a`. The type variable `s` is how we track the
scope this Ref belongs to. The type system will make sure any `Ref` never
escapes out of the `Ivory` effect scope it was defined in.

For more details, follow the types in the definition of `local` in
the `Ivory.Language.Init` module.

The type variable `a` (not to be confused by the Haskell value named `a`) is the
type of the value refered to. The Haskell type checker will infer the type of
`a` and `b` to be `Ref s Uint32`.

----------

Now that we have some local state we can mutate, its time to make a loop in
which we mutate it.

```haskell
  n `times` \ _ -> do
```

As discussed above, the variable `n` has type `Ix 1000`. All of the looping
primitives in Ivory take an `Ix` type so that a static upper bound is known.

One of the looping primitives in Ivory is `times`. It does what you might
expect: given an `Ix`-typed value, it runs the loop body the number of times
specified. The loop body here is a function from `Ix 1000` to `Ivory eff ()`:
`times` will provide a value to the loop body giving the number of times the
loop has been run. In this example, we don't care what the current iteration
of the loop is, so we discard this argument using `_`.

So, we now have a loop which will be run the number of times specified by the
procedure argument `n`, and no more than 1000 times.

Note that we only deal with loops on *values* rather than *references*: this is
how we ensure that the loop body does not mutate the loop counter, which could
cause non-termination. This is one example of how we've restricted the power
of C to enforce safety.

----------

Here is the loop again, with the loop body:

```haskell
  n `times` \ _ -> do
    a' <- deref a
    b' <- deref b
    store a b'
    store b (a' + b')
```

The loop body does a series of operations on the stack variables at
`Ref`s `a` and `b`.

Ivory `Ref`s are a lot like Haskell's `Data.IORef`: they support reading
(via `deref`) and writing (via `store`) with ordering enforced by the enclosing
`Ivory` monad.

In lines 7 and 8, we read the current value stored at the `Ref` with `deref`.
That value is bound to the Haskell variables `a'` and `b'`. Since `a` has type
`Ref s Uint32`, `a'` has type `Uint32`.

In lines 9 and 10, we `store` new values into the references `a` and `b`. Note
that can use the ordinary Haskell Prelude function `+` on values of type `Uint32`.
Ivory integers and floats are instances of the Haskell `Num` typeclass, so any
purely functional code on Ivory integers and floats uses the same syntax as you
would with Haskell integers and floats.

----------

```haskell
  result <- deref a
  ret result
```

After the loop is complete, we have a value stored in `a` which is the `n`th
Fibonacci number. Just as the statements `deref` and `store` in the loop body
were run sequentially in the `Ivory` monad, the `deref` on line 12 will be run
sequentially after the loop is complete.

The `Ivory` statement `ret` takes a value of the procedure return type,
`Uint32`, and terminates the procedure with that return value. This is
analogous to the `return` keyword in C.

Remember when we said above the Haskell type checker will infer `a` and `b` to
have type `Uint32`?  The `ret` function on line 13 tells the type checker that
the type of `result` on like 12 is the result type of the procedure from line
1. It follows that `a` must be a Ref to that type, `a'` must have that type, and
through the use of `+` on line 10, `b'` also must have that type.

This is a nice example of how using Haskell gives us strong guarantees of type
correctness without having to write out a lot of type information explicitly.

---

We've now made a complete Ivory `Def` called `fib_loop`, but we can't yet do
anything with it.

```haskell
fib_tutorial_module :: Module
fib_tutorial_module = package "fib_tutorial" $ do
  incl fib_loop
```

In Ivory, the unit of compilation is the `Module`. Modules are Haskell values
which collect up `Def`s, user defined structures, and global memory declarations
for compilation.

When compiling to C, `Modules` are compiled to a pair of header and
implementation files with a name specified by the first argument to `package`.
So, this Module will produce the files `fib_tutorial.h` and `fib_tutorial.c`.

The second argument to `package` is a Monad of type `ModuleDef`. The Monad class
here is just to provide a convenient syntax: really, `ModuleDef`s could be lists
or some other Monoid.

The `incl` function takes an Ivory `Def` and adds it to a `ModuleDef`. The
result is that the procedure `fib_loop` will be compiled to a C function,
exported by the `fib_tutorial.h` header, and implemented in `fib_tutorial.c`.

Note that it is a responsibility of the user to make sure all procedures in a
`Module` have a unique `proc` name. So, if you duplicate the value `fib_loop' =
fib_loop` under a separate Haskell name, an `incl fib_loop'` in this `Module`,
the result would be generating two C functions named `uint32_t
fib_loop(int32_t)` in the same C file. While this would be a valid Ivory
compilation, it would cause your C compilation to fail.

---

At last, we have a Haskell program which implements an Ivory function and has it
packaged up for compilation.

```haskell
main :: IO ()
main = C.compile [ fib_tutorial_module ]
```

The function `C.compile` takes a `[Module]` and produces an `IO ()` which
compiles the given modules to C and writes the result to the disk.

By default he compiler will write the generated C files to the current
directory. However, it can also parse command line arguments for setting
compilation flags for the Ivory compiler. Try running the executable with the
`--help` flag to see some options.

---

### What's Next?

One good next step to learning the Ivory Language is to go through the contents
of the [`ivory-examples`][ivory-examples] package to see some more examples and
examine some of the generated C sources.

You can also take a look at the [SMACCMPilot project](http://smaccmpilot.org),
a large software project built using the Ivory and Tower languages.

[ivory-examples]: http://github.com/GaloisInc/ivory/tree/master/ivory-examples

