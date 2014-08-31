
## Ivory Concepts

#### Ivory is an embedded domain-specific language for safer systems programming.

That's a mouthful. Lets break that down:

* *Embedded*: Ivory is implemented as a library of [the Haskell programming
  language][haskell]. Ivory programs are written using Haskell syntax and types.
* *Domain-specific Language*: Ivory is not a general purpose programming
  language.  It aims to be a good language for writing a restricted subset of
  programs.
* *Safer*: Ivory gives strong guarantees of type and memory safety, and has
  features which allow the programmer to specify other safety properties.
* *Systems Programming*: Ivory is well suited for writing programs which
  interact directly with hardware and do not require dynamic memory allocation.

You can consider Ivory to be a lot like a restricted version of the C
programming language, embedded in Haskell.

[Continue to a more detailed treatment of Ivory's core concepts](ivory-concepts.html).

[haskell]: http://haskell.org

## Learning Ivory

Ivory is embedded in the Haskell language: this means that Ivory reuses the
syntax and type system of Haskell. It is best if you are comfortable with the
Haskell language before learning Ivory. In particular, Ivory uses modern
extensions to the Haskell language, such as DataKinds and TypeOperators.

Once you [understand the goals of the Ivory language](ivory-concepts.html),
start by reading about the [Ivory Toolchain](ivory-tools.html) and [common libraries](ivory-libs.html).

Additionally, you can learn Ivory by:

* [Following a tutorial](ivory-fib.html)
* [Reading example programs][ivory-examples]
* [Reading the SMACCMPilot source code][ivory-smaccmpilot]
* [Reading the language sources][ivory-lang]

[ivory-examples]: http://github.com/GaloisInc/ivory/tree/master/ivory-examples/examples
[ivory-smaccmpilot]: http://github.com/GaloisInc/smaccmpilot-stm32f4/tree/master/src/flight
[ivory-lang]: http://github.com/GaloisInc/ivory/tree/master/ivory/src/Ivory

## Example Programs

### Hello World

```haskell
puts :: Def ('[IString] :-> Sint32)
puts  = importProc "puts" "stdio.h"

main :: Def ('[] :-> ())
main  = proc "main" $ body $ do
  call_ puts "hello, world\n"
  retVoid
```
*Hello World in Ivory.*

In this example, we tell Ivory about an external procedure called `puts` in
`stdio.h`. The `puts` procedure takes an `IString` (the Ivory type for strings)
as an argument, and returns a `Sint32` (a signed 32 bit integer).

Then, we create a procedure called `main` which takes no arguments and returns
nothing of interest. The procedure body makes a call to `puts`, supplying the
string `"hello, world\n"` as an argument. The underscore in `call_` indicates
the result of the procedure is discarded.

After the call, the next statement in `main` is `retVoid`, which causes the
procedure to exit.

### Fibonacci

```haskell
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
```
*An Ivory program for computing Fibonacci numbers using a loop and mutable
state*


This example is explained in depth in our [Ivory Language tutorial](ivory-fib.html).
