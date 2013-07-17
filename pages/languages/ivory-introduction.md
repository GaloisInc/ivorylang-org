# Ivory Language: Intrudiction

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

For a more detailed treatment of these topics, see the [Ivory
Concepts](ivory-concepts.html) page.

[haskell]: http://haskell.org

## Examples

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
  b <- local (ival 0)

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
