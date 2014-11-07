# Ivory Language Concepts

## An Embedded Language

We will discuss how Ivory has several restrictions which make it less expressive
than the C programming language. On their own, these restrictions would make it
quite laborious to build programs in Ivory. However, Ivory has been built as an
[embedded][] language inside the Haskell programming language. This means Ivory
programs can be constructed using the Haskell language as a macro language.

Rather than inventing its own syntax and type system, the Ivory language reuses
the syntax and type system of Haskell.  So, when we refer to programs in the
Ivory language, we're really talking about a Haskell value which is constructed
using the Haskell library `Ivory.Language`.  These values can then be
interpreted by the Ivory interpreter, or compiled by an Ivory language backend.
Currently, the Ivory language has a single backend which produces C source
files.

Because Ivory programs use Haskell syntax and types, an Ivory programmer
should first have basic familiarity with the Haskell programming language.

## A Systems Language

Ivory is a systems language designed for a natural compilation to C. Most of the
concepts familiar to a C programmer are present in Ivory: procedures, return
values, machine-sized integers and floating point numbers, structures, arrays,
and strings are all available in the Ivory language.

The Ivory language maps naturally to a restricted subset of C, and has a
compiler backend to output C source code. Ivory also supports importing and
calling external C code, and coercing Ivory types to C types.

Ivory is designed for implementing systems and application software for high
assurance systems. Therefore, some language trade-offs have been made so that
programs can have safety properties guaranteed by construction. Ivory's feature
restrictions eliminate many valid, correct programs which are possible to write
in C.  In general, Ivory eliminates many sources of dynamic behavior in favor of
safety. Average case performance is often sacrificed in order to bound the worst
case; the programmer can expect compiled Ivory programs to perhaps use more
memory, code, or time than functionally equivalent safe programs written in C.

While this means Ivory may not be a good language for all applications, Within
the domain of creating high assurance software, these trade-offs are what make
Ivory ideal.

## Differences from the C Language

One major difference between Ivory and C is the treatment of memory allocation
and reference. Ivory does not allow nullable pointers, unbounded memory
access, or heap allocation. These restrictions are made with safety and security
in mind. Nullable pointers may complicate control flow, which makes it more
difficult for the user to create a correct program. Unbounded memory access
is difficult for both users and verification tools to reason about.
Heap allocation is difficult to implement without nullable pointers, and
may create problems reasoning about time (i.e. allocator complexity) and
space (i.e. memory leaks, allocation bounds) of user code. Ivory permits
global allocation and stack allocation, and enforces safety of stack allocated
memory by preventing references from existing after the referent's stack
frame no longer exists.

In addition to treating memory more carefully than C, Ivory makes restrictions
on loop control structures to ensure each loop has a fixed upper bound. We
expect many Ivory users will want to use an external Worst-Case Execution Time
(WCET) tool as part of safety or schedulability analysis, therefore, we have
ensured that it will be trivial for external tools to discover loop bounds.

It is possible to create nonterminating programs in Ivory using the `forever`
control flow primitive, which is designed for implementing programs which
*should never* terminate.  It is also possible to build nonterminating programs
by call recursion.  At this time, the use of forever and recursion are allowed
by the Ivory compiler, but these features may be restricted or removed in the
future.

## Support for Verification

Ivory was created to improve safety for systems and application programmers.
While the Ivory language does guarantee some safety properties by construction,
it also supports specifying run-time properties using external provers.

```haskell
add :: Def ('[Uint32,Uint32] :-> Uint32)
add  = proc "add"
     $ \ x y -> ensures (\r -> r ==? x + y)
     $ body
     $ ret (x + y)

```
*A sample ivory program decorated with an `ensures` clause, checking the return
value against a specification. This is a trivial case where the specification
and implementation are identical.*


The C language backend supports rendering Ivory assertions for static checking
with the [CBMC model checker][cbmc]. The SMACCMPilot project build system
includes integration for CBMC verification of the SMACCMPilot source code.

We also provide our own [symbolic simulator][mc] for verifying Ivory assertions using
[CVC4][cvc4].

Further documentation on Ivory's verification support is forthcoming.

[embedded]:http://wikipedia.com/wiki/Domain-specific_language#Domain-specific_language_topics
[cbmc]: http://www.cprover.org/cbmc/
[mc]: https://github.com/GaloisInc/ivory/tree/master/ivory-model-check
[cvc4]: http://cvc4.cs.nyu.edu/web/
