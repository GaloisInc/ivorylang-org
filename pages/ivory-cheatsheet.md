# Ivory Cheatsheet

This cheatsheet is to get you started with the most common constructs in Ivory.
Many constructs are not covered here; see the Ivory Haddock documentation (as
well as the Ivory standard library) for the full language definition.

** This cheatsheet is incomplete! **

## Types

### Values Types

- Boolean: `IBool`
- Char: `IChar`
- Int types:
    - Signed: `SX`, where `X` is `int8`, `int16`, `int32`, `int64`
        - Example: `Sint32`
    - Unsigned: `UX`, where `X` is `int8`, `int16`, `int32`, `int64`
        - Example: `Uint8`
- Float: `IFloat`
- Double: `IDouble`
- Void: `()`.  In practice, void is only used as a return type for functions.

Note: there are no machine-dependent types in Ivory, like `int`, `short int`,
`long int`.

### Memory area types

A memory area is allocated that has a type.

- Allocated value: `Stored X`, where `X` is a value type.
    - Example: `Stored Uint32`.

- Arrays: `Array n X`, where `n` is a number and `X` is a memory area type
    - Example: `Array 10 (Stored Sint32)`: array of 10 signed 32-bit integers.

- Structs: `Struct "name"`, where `name` is the string associated with the defined
  struct.
    - Example: `Struct "foo"`

### Pointer Types

Scope type is the scope of a pointer.  A scope is either `Local` if the memory
  area is allocated on the stack, `Global` if its allocated in global memory, or
  a type variable if you don't care.

References are non-null pointers.  You almost exclusively use references in
ivory.

Examples:

- `Ref Local
  (Stored Sint32)`: reference to a locally-allocated signed 32-bit int.
- `Ref Global
  (Struct "foo")`: reference to a globally-allocated struct named `"foo"`.

- `Ref s
  (Array 5 (Stored IBool)`: indeterminate allocation scope of an array of 5 Booleans.

### Index Types

Indexes are used to index into an array and for loop counters.  They have a
special type, `Ix n`, where `n` is a natural number.  An index type of `Ix n`
supports the values `0` through `n-1`.

- Example: `Ix 3`.

### Initializer Types

Types of initializers for memory areas.

- `Init X`, where `X` is a memory area type.
    - Example: `Init (Array 3 (Stored IBool))`: initializer type an array of
      three Booleans.

## Initializers

Used to initialize memory areas.

- Basic values: `ival`.
    - Example: `ival 3`
    - Basic values can be initialized to a default value (0 for integer types)
      using `izero`.
- Arrays: `iarray [i0, i1, ..., in]` for an array of length n.  Each element must be an
  initializer corresponding to the type of the array, and the length of the list
  be correspond to the length of the array.
    - Example: `iarray [ival 3, ival 4]` is an initializer with type (or
    whatever integer type we wish to interpret `3` and `4`.
```
Init (Array 2 (Stored Uint32))
```
    - Arrays can be initialized to a default value using `iarray []`

- Structs: assuming a struct is defined with the appropriate fields (see
  [struct definitions][structs]), `istruct
  [fname0 .= exp0, fname1 .= exp1, ...]` is an initializer with type `Init
  (Struct "Foo")` for struct `"Foo"`.
    - Example: `istruct [field0 .= 3; field1 .= 4]` which initializes the fields
      to `3` and `4`, respectively.  Fields can be partially initialized.
    - To initialize a struct with default values, use `istruct []`.

## Struct Definitions

Structs are defined at the top level as follows.  `Foo` is a fresh name for the
struct, and `field0`, `field1`, ... are fresh names for fields.  The types of
fields must be [memory area types][memtypes].

```haskell
[ivory|
struct Foo
  { field0 :: Stored Uint32
  ; field1 :: Array 10 (Stored IBool)
  }
|]
```

For the definition above, the following is generated automatically:

- The type `Struct "Foo"`
- Field accessors `field0` and `field1`

Note: there are no union types in Ivory.


## Expressions

- Numbers: the usual numeric constants
- Arithmetic: `+`, `-`, `*`
- Division: `/`: only defined for `IFloat` and `IDouble`
- Equality: `==?`, `/=?` (not equals)
- Numeric comparisons: `>?`, `>=?`, `<?`, `<=?`
- Quotient: `iDiv`: like C's `/` (truncation towards 0).
    - Example: `iDiv (-3) 2 .== (-1)`
- Remainder: `.%`: like C's `%`.
    - Example: `(-3) .% (-2) .== -1`
- Boolean: `true`, `false`, `.&&`, `.||`, `iNot`
- Bit operators: `.&`, `.|`, `.^`, `iComplement`, `iShiftL`, `iShiftR`
    - Example: `10 iShiftR 1 .== 5`
- Array dereference: `arr ! exp`, where `arr` is a reference to an array and
  `exp` is an [Ivory expression][exp].  Returns a reference to an element of an
  array.
    - Example:
```haskell
x0 <- local (iarray [ival 1, ival 2])
 x <- deref (x0 ! 1)
```
- Struct field indexing: `ref ~> fieldName` where `ref` is a reference to a
  struct and `fieldName` is a field of the struct (see
  [struct definitions][structs]).
    - Example:
```haskell
ref <- local (istruct [field0 .= 3; field1 .= 4])
f0  <- deref (ref ~> field0)
```
    - C:
```c
struct foo  = {field0 = 3; field1 = 4};
struct foo *ref = &foo;
uint32_t f0 = foo->field0;
```
- Choice: `exp0 ? (exp1, exp2)`, where `exp_i` is an [Ivory expression][exp].
    - Example: `(a > b) ? (a, b)`
    - C equivalent: `(a > b) ? a : b`

## Statements

Statements appear in an Ivory `do` block.

There are two kinds of statements:

- Binding statements: statements that bind a value to a fresh variable on the
left-hand side of a `<-` operator.  Assume that each left-hand variable shown is
fresh.  Example: `x <- assign (3 - 4)` (assign `x` the value `3-4`).

- Non-binding statements: statements that do not bind.  Example: `ret
  4` (return 4).

In the following

- `exp` is an [Ivory expression][exp].
- `ref` is a [reference][ref].
- `init` is an [Ivory initializer][init].

For each statement, we give an example and an equivalent in C.

- Assignment: `x <- assign exp`
    - Example: `x <- assign (a + 3)`
    - C: `uint32_t x = a + b;`

- Local allocation: `x <- local init`
    - Example: `x <- local (ival 3)`
    - C:
```c
uint32_t x0 = 3;
uint32_t *x = &x0;
```
    - Note: sometimes, Ivory can't figure out the type of an initializer, so you
      have to give it explicitly.  Example: `x <- local (iarray
      [ival 3, ival 4] :: Init (Array 2 (Stored Uint32)))`

- Dereferencing a memory area: `x <- deref ref`.
    - Example:
```haskell
x0 <- local (init (ival 3))
x  <- deref x0
```
    - C:
```c
uint32_t x0  = 3;
uint32_t *x1 = &x0;
uint32_t x   = *x1;
```

- Storing into a memory area: `store ref exp`.
    - Example:
```haskell
ref <- local (ival 3)
store ref 2
```
    - C:
```c
uint32_t x    = 3;
uint32_t *ref = &x;
         *ref = 2;
```

- Return: `ret exp`
    - Example: `ret (a >=? 1)`
    - C: `return (a >= 1);`

- Return Void: `retVoid`
    - Example: `retVoid`
    - C: `ret void;`

- Branching: `ifte_ exp stmts0 stmts1`, where `stmts0`, `stmts1` are blocks of
  Ivory statements
    - Example:
```haskell
ifte_ (a > 2)
  (do x <- assign (a + 4)
      ret x)
  (ret (a + 3))
```
    - C:
```c
if (a > 2) {
  x = a + 4;
  return x;
} else { return (a + 3); }
```

- Loops: There three kinds of loops.  Each one uses an [index][ix] as the loop
  counter.
    - Counting up: ``(ix :: Ix 4) `for` \currIx -> stmts``.  Counts from `0` to
      `n-1` for `Ix n`.
        - Example:
```haskell
(ix :: Ix 4) `for` \currIx -> $ do
  x <- deref ref
  store ref (x+1)
```
         - C:
```c
for (int currIx = 0; currIx <= 3; currIx++) {
  *ref = *ref + 1;
}
```
    - Counting down: ``(ix :: Ix 5) `times` \currIx -> stmts``.  Counts from `n-1`
       to `0` for `Ix n`.
        - Example:
```haskell
(ix :: Ix 4) `for` \currIx -> $ do
  x <- deref ref
  store ref (x+1)
```
         - C:
```c
for (int currIx = 3; currIx >= 0; currIx--) {
  *ref = *ref + 1;
}
```
         - C:
    - Map over an array: ``arrayMap $ \currIx -> stmts``
        - Example:
```haskell
arrayMap $ \ix -> do
  x <- deref (arr ! ix)
  store (arr ! ix) (x+1)
```
         - C: assuming the number of elements in the array is given by `len`,
```c
for (int ix = 0; ix < len; ix++) {
  arr[ix] = arr[ix] + 1
}
```

- Assertions: `assert exp`
    - Example: `assert (a > b)`
    - C: `assert(a > b);`

- Function calls: `x <- call func exp0 exp1 ... expn`.  See
  [function definitions][fn] below for how to define a function.
    - Example: `x <- call foo 3 (a + 2)
    - C: `int32_t x = foo(3, a+2);`
    - Note: if you want to ignore the return value, use `call_`.

## Functions

Ivory programs are encapsulated in functions.

### Function Types

A function type has the form ``Def ('[ArgsTypes] :-> RetType)`` where `ArgTypes`
are the types of the arguments and `RetType` is the return type.

Examples:

```haskell
fun0 :: Def ('[] :-> ())
fun1 :: Def ('[Uint32] :-> IBool)
fun2 :: Def ('[Ref s (Stored Uint32), IBool] :-> IBool)
```

### Function Implementations

Function definitions take a string that will become the name of the function in
the compiled C (it is good practice to make this string the same as the Haskell
name).  Then 

```haskell
func0 :: Def ('[Uint32] :-> Uint32)
func0 = proc "func0" $ \arg0 -> body $ do
  ret (arg0 + 2)
```
compiles to
```c
uint32_t func0(uint32_t arg0) {
  return (arg0 + 2);
}
```

```haskell
func1 :: Def ('[] :-> Uint32)
func1 = proc "func1" $ body $ do
  ret 2
```
compiles to
```c
uint32_t func1(void) {
  return 2;
}
```

```haskell
func2 :: Def ('[Ref s (Stored Uint32), Uint32] :-> ())
func2 = proc "func2" $ \ref val -> body $ do
  store ref val
  retVoid
```
compiles to
```c
uint32_t func2(uint32_t *ref, uint32_t val) {
  *ref = val;
  return;
}
```

## Modules

Functions and structures must be placed in a module to be compiled.  The
following module contains a function called `foo` and a structure with type
`Struct "Bar"`:

```haskell
aModule :: Module
aModule = package "MyPackage" $ do
  incl foo
  defStruct (Proxy :: Proxy "Bar")
```

## Compilation

The Ivory compiler is called by calling `compile [m0, m1, ... mn]`, where `m_i`
are [modules][moduledefs].

Assuming you are using GHCi (GHC interpreter), you can set arguments to the
compiler by running
```
> :set args ... args ...
```

To get the list of arguments accepted, run
```
> :set args --help
> compile []
```
To run the compiler on module `myModule` with division-by-zero and
integer-overflow checking, run
```
> :set args --div-zero --overflow
> compile [myModule]
```

## Ivory Source Files

At the top of your Haskell file in which you are writing Ivory, place
the following:

```haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

```


[exp]: ivory-cheatsheet.html#expressions
[init]: ivory-cheatsheet.html#initializers
[ix]: ivory-cheatsheet.html#index-types
[ref]: ivory-cheatsheet.html#pointer-types
[fn]: ivory-cheatsheet.html#function-definitions
[memtypes]: ivory-cheatsheet.html#memory-area-types
[structs]: ivory-cheatsheet.html#stuct-definitions
[moduledefs]: ivory-cheatsheet.html#modules
