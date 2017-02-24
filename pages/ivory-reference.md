# Ivory Language Reference

## Syntax

### expressions

### statements

### functions

### types

-- start, borrow from cheatsheet, REWRITEME

#### Values Types

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

#### Memory area types

A memory area is allocated that has a type.

- Allocated value: `Stored X`, where `X` is a value type.
    - Example: `Stored Uint32`.

- Arrays: `Array n X`, where `n` is a number and `X` is a memory area type
    - Example: `Array 10 (Stored Sint32)`: array of 10 signed 32-bit integers.

- Structs: `Struct "name"`, where `name` is the string associated with the defined
  struct.
    - Example: `Struct "foo"`

#### Pointer Types

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

#### Index Types

Indexes are used to index into an array and for loop counters.  They have a
special type, `Ix n`, where `n` is a natural number.  An index type of `Ix n`
supports the values `0` through `n-1`.

- Example: `Ix 3`.

#### Initializer Types

-- end borrowed from cheatsheet

### struct

  * Initialization syntax:
  {} means struct, as in `alloc s0{} = $stringInit("foo");`

### const

### alloc

### extern

### Pointer syntax

  - `(G|S|ε|c)*`, where `c` is a user defined identifier
  - `G` means global
  - `S` means stack
  - nothing means anything
  - giving it a name allows you to match it up elsewhere (not usually needed)

### Includes

  * bring other ivory modules/defs into scope

## Memory Areas

  * C's idea of automatic stack variables doesn't really exist.
    * instead you explicitly alloc on the stack and get a reference
    * then you can store to that reference if you want

### Pointer syntax explained

### `let` vs. `Stored` and `store`

### iterators

  * forever is a `while(1)` loop
  * map is built in array/index iterator
  * Go through fizzbuzz examples and to see what is available

## functions

  * similar to C functions
  * we support `void` return types and empty argument lists
  * also support returning values (stored?)

## Macros

  * Prefix the Haskell function name with `$`.
  * macros can be used where either an expression or a statement is expected
  * back arrow syntax for getting a value out of a macro

### Calling into Haskell

### back arrow

## Modules

## FFI with C

### Imports

  * take a path and a function
  * For FFI with C
  * can also be used with memory areas
  * Syntax seems different. Parens vs. non-parens.

## Translation to C

## Safety, casting, and conversion

  * safeCast is part of ivory. It considers overflow.

## Assertions, Preconditions, and Postconditions

## Ivory Standard Library

  * memcpy is C's memcpy, but it's built in to the syntax
  * lookup in the happy grammar the set of builtins
  * can't use parens for refCopy's and lee says that's bad

### Strings
  * stdlibStringModule
  * generates `ivory_string_FooStr` as a type
