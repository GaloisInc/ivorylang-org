# Ivory Language Reference

## Syntax

### Comments

Ivory supports single line comments that start with `--` and go to the end of
the line. Ex:
```
-- This is a trivial function:
void f () {}
```

### Keywords

* keywords: `if`, `else`, `assert`, `assume`, `pre`, `post`, `let`, `return`,
`alloc`, `store`, `break`, `const`, `struct`, `abstract`, `type`, `include`,
`import`, `extern`, `bitdata`, `as`
  
* Reserved identifiers used in the Ivory standard library (always in scope):
`string`, `memcpy`, `abs`, `signum`, `exp`, `sqrt`, `log`, `pow`, `div`, `sin`,
`cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `sinh`, `cosh`, `tanh`, `asinh`,
`acosh`, `atanh`, `isnan`, `isinf`, `round`, `ceil`, `floor`, `fromIx`,
`ixSize`, `toIx`, `toCArray`, `arrayLen`, `sizeOf`, `nullPtr`, `refToPtr`

* casting operations:
`safeCast`, `bitCast`, `castWith`, `twosCompCast`, `twosCompRep`

* Iterators:
`map`, `upTo`, `upFromTo`, `downFrom`, `downFromTo`, `forever`

## Reserved Symbols

The follow symbols are reserved in Ivory, most of which will have a familiar meaning for
C programmers:
`$`, `::`, `?`, `:`, `.`, `->`, `==`, `!=`, `*`, `/`, `+`, `-`, `%`, `=`, `<`,
`<=`, `>=`, `>`, `|`, `&`, `^`, `~`, `!`, `&&`, `||`, `<<`, `>>`, `(`, `)`,
`}`, `{`, `[`, `]`, `;`, `,`, `@`, `<-`, `_`, `#`

Definitions in Ivory programs needs to appear at the top-level. These include:
function definitions, include and import statements, struct, type, and bitdata
definitions, area definitions, and constants.

### expressions

Expressions in Ivory are similar to expressions in C with the addition of
macros are areas. In Ivory, a reference can be dereference in two ways: a)
first, like a C dereference where it evaluates to the value stored at the
reference, b) second, to get a new reference similar to a C dereference
followed by the address of (`&`) operator. Ivory references are never null.

An example, of a C-style dereference for an area, would be:

```
myArray[i] -- returns the value stored in the array at position i
myStruct->f -- returns the value stored in field f of the struct
```

Examples of Ivory dereferences that return references would be:
```
myArray@i -- returns a reference to the ith position of the array
myStruct.f -- returns a reference to the field f of the struct
```

The full grammar for Ivory expressions follows:

```
<exp> ::= <number>
        | <string>
        | <identifier>
        | '(' <exp> ')'
        -- areas
        | '*' <exp>             -- deref
        | <exp> '@' <exp>       -- array indexing, arr@ix (returns reference)
        | <exp> '[' <exp> ']'   -- array index and derference
        | <exp> '.' <exp>       -- struct field access (returns reference)
        | <exp> '->' <exp>      -- struct field access and dereference
        | '&' <identifier>      -- address of operator
        -- macros
        | '$' <identifier> [ '(' [<exp> ',']* ')' ]
        -- function calls
        | <identifier> '(' [<exp> ',']* ')'
        -- Unary ops
        | <unaryop> <exp>
        -- Binary ops
        | <exp> <binop> <exp>
        | <exp> '?' <exp> ':' <exp>

<unaryop> ::= '!' | '-' | '~'

<binop> ::= '||' | '&&' | '|' | '^' | '&' | '<<' | '>>' | '==' | '!='
          | '<' | '=<' | '>' | '>=' | '+' | '-' | '*' | '/' | '%'
```

Note: Two important syntactic distinctions between macro invocations and
function calls are, a) the macro names need to be prefix with `$` (eg., `$foo`)
and b) function calls must always be followed by an argument list, which may be
empty (eg., `()`), but macro invocations do not require an argument list.

### statements

Keeping with the similarity to C syntax, statements end with a semicolon (`;`).
Notable differences between C syntax and Ivory syntax for statements:

* `store`-statements take the place of C-style assignments.
* `assume` and `assert` statements for stating pre-conditions and invariants.
  If Ivory can statically prove assertions always hold they will be omitted in
  the generated C. Otherwise a call to C's `assert` macro will be generated.
* `let` for working with values without creating a reference.
* `alloc`-statements take the place of `malloc`.
* `memcpy` is a builtin operator instead of a library function.
* macro invocation: The macro `foo` would be invoked as `$foo`. Additionally,
  if the macro produces a value, then the backarrow (`<-`) can be used to assign
  a name to the value. Ex: `x <- $foo(1);`

Ivory also supports block statements, but not all of C's block statements. For
example, Ivory has no `switch` statement. The Block statements in ivory are
either an iterator usage (`map`, `upTo`, `forever`, etc) or an if-statement.

```
<simpleStmt> ::= 'assert' <exp> ';'
               | 'assume' <exp> ';'
               | 'let' [<type>] <identifier> '=' <exp> ';'
               | 'return' [<exp>] ';'
               | 'memcpy' <identifier> <identifier> ';'
               | <allocRef> -- see the `alloc` section
               | 'store' <exp> 'as' <exp> ';'
               -- function calls
               | <identifier> ['(' [<exp> ',']*')']
               | '$' <identifier> ['(' [<exp> ',']*')']
               -- binding the value returned from a macro invocation
               | <identifier> '<-' '$' <identifier> ['(' [<exp> ',']* ')']
               | 'break' ';'

<blkStmt> ::= 'map' <identifier> '{' <stmts> '}'
            | 'upTo' <exp> <identifier> '{' <stmts> '}'
            | 'downFrom' <exp> <identifier> '{' <stmts> '}'
            | 'upFromTo' '(' <exp> ',' <exp> ')' <identifier> '{' <stmts> '}'
            | 'downFromTo' '(' <exp> ',' <exp> ')' <identifier> '{' <stmts> '}'
            | 'forever' '{' <stmts> '}'
            | 'if' <exp> '{' <stmts> '}' 'else' '{' <stmts> '}'

<stmts> ::= <stmts> (<simpleStmt> | <blkStmt>)

```

### functions

Functions in Ivory are similar to C functions. The body of functions may only
contain statements and not definitions. The simplest ivory function would be:

```
void f() {}
```

Here is a more interesting function with pre- and post-conditions:

```
uint32_t g(uint32_t a) {
  return a + 4;
}
{ pre(a < 4);
  pre(a > 0);
  post(return > 5);
}
```

The syntax for functions is as follows:

```
<procDef>    ::= <type> <identifier> '(' <args> ')' '{' <stmts> '}' <prePostBlk>
<args>       ::= [ <type> <identifier> ',' ]*
<prePostBlk> ::= [ '{' <prePosts> '}' ]
<prePosts>   ::= [ <prePost> ';' ]*
<prePost>    ::= 'pre'  '(' <exp> ')'
               | 'post' '(' <exp> ')'
```

#### Pre and post conditions

Function definitions in Ivory may be followed by optional pre and post
conditions on the function. These conditions are written as Ivory expressions
and may mention the parameters to the function or the return value of the
function. In a post-condition, the `return` keyword has a special meaning and
represents the value returned by the function. In the example above, the
post-condition `post(return > 5)` states that the value returned by the
function will be greater than `5`.  In addition to expressions on values, the
conditions can also use Ivory references.

Multiple pre or post conditions may be provided for a function, in which case
they must all hold.

Ivory supports using an external SMT solver to automatically check pre and post
conditions, but this topic is not covered here.

### types

#### Values Types
Ivory has many of the same basic types you would expect in C:
`void`, `bool`, `char`, `float`, `double`, `int8_t`, `int16_t`, `int32_t`,
`int64_t`, `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t`

Note: there are no machine-dependent types in Ivory, like `int`, `short int`,
`long int`.

#### Memory area types

All allocated data in Ivory is associated with an area.

The first area is the Global area. The Global area is always in scope and exists
for the whole lifetime of the Ivory program. The Global area is abbreviated to
`G` when it appears in a reference's type.

The second area is the Stack area (abbreviate `S`). Each function gets its own
Stack area and this area refers to the stack space of the C stack for the
function.

All references in Ivory are associated with an area. The area qualifier for
references must fit the following rules:

  * No area specified: This reference may point to memory in any area.
  * `G`: This reference always points to memory in the Global area.
  * `S`: This reference always points to memory in the Stack area.
  * user specified name: The area the reference points to is given a name, which
    can be used elsewhere if two references need to refer to the same area.

Examples:

```
-- Stack allocated variable.
uint32_t deref_stack(S*uint32_t i) {
  return *i;
}

-- Global allocated variable.
uint32_t deref_global(G*uint32_t i) {
  return *i;
} 
```

If the area qualifier is missing in the type, then that reference is allowed to
point to any area.

#### Index Types

Indexes are used to index into an array and for loop counters.  They have the
special type, `ix_t n`, where `n` is a natural number. An index type of `ix_t
n` supports the values `0` through `n-1`. Any values manually assigned to the
index will be taken modulo `n`, so that they are in the correct range.

#### bitdata

These are basically enumerations where the total size can be specified in bits
and the size of each enumerated value can be specified along with its value.

-- TODO: actually, they are more than that. They work more like ADTs of the
above.

### struct

Structs come in three varieties in Ivory:

  * C-style structs: create a C-style aggregations of types. 
  * string
  * abstract

  * Declaration syntax:
```
<structDef> ::= 'struct' <identifier> '{' [ <type> <identifier> ';']* '}'
                  | 'string' 'struct' <identifier> <integer>
                  | 'abstract' 'struct' <identifier> <string>
```

Note: Even though Ivory has structs similar to C-style structs, there is an
important difference to be ware of. The fields of a struct must be explicit
about how the value is stored. If the value is stored directly in the struct,
the type of the field needs to begin with a `&` character. For the basic types,
such as `int8_t`, Ivory can infer that the struct will need to store the value
and hence the `&` may be omitted.

#### string structs

The string variant of structs is for creating fixed size arrays with two
fields. The first field for tracking the current length of the string and the
other field that holds the elements up to the declared maximum size. Ivory
provides a standard library of functions for working with strings,
`stblibStringModule`.

  * Ex initialization:
  `alloc s{} = $stringInit("foo");`

Fully understand the usage of the string module requires reading the Haskell
code, but here we cover some of the basics.

Declaring a string:
```
include stdlibStringModule -- Bring the stdlibStringModlue into scope
string struct MyString 4
```

This will create a type named `ivory_string_MyString`. For example, a function
that takes `MyString` value as a parameter would look like this:
```
void str_example(* struct ivory_string_MyString s)
{
 -- do something with the string
}
```

The macro `string_lit_store` can be used to store a string into a string
variable.

Example macros in the stdLibStringModule:

* `stringInit`: allocates a new string
* `string_lit_store`: stores a string literal into a string variable
* `string_lit_array`: stores a string literal into an array of `uint8_t`

#### abstract structs

Abstract structs are used as pointers to structs that exist external to the
Ivory program, such as those declared in C. The last parameter to declare an
abstract struct is the filename of the C declaration for linkage purposes.

### Constants

Constant definitions may only appear at the toplevel of an Ivory program and
defines a value that is in scope for the rest of the Ivory program. Constants
have the following syntax:

```
<constDef> ::= [ <type> ] <identifier> '=' <exp> ';'
```

### alloc

Ivory supports allocation syntax for references, arrays, and structs.

```
<allocRef> ::= 'alloc' '*' <identifier> [ '=' <exp> ] ';'
             | 'alloc' <identifier> '[' ']' [ '=' <arrInit> ] ';'
             | 'alloc' <identifier> [ '=' <structInit> ] ';'
<arrInit> ::= '{' [ <exp> ',' ]* '}'
<structInit> ::= '$' <identifier> [ '(' [<exp> ',']* ')' ] -- macro initialized
               | '{' [ <identifier> '=' <exp> ',' ]* '}'
```

### Includes, imports, and externs

Definitions from other Ivory modules are brought into scope using `include`.

```
<includeDef>  ::= 'include' <identifier>
```

Ivory provides two ways to bring definitions from C into scope depending on the
type of definition.  Use `extern` to bring variables into scope from external C
sources and use `import` to bring functions into scope. The arguments to
`extern` include the (relative) path to the C definition, the type of the
definition, and the name. The arguments to `import` include the (relative) path
to the C definition, the C name, the Ivory type, and the Ivory name.

```
<includeProc>  ::= 'import' '(' <header> ',' <identifier> ')' <type> <identifier> '(' <args> ')'
<externImport> ::= 'extern' <header> <type> <identifier>
<header>       ::= [ <identifier> '/' ]* <identifier> '.' <identifier> -- file paths
```

### iterators

Instead of providing traditional looping constructs, Ivory supports loops
through the use of iterators. All of the forms of the iterator syntax, except
`forever`, take the name of the iterator as a parameter, the iterator will be
created and brought into scope automatically. The difference between the
different iterator forms gives the programmer control over the range and/or
order of the iteration.

The iterators here are described by starting from simplest form and then moving
to most elaborate forms, as the later iterators subsume the preceding ones. The
explanations assume the reader is familiar with the preceding forms.

* `forever`: The simplest iterator is the `forever` iterator that creates the
equivalent of a C `while(1)` loop. It simply iterates without stopping and
without a loop counter. Ex:
```
-- loop forever calling foo
forever {
  foo();
}
```

* `map`: The next simplest iterator is the `map` iterator that iterates over
the full range of the indices of an array, which must appear in the body of
the `map`. Ex:
```
-- copy array 'other' into 'my_array'
void copy_array4(const *uint32_t[4] other) {
  alloc my_array[] = {0,0,0,0};
  map iter {
        store my_array@iter as other[iter];
  }
}
```

* `upTo` and `downFrom`: These iterators take an expression for the bound on
the largest iterator value (here, `top`). `upTo` will iterate on the range `0`
to `top` (inclusive) and `downFrom` iterates from `top` to `0` (decreasing and
also inclusive). Ex:
```
void copy_array_upTo(const *uint32_t[4] other, ix_t 4 top) {
  alloc my_array[] = {0,0,0,0};
  upTo top iter {
        store my_array@iter as other[iter];
  }
}
void copy_array_downFrom(const *uint32_t[4] other, ix_t 4 bottom) {
  alloc my_array[] = {0,0,0,0};
  downFrom bottom iter {
        store my_array@iter as other[iter];
  }
}
```
Note: The example above also demonstrates the two different forms of array
indexing. The `@` syntax which returns a reference and the `[]` syntax which is
like the `@` syntax but with a dereference.

* `upFromTo` and `downFromTo`: Similar to the above iterator syntax, these two
variants take an additional bound, the value to stop on (again, inclusive
range). The order is `(start, stop)` and again the difference between these two
forms is the order (increasing or decreasing) of the iteration. Ex:
```
void copy_array_upFromTo(const *uint32_t[4] other, ix_t 4 start, ix_t 4 stop) {
  alloc my_array[] = {0,0,0,0};
  upFromTo (start, stop) iter {
        store my_array@iter as other[iter];
  }
}
void copy_array_downFromTo(const *uint32_t[4] other, ix_t 4 start, ix_t 4 stop) {
  alloc my_array[] = {0,0,0,0};
  downFromTo (start, stop) iter {
        store my_array@iter as other[iter];
  }
}
```

## Macros

Ivory uses Haskell as a macro language. The main use for macros is code
generation or statically computing a value. When a macro is invoked, the name
must be prefixed with a `$` sigil. Macros may appear where either a statement
or expression is expected, depending on the macro. When a macro computes a
value, the `<-` syntax is used to give that value a name in the Ivory program.

## Interfacing with C

There are two main ways to interface with C, calling a function that is defined
in C or relying on values defined in C code.  Before a C function can be called
from Ivory, first it must be imported and before a C variable can be used it
must be declared `extern` in the Ivory module.

Ex:
```
extern someheader.h G* uint8_t myptr -- brings myptr into scope as a global reference
import (someheader.h, some_C_fn_name) int ivory_fn_name ()
```

Note: The function import requires both the name of the function in C and the name that will
be used in Ivory, in that order.

Note: Ivory does not support C's notion of varargs. As such it is common to
import varargs functions, such as `printf`, multiple times under different
names, once for each set of arguments. Ex:

```
import (stdio.h, printf) int32_t printf_int8_t(string s, int8_t n)
import (stdio.h, printf) int32_t printf_int32_t(string s, int32_t n)
```

## Casting and conversion

Ivory is designed so that programs are not allowed to have undefined behavior
and in turn this requires special support for casting operations. The C standard
makes conversion from unsigned to signed well-defined as long as the value can
be preserved. But if the unsigned value is bigger than the largest representable
value of the signed type, as is the case for a value we want to treat as
negative, the behavior is implementation-specific, and may trap.

* `safeCast`

    Only defined for conversions that are statically known to be safe. Here safety
    means that there is no undefined behavior in terms of C.

* `castWith`

    This will cast between any two types, but if the cast is not safe according to
    the rules of `safeCast` it evaluates to a user supplied default value.

* `twosCompCast`

    This algorithm checks the most significant bit, which is the sign
    bit. If it is clear, then the value is representable in the target
    type (assuming they're the same bit-width) and we can just cast.

    One way to define two's complement is that `-x = ~x + 1`. Negating
    both sides gives `x = -~x - 1`, and that is the identity we use when
    the sign bit is set.

    If the sign bit was set, then if we flip every bit we will have a
    value that is representable and we can cast it to the target type.
    So we insert the cast after complementing the value but before
    negating and subtracting 1.

    On machines that natively use two's complement for signed numbers,
    this should optimize away to use zero instructions. However, the C
    standard also permits implementations that use one's complement or
    sign-magnitude representations, and this algorithm is expected to
    work in those implementations as well.

* `twosCompRep`

    Takes a signed value interpreted as a two's complement, and returns an
    unsigned value with the identity bit pattern. This is only defined for cases
    where it is statically guaranteed not to overflow.

* `bitCast`

    A narrowing cast from one bit type to another.  This explicitly discards
    the upper bits of the input value to return a smaller type, and is only defined
    for unsigned integers.

## Ivory Standard Library

-- TODO: documenting this is more work than I had realized. There's a lot I
don't understand.

* memcpy is C's memcpy, but it's built in to the syntax
* lookup in the happy grammar the set of builtins
* can't use parens for refCopy's and lee says that's bad

## Translating to C

Below is a complete set of example files to get an simple Ivory environment
going. We assume you have a Haskell development toolchain already installed and
setup. If not, we recommend [Haskell Platform
Core](https://www.haskell.org/platform/).

First, your project will need a cabal description file so that it can be built
as a Haskell project.

* First, your project will need a cabal description file so that it can be
built as a Haskell Project. The most important detail for this file is the
`build-depends` field. This tells cabal that we need the `ivory`,
`ivory-stdlib`, and `ivory-backend-c` packages. For this example, the file is
named: `minimal-ivory-example.cabal`

    ```
    name:                minimal-ivory-example
    version:             0.1.0.0
    license:             BSD3
    license-file:        LICENSE
    author:              Your Name
    maintainer:          you@example.com
    build-type:          Simple
    extra-source-files:  ChangeLog.md
    cabal-version:       >=1.10
    
    executable minimal-ivory-example
      main-is:             Main.hs
      -- other-modules:
      -- other-extensions:
      build-depends:       base >=4.8 && <4.10
                         , ivory
                         , ivory-stdlib
                         , ivory-backend-c
      hs-source-dirs:      src
      default-language:    Haskell2010
    ```

* Cabal requires an additional file. This file is not important for this simple
project and we use the default: `Setup.hs`

    ```
    import Distribution.Simple
    main = defaultMain
    ```

* The `main` function for the project is in `src/Main.hs`. This file calls the
Ivory compiler on whatever input files we specify.  Here we use the `ivoryFile`
quasiquoter to compile the file `example.ivory`. The line `main = runCompiler`
mentions `example`. The name `example` is created by the `ivoryFile` line by
concatenating directory names and dropping the `.ivory` suffix. For instance,
if the file was at the path `directory1/directory2/my_input.ivory`, then we
would use `direcotry1directory2my_input` as the module name passed to
`runCompiler`:

    ```
    {-# LANGUAGE QuasiQuotes         #-}
    {-# LANGUAGE DataKinds           #-}
    module Main where
    
    import           Ivory.Compile.C.CmdlineFrontend
    import           Ivory.Language
    import           Ivory.Stdlib
    
    [ivoryFile|example.ivory|]
    
    main :: IO ()
    main = runCompiler [example, stdlibStringModule] stdlibStringArtifacts
      initialOpts {outDir = Just "ivory-example", constFold = True}
    ```

* Finally, we have our Ivory program that we want to translate to C: `example.ivory`

    ```
    void f() {}
    ```

This will create a directory, `ivory-example` and write the output there.
