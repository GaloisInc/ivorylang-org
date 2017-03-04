# Ivory Language Reference

## Syntax

### Keywords

  * `if`
  * `else`
  * `assert`
  * `assume`
  * `pre`
  * `post`
  * `let`
  * `return`
  * `alloc`
  * `store`
  * `break`
  * `const`
  * `struct`
  * `abstract`
  * `type`
  * `include`
  * `import`
  * `extern`
  * `bitdata`
  * `as`
  * Reserved identifiers used in the Ivory standard library (always in scope):
    * `string`
    * `memcpy`
    * `abs`
    * `signum`
    * `exp`
    * `sqrt`
    * `log`
    * `pow`
    * `div`
    * `sin`
    * `cos`
    * `tan`
    * `asin`
    * `acos`
    * `atan`
    * `atan2`
    * `sinh`
    * `cosh`
    * `tanh`
    * `asinh`
    * `acosh`
    * `atanh`
    * `isnan`
    * `isinf`
    * `round`
    * `ceil`
    * `floor`
    * `fromIx`
    * `ixSize`
    * `toIx`
    * `toCArray`
    * `arrayLen`
    * `sizeOf`
    * `nullPtr`
    * `refToPtr`
    * casting
        * `safeCast`
        * `bitCast`
        * `castWith`
        * `twosCompCast`
        * `twosCompRep`
    * Iterator related
        * `map`
        * `upTo`
        * `upFromTo`
        * `downFrom`
        * `downFromTo`
        * `forever`

## Reserved Symbols

The follow symbols are reserved in Ivory, most of which will have a familiar meaning for
C programmers:

  * `$`
  * `::`
  * `?`
  * `:`
  * `.`
  * `->`
  * `==`
  * `!=`
  * `*`
  * `/`
  * `+`
  * `-`
  * `%`
  * `=`
  * `<`
  * `<=`
  * `>=`
  * `>`
  * `|`
  * `&`
  * `^`
  * `~`
  * `!`
  * `&&`
  * `||`
  * `<<`
  * `>>`
  * `(`
  * `)`
  * `}`
  * `{`
  * `[`
  * `]`
  * `;`
  * `,`
  * `@`
  * `<-`
  * `_`
  * `#`

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
myArray[i] // returns the value stored in the array at position i
myStruct->f // returns the value stored in field f of the struct
```

Examples of Ivory dereferences that return references would be:
```
myArray@i // returns a reference to the ith position of the array
myStruct.f // returns a reference to the field f of the struct
```

The full grammar for Ivory expressions follows:

```
<exp> ::= <number>
        | <string>
        | <identifier>
        | '(' <exp> ')'
        // areas
        | '*' <exp>             // deref
        | <exp> '@' <exp>       // array indexing, arr@ix (returns reference)
        | <exp> '[' <exp> ']'   // array index and derference
        | <exp> '.' <exp>       // struct field access (returns reference)
        | <exp> '->' <exp>      // struct field access and dereference
        | '&' <identifier>      // address of operator
        // macros
        | '$' <identifier> [ '(' [<exp> ',']* ')' ]
        // function calls
        | <identifier> '(' [<exp> ',']* ')'
        // Unary ops
        | <unaryop> <exp>
        // Binary ops
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
               | <allocRef> -- TODO: unpack this
               | 'store' <exp> 'as' <exp> ';'
               // function calls
               | <identifier> ['(' [<exp> ',']*')']
               | '$' <identifier> ['(' [<exp> ',']*')']
               // binding the value returned from a macro invocation
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
  return a;
}
{ pre(a < 4);
  pre(a > 0);
  post(return > 5);
}
```

### types

#### Values Types
Ivory has many of the same basic types you would expect in C:

  * void
  * bool
  * char
  * float
  * double
  * int8_t
  * int16_t
  * int32_t
  * int64_t
  * uint8_t
  * uint16_t
  * uint32_t
  * uint64_t

Note: there are no machine-dependent types in Ivory, like `int`, `short int`,
`long int`.

#### Memory area types

All allocated data in Ivory is associated with an area.

The first area is the Global area. The Global area is always in scope and exists
for the whole lifetime of the Ivory program. The Global area is abbreviated to
'G' when it appears in a reference's type.

The second area is the Stack area (abbreviate 'S'). Each function gets its own
Stack area and this area refers to the stack space of the C stack for the
function.

All references in Ivory are associated with an area. The area qualifier for
references must fit the following rules:

  * No area specified: This reference may point to memory in any area.
  * 'G': This reference always points to memory in the Global area.
  * 'S': This reference always points to memory in the Stack area.
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

#### string structs

The string variant of structs is for creating fixed size arrays with two
fields. The first field for tracking the current length of the string and the
other field that holds the elements up to the declared maximum size. Ivory
provides a standard library of functions for working with strings,
`stblibStringModule`.

  * Ex initialization:
  `alloc s{} = $stringInit("foo");`

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
<structInit> ::= '$' <identifier> [ '(' [<exp> ',']* ')' ] // macro initialized
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
<includeProc> ::= 'import' '(' <header> ',' <identifier> ')' <type> <identifier> '(' <args> ')'
<args> ::= [ <type> <identifier> ',' ]*
<externImport> ::= 'extern' <header> <type> <identifier>
<header> ::= [ <identifier> / ]* <identifier> '.' <identifier> // file paths
```

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
