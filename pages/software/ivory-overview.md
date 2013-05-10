# Ivory Language

Ivory is an embedded domain specific language for safer systems programming.
You can consider Ivory to be a lot like a restricted version of the C
programming language. Ivory's restrictions are designed to eliminate certain
classes of bugs.

## A Systems Language

Ivory is a systems language designed for a natural compilation to C. Most of the
concepts familiar to a C programmer are present in Ivory: procedures, return
values, machine-sized integers and floating point numbers, structures, arrays,
and strings are all available in the Ivory language.

Ivory is designed for implementing systems and application software for high
assurance systems. In Ivory, trade-offs have been made to guarantee bounded
behavior.
Ivory's feature restrictions eliminate many valid, correct programs which are
possible to write in C. However, within the domain of high assurance software,
these restrictions are a reasonable trade-off between expressivity and
safety. In safety critical real-time systems, we more

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

In addition to treating memory more carefully than C, 

In the next section, we'll discuss how Ivory's implementation as an
embedded language 

## An Embedded Language



## Support for Verification





