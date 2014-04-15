
# Tower User Guide

This is an informal guide to using the Tower language. We suggest you
also consult the [haddock documentation][tower-haddock] and the
[examples][] along with this guide.

## Language goals

Generate code for a system of tasks which communicate through channels.

- tasks only have an event loop, the user specifies the event sources and
  provides event handlers written in the Ivory language
- only outbound communication is by writing to channels.
- nonblocking read messages from channels, recieve new channel messages as events, or read the
  latest message only

also has some Ivory module system functionality so the user can specify all of
the required Ivory code for a system.

## Contexts

`Tower` context:

- create (instantiate) channels
- create (instantiate) tasks
- specify ivory modules and dependencies which are part of the system build


`Task` context:
- bring channel endpoints into scope (graph capture)
- specify events (from channel sinks, timers, or external events)
- specify event handlers (ivory callbacks)
- create "taskLocal" variables, shared amongst the event handlers
- other Ivory module bookeeping - dependencies, helper procs, etc

## Using Channels


## "Platform" phantom type

Each of the `Task` and `Tower` type constructors take a phantom type `p` and
return type `a`, e.g. `Task p a`. The phantom type `p` is used to t


