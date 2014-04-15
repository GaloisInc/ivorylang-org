
# Tower User Guide

This is an informal guide to using the Tower language. We suggest you
also consult the [haddock documentation][tower-haddock] and the
[examples][] along with this guide.

[tower-haddock]: http://smaccmpilot.org/haddock/tower/Ivory-Tower.html
[examples]: https://github.com/GaloisInc/tower/tree/master/tower-examples

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

In the `Tower` context the user can:

- Create (instantiate) channels
- Create (instantiate) tasks
- Specify ivory modules and dependencies which are part of the system build

In the `Task` context the user can:

- Bring channel endpoints into scope (graph capture)
- Specify events (from channel sinks, timers, or external events)
- Specify event handlers (ivory callbacks)
- Create "taskLocal" variables, shared amongst the event handlers
- Do other Ivory module bookeeping - dependencies, helper procs, etc.

## Using Channels

In the Tower context, `channel` instantiates a channel, giving a `ChannelSource`
and `ChannelSink` to the user.

There is exactly one way to use a `ChannelSource` in a Task context:
`withChannelEmitter`. This transforms the source into a `ChannelEmitter`,
which can then be used from any task Ivory code to send a message onto the
channel using the `emit_` primitive. (`emitV_` is a variation on `emit_` that
takes a value, rather than a reference - this pattern is used in several places
in Tower).

There are three ways to use a `ChannelSink` in a Task context:

- Create a `ChannelReceiver` using `withChannelReceiver`
- Create a `ChannelReader` using `withChannelReader`
- Create an `Event` using `withChannelEvent` (see the Event section below)

ChannelReceiver is used to access a channel as a queue with a nonblocking pop
operation. The `receive` primitive performs this operation in the task's Ivory
code.

ChannelReader is used to access the most recent value written to a channel.
The `chanRead` primitive performs this operation, giving true when a valid value
has been written to the channel. Channels are optionally empty when the system
is initialized, see `channel'` for initial values.

## Events

Each task only runs user provided Ivory code when an Event has occured.
Events have an `area` parameter specifying the type of the message delivered.

The user attaches Ivory code to an `Event` with the `handle` primitive. The
Ivory code provided to this primitive is a function that takes a `ConstRef`
to the message and runs arbitrary Ivory code. An event may have many `handle`
callbacks attached to it - each callback will be run on each message.

There is one special "event" not covered by the following subsections, the
initialization event. The `taskInit` primitive allows users to specify code
which runs before any other event handler is run.

### Channel Events

A task can have a `ChannelSink` create `Event`s each time a new value
is available using the `withChannelEvent` primitive. Channel events are
delivered as messages in the order which they were posted to the channel by a
ChannelEmitter.

### Timer Events

A task can create a timer triggered event using the `withPeriodicEvent`
primitive. This event gives the period's trigger time as the message (`Stored
ITime`).

### Signal Events

A task can create an event triggered by an external signal - an asynchronous
exception, such as a system interrupt. A task registers an external signal with
the primitive `withSignalEvent`. The event delivers a meaningless message of
type  `Stored IBool` - the boolean will always be `true`, and only indicates
that the signal has occured.

`withSignalEvent` takes a type `SignalType` - an associated type determined by
the `Signalable` class instance used for the platform.

A primitive `withUnsafeSignalEvent` is available from the `Ivory.Tower.Signal`
module. This is a variation on `withSignalEvent` that takes an extra argument
for arbitrary Ivory code. This Ivory code is run in the context of the
asynchronous signal. Because it is run asynchronously, it is unsafe to access
other values from the Task context in that code, for example it is not safe to
read or write `taskLocal` state, or use `emit_`, `receive`, or `chanRead` from
that Ivory code.

A placeholder platform type `NoSignals` has a trivial implementation used for
building systems that do not require signal functionality. A simple
demonstration of a system interrupt signal is shown in `tower-examples`, and
more complex use cases are found in `ivory-bsp-stm32f4`.

## "Platform" phantom type

Each of the `Task` and `Tower` type constructors take a phantom type `p` and
return type `a`, e.g. `Task p a`. The phantom type `p` is used to track
properties of the platform, e.g. the `Signalable` type class is used to
provide the platform's `SignalType` and the 

Library code can make use of the phantom type to track other properties. For
example, in the `ivory-bsp-stm32f4` we use the `BoardHSE` class to generate code
specific to the external oscillator frequency provided by the board. We use a
`BoardHSE p` constraint on the platform parameter to track that every Tower task
generates code using the same instance of the `BoardHSE` class. [See an example
of this usage in the ivory-bsp-tests package][tests-platform].

[tests-platform]: https://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/src/ivory-bsp-tests/examples/Platforms.hs

