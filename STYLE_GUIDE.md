# Style Guide

The purpose of this style guide is to:

* Express the æsthetics and the soul of the project's team
* Make contributions easier by making the shape and location of new code predictable

Thus, the form is the function.

> The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
> NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and
> "OPTIONAL" in this document are to be interpreted as described in
> [RFC 2119].

## Haskell

### Point-free style

You MUST NOT use point-free style that favours terseness over readability.
We read code when something bad happens to it, or we need a clear and easy
understand of what is happening.

- You MUST NOT use tuple sections. Write `\x -> (a, x)` instead of `(a,)`

- Three or more composed functions SHOULD be laid out as a pipeline with `(&)` and `(<&>)`, not `(.)`.

### Import qualification

You SHOULD use qualified imports for functions and open imports for types.

### Function arguments

In case a function takes a significant amount of function arguments that do not differ in types
(Three successive `Text` arguments, for instance), or a great amount of arguments,
it is RECOMMENDED to write a bespoke record that holds those arguments and will be
passed to the function. For clarity, you MAY give the record the suffix `Props`.

### GTK signal handlers

A signal handler lives as long as the object that emits the signal, or until
it is disconnected. The Haskell GC cannot see the GObject reference counts,
so a handler that keeps its emitter alive is never collected.

- A handler MUST NOT capture its own emitter, or an ancestor of it.
  Use `?self` or the arguments of the signal instead.

  ```haskell
  -- Bad: the closure holds a reference to `button`, and `button` holds the closure
  on button #clicked $ set button [#sensitive := False]

  -- Good
  on button #clicked $ set ?self [#sensitive := False]
  ```

- A handler connected to a long-lived object (the `Application`, a shared
  model, a file monitor) MUST be disconnected when the subscriber is destroyed.

  ```haskell
  -- Bad: `label` stays alive as long as `model` does
  void $ on model #itemsChanged $ \_ _ _ -> updateCount label

  -- Good
  handlerId <- on model #itemsChanged $ \_ _ _ -> updateCount label
  void $ on label #destroy $ disconnectSignalHandler model handlerId
  ```

- A handler SHOULD capture a small handle (an `IORef`, a `TVar`, an
  identifier), not the whole application state.

  ```haskell
  -- Bad: a forgotten handler keeps the whole state alive
  on entry #changed $ render state

  -- Good
  on entry #changed $ readIORef stateRef >>= render
  ```

- A `GLib.timeoutAdd` or `GLib.idleAdd` callback that repeats MUST be removed
  with `GLib.sourceRemove` when its owner is destroyed. A one-shot callback
  MUST return `False`.

  ```haskell
  -- Bad: the callback runs forever, even after `label` is gone
  void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 $ tick label >> pure True

  -- Good
  sourceId <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 $ tick label >> pure True
  void $ on label #destroy $ void (GLib.sourceRemove sourceId)
  ```

- A list item factory that connects handlers in `#bind` MUST disconnect them
  in `#unbind`. Recycled rows otherwise accumulate handlers.

## Pull Requests (PRs)

### Ticket

When fixing a problem, you SHOULD open a ticket that describes the issue and
the spirit of the fix, if you know it in advance. This allows other participants
to give insights before you spend time implementing the fix. Multiple PRs :can
target a single ticket if it helps with readability.

### Commits hygiene

You SHOULD use [Scoped Commits] for your commits. Commits of a PR MUST be
squashed upon merging.

Your PR commit SHOULD mention the ticket that it refers to, and you MAY use a
forge-specific keyword to close it.

### Commit attribution

All commits MUST have attribution of their human authors.

Since the maintainers cannot ascertain the copyright legality of LLM-generated
contributions, the maintainers are not interested in reviewing commits or
accepting commits that are attributed to coding agents.

The human contributors MUST take full and sole responsibility for authorship.

Should your work be sponsored, your are invited to mention it with the
`Sponsored-By: <company name>` trailer. This allows us to properly give
attribution to our sponsors.

[RFC 2119]: https://datatracker.ietf.org/doc/html/rfc2119
[Scoped Commits]: https://scopedcommits.com/

## Closing words

Our style is both the expression of ourselves, and aspirational.
You are encouraged to submit patches that make the codebase adhere to it.
