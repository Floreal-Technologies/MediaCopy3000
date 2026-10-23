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
it is disconnected. The handler holds a strong reference to each object that
it captures, and the Haskell GC cannot see GObject reference counts. Thus a
handler that captures a widget keeps that widget alive.

You MUST connect handlers with `Floreal.Commons.Gtk.Signal`, not with `on` or
`after` from `Data.GI.Base`. HLint enforces this. An `On #signal` attribute in
`new` is equivalent to `onSelf`.

- Use `onSelf` for a handler that lives as long as its emitter. The handler
  MUST NOT capture its emitter, or an ancestor of it. Use `?self` instead.

  ```haskell
  -- Bad: the closure holds `button`, and `button` holds the closure
  onSelf button #clicked $ set button [#sensitive := False]

  -- Good
  onSelf button #clicked $ set ?self [#sensitive := False]
  ```

- Use `onBehalf` when a widget connects a handler to an object that lives
  longer. Examples are the `Application`, a shared model and a file monitor.
  The handler holds the widget weakly and receives it as its first argument.
  When GTK destroys the widget, `onBehalf` disconnects the handler. The handler
  MUST NOT capture the widget.

  ```haskell
  -- Bad: `label` stays alive as long as `model` does
  onSelf model #itemsChanged $ \_ _ _ -> updateCount label
  onBehalf label model #itemsChanged $ \_ _ _ _ -> updateCount label

  -- Good
  onBehalf label model #itemsChanged $ \owner _ _ _ -> updateCount owner
  ```

- A handler SHOULD capture a small handle (an `IORef`, a `TVar`, an
  identifier), not the whole application state.

  ```haskell
  -- Bad: a forgotten handler keeps the whole state alive
  onSelf entry #changed $ render state

  -- Good
  onSelf entry #changed $ readIORef stateRef >>= render
  ```

- You MUST NOT use `GLib.timeoutAdd` or `GLib.idleAdd`. Use `postMain`,
  `postAfter` or `postIdle` from `Floreal.Commons.GLib.MainLoop` for an action
  that runs one time. Use `debounced` for an action that replaces the action
  that waits. Use `everyOnBehalf` for an action that repeats for a widget. It
  stops when GTK destroys the widget.

  ```haskell
  -- Bad: the callback runs forever, and keeps `label` alive
  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 $ tick label >> pure True

  -- Good
  everyOnBehalf label 1000 tick
  ```

- A list item factory MUST use `bindItems` or `recycleItems` from
  `Floreal.Commons.Gtk.ListStore`. `recycleItems` builds each row one time at
  `#setup`, and `#bind` only updates it. The bind action returns its cleanup.
  The cleanup runs at `#unbind`, `#teardown` and the next `#bind`. GTK
  recycles the rows and does not destroy them, so `onBehalf` does not help
  here.

  ```haskell
  -- Bad: the handler stays on the recycled row
  onSelf factory #bind $ \object -> do
    listItem <- unsafeCastTo Gtk.ListItem object
    void $ onSelf model #itemsChanged $ \_ _ _ -> refresh listItem

  -- Good
  bindItems factory $ \listItem job -> do
    handler <- onSelf model #itemsChanged $ \_ _ _ -> refreshJob listItem job
    pure (disconnectSignalHandler model handler)
  ```

### GTK widget state

A widget that shows a model value and that the user can change MUST use a
`Control` from `Floreal.Commons.Gtk.Bind`.

- The paint function from `bind` changes the widget only when the value
  differs, so you can call it on each render.

- `bind` blocks the handler while it paints. You MUST NOT use a flag or the
  focus to stop the loop.

  ```haskell
  -- Bad: a flag stops the loop, and the handler holds its emitter
  onSelf row (Adw.PropertyNotify #active) $ \_ -> do
    painting <- readIORef paintingRef
    unless painting $ Adw.switchRowGetActive row >>= dispatch . SetEnabled

  -- Good
  paintEnabled <- bind (switchRow row) (dispatch . SetEnabled)
  ```

- A property handler SHOULD use `PropertySet`, which gives the new value,
  not `PropertyNotify`.

  ```haskell
  -- Bad: the closure holds `toggle`
  onSelf toggle (Gtk.PropertyNotify #active) $ \_ -> Gtk.switchGetActive toggle >>= react

  -- Good
  onSelf toggle (PropertySet #active) react
  ```

- In a new `Control`, `connect` MUST report an update (`const v`, or a
  change to one part) and SHOULD use `connectSelf` or `propertyControl`.
  After `paint v`, `current` MUST give `v`. It MUST NOT read text that the
  user did not apply.

### GTK lists

- A list that changes SHOULD be a `ListStore` in a `Gtk.ListView`, not a
  `Gtk.ListBox` that you refill on each render. A `Gtk.ListBox` is correct
  for a short, fixed set of rows.

- Paint the list with `replaceItems`, or `spliceItems` if you know what
  changed.

- `bindItems` gives the bind action the value of the item:

  ```haskell
  bindItems factory $ \listItem job -> do
    paintJobRow listItem job
    pure (pure ())
  ```

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
