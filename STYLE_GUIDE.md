# Style guide

The purpose of this style guide is to:

- Express the æsthetics and the soul of the project's team
- Make contributions easier by making the shape and location of new code
  predictable

Thus, the form is the function.

> The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
> NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and
> "OPTIONAL" in this document are to be interpreted as described in
> [RFC 2119]. In French text, "DOIT", "NE DOIT PAS", "DEVRAIT",
> "NE DEVRAIT PAS" and "PEUT" have the same meaning as "MUST",
> "MUST NOT", "SHOULD", "SHOULD NOT" and "MAY".

## Localisation

Text in the Haskell source MUST be in English.

### English

In both code and prose, the orthography and constructs used SHOULD use Standard
British Spelling, and conventions outlined in the [New Oxford Style Manual].

### Français

L’usage du français DEVRAIT être conforme au [Lexique des règles typographiques
en usage à l’Imprimerie nationale][Lexique Typographique].

## Haskell

### Point-free style and composition

You SHOULD NOT use point-free style when it makes the code harder to read.
We read code when something bad happens to it, or we need a clear and easy
understanding of what is happening.

These two rules are more precise:

- You MUST NOT use tuple sections. Write `\x -> (a, x)` instead of `(a,)`.

- Three or more composed functions SHOULD be laid out as a pipeline with `(&)`
  and `(<&>)`, not `(.)`.

### Import qualification

You SHOULD use qualified imports for functions and open imports for types.

The examples in this guide use these imports:

```haskell
import Data.GI.Base (AttrOp ((:=)), SignalProxy (PropertyNotify, PropertySet))
import Data.GI.Base qualified as GI
import Floreal.Commons.Gtk.Bind qualified as Bind
import Floreal.Commons.Gtk.Common qualified as Common
import Floreal.Commons.Gtk.ListStore qualified as ListStore
import Floreal.Commons.Gtk.Signal qualified as Signal
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gtk (ListItem)
import GI.Gtk qualified as Gtk
```

### Function arguments

In case a function takes a significant number of function arguments that do
not differ in type (three successive `Text` arguments, for instance), or a
great number of arguments, it is RECOMMENDED to write a bespoke record that
holds those arguments and will be passed to the function. For clarity, you MAY
give the record the suffix `Props`.

### GTK widgets

A widget module gives a render function. The render function paints a view
model on the existing widgets of the module. A paint function is different: it
comes from `bind`, and it paints one value on one `Control`.

- A render function SHOULD only paint the existing widgets. Only lists SHOULD
  add or remove widgets.

### GTK signal handlers

A signal handler lives as long as the object that emits the signal, or until
it is disconnected. The handler holds a strong reference to each object that
it captures, and the Haskell GC cannot see GObject reference counts. Thus a
handler that captures a widget keeps that widget alive.

You MUST connect handlers with `Floreal.Commons.Gtk.Signal`, not with `on` or
`after` from `Data.GI.Base`. HLint enforces this. An `On #signal` attribute in
`new` is equivalent to `onSelf`.

- You SHOULD use `onSelf` for a handler that lives as long as its emitter.
  The handler MUST NOT capture its emitter, or an ancestor of it. Use `?self`
  instead.

  ```haskell
  -- Bad: the closure holds `button`, and `button` holds the closure
  Signal.onSelf button #clicked $ GI.set button [#sensitive := False]

  -- Good
  Signal.onSelf button #clicked $ GI.set ?self [#sensitive := False]
  ```

- You MUST use `onBehalf` when a widget connects a handler to an object that
  lives longer. Examples are the `Application`, a shared model and a file
  monitor. The handler holds the widget weakly and receives it as its first
  argument. When GTK destroys the widget, `onBehalf` disconnects the handler.
  The handler MUST NOT capture the widget.

  ```haskell
  -- Bad: `label` stays alive as long as `model` does
  Signal.onSelf model #itemsChanged $ \_ _ _ -> updateCount label
  Signal.onBehalf label model #itemsChanged $ \_ _ _ _ -> updateCount label

  -- Good
  Signal.onBehalf label model #itemsChanged $ \owner _ _ _ -> updateCount owner
  ```

- A handler SHOULD capture a small handle (`dispatch`, an `IORef`, a `TVar`,
  an identifier), not the whole application state.

  ```haskell
  -- Bad: a forgotten handler keeps the whole state alive
  Signal.onSelf entry #changed $ render state

  -- Good: the handler holds only `dispatch`
  Signal.onSelf entry (PropertySet #text) (dispatch . SetPaletteQuery)
  ```

- You MUST NOT use `GLib.timeoutAdd` or `GLib.idleAdd`. For an action that
  runs one time, you SHOULD use `postMain`, `postAfter` or `postIdle` from
  `Floreal.Commons.GLib.MainLoop`. For an action that replaces the action that
  waits, you SHOULD use `debounced` from the same module. For an action that
  repeats for a widget, you SHOULD use `everyOnBehalf` from
  `Floreal.Commons.Gtk.Signal`. It stops when GTK destroys the widget.

  ```haskell
  -- Bad: the callback runs forever, and keeps `label` alive
  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 $ tick label >> pure True

  -- Good
  Signal.everyOnBehalf label 1000 tick
  ```

### GTK widget state

A widget that shows a model value and that the user can change MUST use a
`Control` from `Floreal.Commons.Gtk.Bind`.

When a render function paints such a widget, the widget fires its signal, and
the handler dispatches the value again. This is a feedback loop.

The paint function from `bind` changes the widget only when the value differs,
so you can call it on each render. `bind` also blocks the handler while it
paints, which stops the loop.

- You MUST NOT use a flag or the focus to stop the loop.

  ```haskell
  -- Bad: a flag stops the loop, and the handler holds its emitter
  Signal.onSelf row (PropertyNotify #active) $ \_ -> do
    painting <- readIORef paintingRef
    unless painting $ Adw.switchRowGetActive row >>= dispatch . SetEnabled

  -- Good
  paintEnabled <- Bind.bind (Bind.switchRow row) (dispatch . SetEnabled)
  ```

- A dialog MUST be a `Control Bool`, where `True` means open. You SHOULD use
  `closing` to run an action when the user closes the dialog.

  ```haskell
  paintOpen <- Bind.bind openControl (Bind.closing (dispatch CloseOffloadDialog))
  ```

- For a larger repaint, you SHOULD use `bindQuietly`. It gives the paint
  function and also `quietly`, which blocks the handlers while its action
  runs. See `renderSidebar` in `MediaCopy.Gtk.View`.

- A property handler SHOULD use `PropertySet`, which gives the new value,
  not `PropertyNotify`.

  ```haskell
  -- Bad: the closure holds `toggle`
  Signal.onSelf toggle (PropertyNotify #active) $ \_ ->
    Gtk.switchGetActive toggle >>= react

  -- Good
  Signal.onSelf toggle (PropertySet #active) react
  ```

- In a new `Control`, `connect` MUST report an update (`const v`, or a
  change to one part) and SHOULD use `connectSelf` or `propertyControl`.
  After `paint v`, `current` MUST give `v`. It MUST NOT read text that the
  user did not apply.

### GTK lists

- A long list, or a list that changes often, SHOULD be a `ListStore` in a
  `Gtk.ListView`, not a `Gtk.ListBox` that you refill on each render. A
  `Gtk.ListBox`, an `Adw.PreferencesGroup` or an `Adw.ExpanderRow` is correct
  for a short list.

- You SHOULD paint a `ListStore` that you hold yourself with `replaceItems`,
  or with `spliceItems` if you know what changed.

- A list item factory MUST use `bindItems` or `recycleItems` from
  `Floreal.Commons.Gtk.ListStore`. `recycleItems` builds each row one time at
  `#setup`, and `#bind` only updates it. The bind action returns its cleanup.
  The cleanup runs at `#unbind`, `#teardown` and the next `#bind`. GTK
  recycles the rows and does not destroy them, so `onBehalf` does not help
  here. `bindItems` gives the bind action the list item and the value of the
  item.

  ```haskell
  -- Bad: the handler stays on the recycled row
  Signal.onSelf factory #bind $ \object -> do
    listItem <- GI.unsafeCastTo ListItem object
    void $ Signal.onSelf model #itemsChanged $ \_ _ _ -> refresh listItem

  -- Good
  ListStore.bindItems factory $ \listItem job -> do
    handler <- Signal.onSelf model #itemsChanged $ \_ _ _ -> refreshJob listItem job
    pure (GI.disconnectSignalHandler model handler)
  ```

## MediaCopy 3000

### Localisation

Text that changes with the language comes from `locales/*.ftl`.

### Signal handlers

- A handler SHOULD send one `UiMessage` through `dispatch`. The new state
  comes back through the render function.

### Lists

- A long list, or a list with a selection, SHOULD be a `KeyedList` from
  `MediaCopy.Gtk.Widgets.Lists`, in a `Gtk.ListView`. A `KeyedList` holds a
  `ListStore` and paints it itself. It keeps the rows, the selection and the
  scroll position, and changes only the keys that changed. You SHOULD paint it
  with `renderKeyed`, not with `replaceItems` or `spliceItems`.

- A short list of `RowView` in an `Adw.PreferencesGroup` or an
  `Adw.ExpanderRow` MAY use `newRowsCell` from `MediaCopy.Gtk.Widgets.Common`.
  It rebuilds all the rows, so the focus position is lost.

## Pull requests (PRs)

### Ticket

When fixing a problem, you SHOULD open a ticket that describes the issue and
the spirit of the fix, if you know it in advance. This allows other participants
to give insights before you spend time implementing the fix. Multiple PRs can
target a single ticket if it helps with readability.

### Commit hygiene

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

Should your work be sponsored, you MAY mention it with the
`Sponsored-By: <company name>` trailer. This allows us to properly give
attribution to our sponsors.

## Closing words

Our style is both the expression of ourselves, and aspirational.
You are encouraged to submit patches that make the codebase adhere to it.

[RFC 2119]: https://datatracker.ietf.org/doc/html/rfc2119
[Scoped Commits]: https://scopedcommits.com/
[New Oxford Style Manual]: https://global.oup.com/academic/product/new-oxford-style-manual-9780198767251?cc=fr&lang=en&
[Lexique Typographique]: https://actes-sud.fr/catalogue/lexique-des-regles-typographiques-000702
