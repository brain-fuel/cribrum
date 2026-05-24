# TEAWeb Counter — MVP demo

The architectural keystone for TEAWeb. ~80 LOC of app code proves the
TEAWeb runtime end-to-end:

- `HExpr` as the view return type (via `View msg`).
- Typed `onClick` carrying `msg`.
- Dispatch through the handler table installed by `mount`.
- `update` returning `(model, Cmd msg)`.
- A `Cmd` leaf (`Focus`) firing into the DOM.

## Build

```
$ cd examples/teaweb/counter
$ idris2 --cg javascript --build counter.ipkg
```

This produces `build/exec/counter` — a single JavaScript bundle.

## Run

Open `index.html` in a browser (it loads the bundle and mounts the
app under `#app`):

```
$ python3 -m http.server 8080
# then visit http://localhost:8080
```

Or any static-file server.

## App

`src/Main.idr` defines:

- `data Msg = Increment | Decrement | FocusInput`
- `record Model = MkModel { count : Int }`
- `init_  : (Model, Cmd Msg)`
- `update_ : Msg -> Model -> (Model, Cmd Msg)`
- `view_   : Model -> View Msg`
- `subs_   : Model -> Sub Msg`
- `main : IO () = mount prog "app"`

Increment/Decrement buttons mutate `count`; the "Focus the input"
button emits a `Focus "name-input"` Cmd that the runtime interprets
via `element.focus()` at the FFI boundary.

## What this does NOT yet have

- A Phase-2 content-model proof in the view's return type. The current
  spike returns `View msg`; after Phase 2 lands, smart constructors
  will refuse illegal children at compile time and `view_` will return
  `(h : HExpr ** IsValidHtml h × StructuralAA h)`.
- Keyed-children diff — `reconcile` is Day-1 blow-and-rebuild. The
  input field is *controlled* (model holds the name, view sets `value=`)
  so blow-and-rebuild restores it correctly after each render; this is
  the Elm/React-style escape hatch that makes Day-1 ergonomic. The T6
  docs nav demo will force the Day-2 keyed diff for perf.

## How the demo proves Day-1 end-to-end

| Click / type     | What you should see                                          |
|------------------|--------------------------------------------------------------|
| `+` button       | Count increments. Name field still holds whatever you typed. |
| `-` button       | Count decrements. Name field still holds it.                 |
| Type in input    | Greeting line echoes the name live (onInput → UpdateName).   |
| `Focus the input`| Cursor jumps to the input field. Typed text is preserved.    |
