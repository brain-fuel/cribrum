# TEAWeb T6 — Cribrum docs nav island

Per `plan.dj` §Phase T6: the keystone end-to-end demo. A single-page
documentation shell with a static prose column and a TEAWeb-mounted
table-of-contents island in the sidebar. The island demonstrates:

- `HExpr` as the view return type (via `View msg`).
- `onInput` carrying a `String -> msg` closure for search-as-you-type.
- `onKeyDown` carrying a `String -> msg` closure for arrow-key
  navigation (ArrowUp/ArrowDown/Home/End/Escape).
- `onClick` + `onMouseEnter` for pointer interaction.
- `Cmd Focus` returned from `update` for "Esc clears + refocuses
  search" UX.
- Reconcile + focus preservation (Day-1 blow-and-rebuild) across every
  keystroke.

The prose column in `index.html` *and* the TOC entries in `src/Generated.idr`
are both generated from `README.dj` by `tools/render-docsnav` — see
`make docsnav` from the project root. Anchors are slugified once
(`Cribrum.Pipeline.Anchor.slugify`) so the heading `id` attributes and
the TOC `href`s come from the same source and match without manual sync.

## Build

```
$ make docsnav        # from project root — regenerates index.html +
                      # src/Generated.idr from README.dj, then builds the JS bundle
```

Or, if you only need the JS bundle (TOC + prose unchanged):

```
$ cd examples/teaweb/docsnav
$ idris2 --cg javascript --build docsnav.ipkg
```

Produces `build/exec/docsnav` — a single JavaScript bundle.

## Run

```
$ python3 -m http.server 8080
# then visit http://localhost:8080
```

Or any static-file server. The page loads `build/exec/docsnav` and
mounts the island under `#toc-island`.

## App

`src/Main.idr` defines:

- `data Msg = UpdateQuery String | KeyPressed String | HoverItem Nat
  | ClickItem Nat | ClearQuery | FocusSearch`
- `record Model { query : String, selected : Maybe Nat }`
- `tocItems = genTocItems` — TOC rows imported from the auto-generated
  `src/Generated.idr` (harvested from `README.dj` headings; `TocItem`
  record lives in `src/TocData.idr`).
- `filterItems : String -> List TocItem -> List TocItem` — ASCII case-
  fold substring match against item titles.
- `update_`: rebuilds the selected index against the filtered list on
  every query change; arrow-key handling wraps at list ends.

## Keyboard map

| Key            | Effect                                       |
|----------------|----------------------------------------------|
| typing         | filters the TOC live                         |
| `ArrowDown`    | move highlight down (wraps to first at end)  |
| `ArrowUp`      | move highlight up (wraps to last at start)   |
| `Home`         | highlight first visible item                 |
| `End`          | highlight last visible item                  |
| `Escape`       | clear filter; refocus search box             |
| `Enter`        | (anchor follows on click; no special handler) |
