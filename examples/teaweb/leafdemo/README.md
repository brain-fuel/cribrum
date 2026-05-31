# TEAWeb — leaf-coverage demo

End-to-end exercise of every TEAWeb `Cmd` / `Sub` leaf the **counter**
demo doesn't already cover. The counter proves `Every` + `After` +
`Focus`; this one closes the loop on the rest, so each FFI shim in
`Cribrum.Render.Dom` is driven by a real bundle in a real browser — the
only place these chez-untestable effects can be observed.

| Leaf | Where in the demo |
|------|-------------------|
| `Cmd.Http`             | **Fetch** button → `GET ./hello.json`; the settle round-trips an `HttpResult` back through the dispatch loop. |
| `Cmd.Random`           | **Roll** button → uniform die face in `[1,6]`. |
| `Cmd.SendPort` + `Sub.Port` | **Ping** sends on the `echo-out` outbound port; `index.html` echoes it back on the `echo` inbound port, folded into a msg by the `Sub.Port` subscription. |
| `Sub.OnAnimationFrame` | **Start/Stop frames** toggles the rAF leaf in/out of the `Batch`, so the runtime's subscription diff installs / tears it down live. |
| `Sub.OnKeyDown`        | Document-level key listener; shows the last key pressed anywhere on the page. |

## Run

```sh
make leafdemo        # JS-build the bundle
# fetch needs a real origin — file:// blocks it, so serve over HTTP:
python3 -m http.server -d examples/teaweb/leafdemo
# open http://localhost:8000/
```

## What to watch

- **Fetch** flips `result:` to `HTTP 200 — {"msg":"hello from fetch",...}`.
- **Roll** lands a 1–6 face.
- **Start frames** makes `frames:` race upward; **Stop frames** freezes it
  (the rAF subscription was torn down, not merely ignored).
- Typing any key updates `last key:`.
- **Ping** bumps `sent:` and, ~60 ms later, `echo:` shows
  `host echoed: ping #N` — proof the payload made a full round-trip out
  through `SendPort` and back in through `Sub.Port`.
