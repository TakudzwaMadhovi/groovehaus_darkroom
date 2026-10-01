# Groovehaus Darkroom

Desktop image processing app in Clojure + JavaFX.

## Run

Requires JDK 17+ and [Leiningen](https://leiningen.org).

    lein run                    # opens resources/sample.png
    lein run path/to/image.jpg  # opens another image
    lein test

JavaFX natives are picked automatically per OS/arch (see `project.clj`).

## Layout

| Namespace | Role |
|---|---|
| `darkroom.imaging.core` | Pure pixel logic: load, `fit` (preview downscale), brightness, contrast, gamma, saturation. No UI imports. |
| `darkroom.imaging.histogram` | `compute`: per-channel (R, G, B, luma) 256-bin counts. Pure logic. |
| `darkroom.imaging.export` | `save!`: write JPEG (quality 1–100) or PNG (lossless), atomically. |
| `darkroom.imaging.pipeline` | Registry mapping settings (`{:brightness 20}`) to operations. |
| `darkroom.ui.histogram-view` | Canvas that draws histogram data. |
| `darkroom.ui.export-dialog` | Dialog for folder, file name, format, quality. |
| `darkroom.ui.view` | JavaFX window and controls. Calls a `render-fn`; knows no image math. |
| `darkroom.main` | Wires the two together. |

## Adding a feature

1. Write `(fn [image value])` in `darkroom.imaging.core` (or a new namespace) and unit-test it.
2. Add `[:setting-key op-fn neutral-value]` to `operations` in `pipeline.clj`.
3. Add an entry to `controls` in `darkroom.ui.view` that includes `:setting-key` in the settings map.

`resources/sample.png` is regenerated with `lein run -m clojure.main scripts/make_sample.clj`.

## Performance notes (2020 Intel MacBook Air target)

- Sliders re-render a downscaled preview (longest side 1600 px, `preview-max-side` in `main.clj`), off the UI thread; stale requests are dropped.
- Per-channel ops use 256-entry lookup tables and split work across cores.
- `project.clj` sets `-Djava.awt.headless=true` (avoids AWT/JavaFX conflicts on macOS) and `-Xmx2g`. These apply to `lein run`; pass them yourself when running a jar.
- Double-click a slider to reset it.
- **Export…** re-renders the full-resolution original with the current slider settings (not the preview) and writes it to the chosen folder. JPEG flattens transparency onto white.
