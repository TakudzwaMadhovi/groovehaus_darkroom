# Groovehaus Darkroom

Photo editor in Clojure + JavaFX. The UI follows the Groovehaus design handoff
(black canvas, bone type, one sun-yellow accent, Bebas Neue / Cormorant Garamond /
Rajdhani); the engine is pure Clojure plus OpenCV (denoise) and LibRaw (RAW).

## Run

Requires JDK 17+ and [Leiningen](https://leiningen.org).

    lein run                     # opens the Library
    lein run path/to/photo.jpg   # opens that frame in Develop (shoot = its folder)
    lein test

JavaFX / OpenCV / LibRaw natives are chosen per OS in `project.clj`.
Edits are non-destructive; the catalog (shoots, ratings, adjustments, history)
is stored as EDN in the per-user data folder
(`~/Library/Application Support/Groovehaus Darkroom/catalog.edn` on macOS).

## Views

- **Library**: shoots sidebar (`NEW +`), filters ALL / PICKS / EDITED, thumbnail
  size slider, tiles with index, star (pick = 5), rating dots, EDITED flag.
  Click selects, double-click or `↵` opens Develop. Drag files/folders onto the grid
  or use `IMPORT +` to add frames to the open shoot.
- **Develop**: canvas with AFTER/BEFORE, luma histogram, tabs
  BASIC (exposure, contrast, highlights, shadows, temperature, saturation, denoise),
  CURVE, LOOK (B&W, fade, grain, vignette), CROP (straighten, aspect, flip),
  PRESETS, HISTORY; filmstrip "THE ROLL".
- **Export** (`⌘E`): JPEG/PNG, long edge 1080 / 2048 / full, JPEG quality, destination
  folder. Renders the full-resolution original; never overwrites (adds `-2`, `-3`).

Shortcuts: `⌘I` import · `⌘E`/`E` export · `Esc` close · `\` hold = before ·
`← →` frames (wraps) · `0–5` rating (same key clears) · `G` Library · `D` Develop ·
`↵` open (Library). Double-click or click a slider's label to reset it.

## Layout

| Namespace | Role |
|---|---|
| `darkroom.imaging.core` | Pixel helpers: load, `fit`, `orient`, legacy brightness/contrast/gamma ops |
| `darkroom.imaging.develop` | Develop engine: geometry (crop/straighten/flip), tone pipeline, curve LUT, resize. Port of the handoff's reference engine (parity-tested) |
| `darkroom.imaging.denoise` | OpenCV non-local-means denoise (offline, CPU) |
| `darkroom.imaging.pipeline` | Stages denoise → geometry → tone, with a per-stage cache for interactive use |
| `darkroom.imaging.raw` / `loader` / `exif` | LibRaw decode to linear RGB, single image loader, EXIF orientation |
| `darkroom.imaging.histogram` / `export` / `browser` | Histogram, JPEG/PNG writer, folder scan + thumbnails |
| `darkroom.catalog` | Pure library model: shoots, frames, ratings, adjustments, history, presets; EDN persistence |
| `darkroom.ui.state` | App state atom and actions |
| `darkroom.ui.theme` / `widgets` / `darkroom.css` | Fonts, stylesheet, tracked text, buttons, slider |
| `darkroom.ui.header` / `library` / `develop` / `curve` / `export-overlay` / `app` | Views and wiring |
| `darkroom.ui.canvas` / `thumbs` | Background loading, rendering and thumbnail caches |

## Performance notes

- Sliders re-render a downscaled working copy (1400 px, 2000 px on Retina) off the UI
  thread; stale requests are dropped. Tone and geometry are fused per-pixel loops
  split across cores (about 50 ms for a 1.7 MP working copy, 0.9 s for 24 MP in tests).
- Denoise runs at :draft quality while a slider moves and refines 300 ms later;
  `pipeline/renderer` caches each stage so tone changes never re-run denoise.
- JVM: `-Djava.awt.headless=true -Xmx2g` (see `project.clj`; pass them yourself for a jar).

## Notes on the design port

JavaFX has no CSS letter-spacing, so tracked labels insert thin/hair spaces.
Sliders are native `Slider`s styled in CSS with a gradient fill. The design's
BASIC tab has no denoise or brightness/gamma controls: DENOISE was added as a 7th
BASIC slider; the earlier brightness/gamma ops remain in `core` but are not in the UI.
