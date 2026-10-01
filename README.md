# Groovehaus Darkroom

Photo editor in Clojure + JavaFX. The UI follows the Groovehaus design handoff
(black canvas, bone type, one sun-yellow accent, Bebas Neue / Cormorant Garamond /
Rajdhani); the engine is pure Clojure plus OpenCV (denoise) and LibRaw (RAW).

Editing is done on **float, scene-linear pixels in a wide-gamut working space**, not
on 8-bit sRGB: RAW files keep their 16-bit precision, exposure and white balance act
on linear light, over-range highlights survive until the final clamp, and export
converts to sRGB, Display P3 or Adobe RGB with the ICC profile and camera EXIF
embedded.

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
- **Develop**: canvas with AFTER/BEFORE, histogram (luma or RGB, with shadow/highlight
  clipping readout; `CLIP` or `J` paints clipped pixels red/blue), tabs
  BASIC (AUTO tone, AUTO WB, PICK WB eyedropper; exposure, contrast, highlights, shadows,
  whites, blacks, temperature, tint, vibrance, saturation), DETAIL (texture, clarity, dehaze, sharpening with radius and
  edge masking, luminance and colour noise reduction), COLOR (8-band HSL mixer, split
  toning with balance), CURVE (RGB and per-channel red/green/blue curves), LOOK (B&W,
  fade, grain, vignette), CROP (interactive crop rectangle with aspect lock, straighten,
  90° turns, flips, perspective, lens distortion, chromatic aberration), PRESETS, HISTORY;
  filmstrip "THE ROLL".
- **Export** (`⌘E`): JPEG/PNG, long edge 1080 / 2048 / full, colour space (sRGB,
  Display P3, Adobe RGB), JPEG quality, destination folder. Renders the full-resolution
  original; never overwrites (adds `-2`, `-3`). The output carries its ICC profile; JPEGs
  also carry the camera's EXIF (make, model, lens, exposure, ISO, dates, copyright).

Shortcuts: `⌘I` import · `⌘E`/`E` export · `Esc` close · `\` hold = before ·
`← →` frames (wraps) · `0–5` rating (same key clears) · `G` Library · `D` Develop ·
`↵` open (Library). Double-click or click a slider's label to reset it.

## Layout

| Namespace | Role |
|---|---|
| `darkroom.imaging.color` | Colour science: RGB spaces, 3x3 matrices, transfer curves, Bradford adaptation, white-balance (temperature/tint) matrix, ICC profile generation |
| `darkroom.imaging.scene` | Float scene-linear images in the working space; conversion from 8-bit sRGB / LibRaw 16-bit and to 8/16-bit output spaces; LUT helpers; linear-light box downscale |
| `darkroom.imaging.geometry` | Crop, straighten, quarter turns, flips, perspective, lens distortion, CA correction in one resampling pass with an exact fill-zoom; resize |
| `darkroom.imaging.auto` | Auto tone (exposure/contrast/highlights/shadows/whites/blacks), auto white balance, white balance from a picked colour (Newton solve on the temperature/tint matrix) |
| `darkroom.imaging.crop` | The crop rectangle's drag maths (move, resize, draw, aspect lock, hit testing) |
| `darkroom.imaging.core` | 8-bit ARGB pixel helpers: load, `fit`, `orient`, legacy brightness/contrast/gamma ops |
| `darkroom.imaging.develop` | Tone engine on scene images: linear exposure + white balance, then perceptual-domain edits (whites/blacks, shadows/highlights, contrast, curves, HSL, vibrance, split toning, B&W, vignette, grain) |
| `darkroom.imaging.denoise` | OpenCV non-local-means denoise on 16-bit data (offline, CPU) |
| `darkroom.imaging.detail` | Texture, clarity, sharpening (luminance only), dehaze, chroma-noise reduction; Gaussian blur on a reduced copy for large radii |
| `darkroom.imaging.pipeline` | Stages denoise → colour NR → geometry → dehaze → tone → detail, with a per-stage cache for interactive use |
| `darkroom.imaging.raw` / `loader` / `exif` | LibRaw decode (16-bit linear, working space), single image loader, EXIF orientation/read/write |
| `darkroom.imaging.histogram` / `export` / `browser` | Histogram, JPEG/PNG writer with ICC + EXIF, folder scan + thumbnails |
| `darkroom.catalog` | Pure library model: shoots, frames, ratings, adjustments, history, presets; EDN persistence |
| `darkroom.ui.state` | App state atom and actions |
| `darkroom.ui.theme` / `widgets` / `darkroom.css` | Fonts, stylesheet, tracked text, buttons, slider |
| `darkroom.ui.header` / `library` / `develop` / `curve` / `crop-overlay` / `export-overlay` / `app` | Views and wiring |
| `darkroom.ui.canvas` / `thumbs` | Background loading, rendering and thumbnail caches |

## Accessibility

- **Keyboard:** everything is reachable with Tab. Focus rings show only while the
  keyboard is in use (like `:focus-visible`). Library tiles take arrow keys (Up/Down
  move by row), Space selects, Enter opens. The tone curve takes Left/Right (choose
  a point), Up/Down (1%, Shift 5%, PageUp/PageDown 10%), Home/End, Backspace (reset).
  Lists that rebuild (tiles, shoots, filmstrip, history, crop pills) keep keyboard focus.
  The export dialog is modal: focus moves in, nothing behind it is reachable, `Esc`
  closes it and focus returns.
- **Screen readers:** every control has a plain-text accessible name (tracked labels
  are announced without the letter-spacing characters), toggles announce
  ", selected", tiles announce name, position, rating and edited state, photos have
  alt text, the curve announces the point and its value. JavaFX exposes this to
  VoiceOver (macOS) and Narrator/NVDA (Windows).
- **Contrast:** all text is at least WCAG AA 4.5:1. The handoff's 50-55% bone text
  was raised to 60% (>= 5.3:1 on every surface); `test/darkroom/ui/contrast_test.clj`
  checks the stylesheet.
- **Audit:** `xvfb-run -a lein run -m clojure.main scripts/a11y_audit.clj <folder>` walks the
  live scene and exercises the keyboard behaviour above.
- **Known limits:** JavaFX has no live regions, so the toast (e.g. "EXPORTED") is not
  announced; the star and filter chips keep the design's 36 px targets; not tested
  with VoiceOver/Narrator.

## Performance notes

- Sliders re-render a downscaled working copy (1400 px, 2000 px on Retina) off the UI
  thread; stale requests are dropped. Tone and geometry are fused per-pixel loops
  split across cores (about 0.1 us per pixel per stage on 4 cores: 6 MP in 0.6 s).
- Denoise runs at :draft quality while a slider moves and refines 300 ms later;
  `pipeline/renderer` caches each stage so tone changes never re-run denoise.
- Memory: a float image is 12 bytes per pixel. The JVM heap is `-XX:MaxRAMPercentage=60`
  (see `project.clj`; pass `-Djava.awt.headless=true -XX:MaxRAMPercentage=60` yourself for a jar).
- Rendering is CPU-only; there is no GPU path.

## Colour pipeline

1. **Decode.** RAW: LibRaw, 16-bit linear, camera white balance, output colour 4
   (a wide-gamut space defined in `color/spaces :working`; `raw-test` re-measures it
   against a synthetic DNG). Other formats: 8-bit sRGB, linearised exactly (round trip is
   bit-exact).
2. **Linear light.** White balance (temperature/tint as a Bradford adaptation
   between source and D65 whites) and exposure (a true `2^EV` gain).
3. **Perceptual domain.** Shadows/highlights, contrast, fade, tone curve, saturation,
   black & white, vignette and grain run on sRGB-encoded values of the same pixels (as
   the design reference did). Values are not clamped here, only at the end.
4. **Output.** Matrix to the chosen space, clamp (gamut clip), that space's curve,
   quantise. The preview is always sRGB; there is no display-profile management, so on a
   wide-gamut monitor the preview is shown as sRGB.

Alpha is not carried: images are treated as opaque photographs.

## Notes on the design port

JavaFX has no CSS letter-spacing, so tracked labels insert thin/hair spaces.
Sliders are native `Slider`s styled in CSS with a gradient fill. The design's
BASIC tab has no denoise, tint or brightness/gamma controls: DENOISE and TINT were added as
BASIC sliders; the earlier brightness/gamma ops remain in `core` but are not in the UI.
The tone maths was moved from the design reference's 8-bit gamma-space engine to the float
pipeline above, so exposure and temperature no longer match the reference pixel for pixel
(the reference multiplied encoded values; exposure is now real light) and the presets look
slightly different from the prototype.
