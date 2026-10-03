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

**Linux ARM64:** supported, with two differences. JavaFX is 22.0.2 there (21.x has no
linux-aarch64 build on Maven Central; it runs on JDK 17+). And Bytedeco publishes no LibRaw
natives for ARM64, so RAW files are decoded by LibRaw's own `dcraw_emu` tool
(`sudo apt install libraw-bin`, or set `DARKROOM_DCRAW_EMU` to its path). Results match the
native decoder (same LibRaw, same options); it is slower because pixels pass through a pipe,
and embedded previews are found by scanning the file for the largest JPEG. Without `dcraw_emu`
installed, RAW files are reported as unreadable and everything else works.
`DARKROOM_RAW_BACKEND=cli` forces this path on any platform. This path is tested on x86_64
only; no ARM64 hardware was available.
Edits are non-destructive; the catalog (shoots, ratings, adjustments, history)
is stored as EDN in the per-user data folder
(`~/Library/Application Support/Groovehaus Darkroom/catalog.edn` on macOS).

## Views

- **Library**: shoots sidebar (`NEW +`), filters ALL / PICKS / EDITED / REJECTED, search
  (file name, title, caption, creator, keywords, camera), sort by import order, name
  (natural: IMG_2 before IMG_10), rating, capture time or edited, colour-label filter,
  thumbnail size slider, tiles with index, star (pick = 5), rating dots, colour dot, EDITED
  and REJECTED flags. Click selects, `⌘`/`Ctrl`-click and `Shift`-click select several,
  `⌘A` all, double-click or `↵` opens Develop. `SURVEY` (`N`) shows the selected frames
  large side by side. The right-hand panel edits the selection: camera details (EXIF),
  colour label, reject, title / caption / creator / copyright, keywords, and COPY / PASTE
  settings, VIRTUAL COPY, REMOVE (from the shoot; files are never touched) and WRITE XMP.
  Drag files/folders onto the grid or use `IMPORT +` to add frames to the open shoot.
- **Develop**: canvas with AFTER/BEFORE, histogram (luma or RGB, with shadow/highlight
  clipping readout; `CLIP` or `J` paints clipped pixels red/blue), tabs
  BASIC (AUTO tone, AUTO WB, PICK WB eyedropper; exposure, contrast, highlights, shadows,
  whites, blacks, temperature, tint, vibrance, saturation), DETAIL (texture, clarity, dehaze, sharpening with radius and
  edge masking, luminance and colour noise reduction), COLOR (8-band HSL mixer, split
  toning with balance), CURVE (RGB and per-channel red/green/blue curves), LOOK (B&W,
  fade, grain, vignette), CROP (interactive crop rectangle with aspect lock, straighten,
  90° turns, flips, perspective, lens distortion, chromatic aberration), LOCAL (masked
  adjustment layers: linear/radial gradients, brush, luminance/colour range, subject, sky;
  13 adjustments each, amount, feather, invert, limits, mask preview), SPOTS (heal or clone
  blemishes by clicking), PRESETS (built-in and your own: save, apply, delete, import /
  export as a file; copy / paste settings), HISTORY (undo / redo, click a step to go back to
  it, named snapshots), INFO (the same metadata panel as the Library);
  filmstrip "THE ROLL".
- **Export** (`⌘E`): this frame, the selection or the whole shoot (batch, with progress);
  JPEG, PNG, TIFF (16-bit, deflate) or WebP; long edge 1080 / 2048 / full; colour space
  (sRGB, Display P3, Adobe RGB; WebP is always sRGB; `TIFF32` stores the linear working space unconverted); JPEG / WebP quality; output
  sharpening for screen, matte or glossy print (low / standard / high, applied after the
  resize); metadata (all camera data, copyright and creator only, or none; the frame's own
  creator / copyright notes override the camera's); optional text watermark with position;
  file-name template (`{name}` `{n}` `{n3}` `{date}` `{rating}` `{shoot}`); destination
  folder; presets WEB, PRINT and SOCIAL. Renders the full-resolution original; never
  overwrites (adds `-2`, `-3`). JPEG, PNG and TIFF carry their ICC profile; EXIF is written
  into JPEG only (not TIFF, PNG or WebP). The dialog's choices last for the session, not
  across restarts.

Shortcuts: `⌘I` import · `⌘E`/`E` export · `Esc` close / clear selection · `\` hold = before ·
`← →` frames (wraps) · `0–5` rating (same key clears) · `X` reject · `6–9` red / yellow /
green / blue label · `⌘Z` undo · `⌘⇧Z` or `⌘Y` redo · `⌘⇧C` / `⌘⇧V` copy / paste settings ·
`J` clipping view · `N` survey · `G` Library · `D` Develop · `↵` open (Library).
(`⌘` is `Ctrl` on Windows and Linux.) Double-click or click a slider's label to reset it.

### Lens profiles

CROP tab → LENS PROFILE. Choose the `db` folder of a [lensfun](https://lensfun.github.io)
checkout (`LENS DATABASE…`; the open calibration collection used by darktable and
RawTherapee, not bundled here) and `AUTO LENS PROFILE` looks the photo's lens up by the
name in its EXIF, picks the calibration for its sensor size and focal length, and corrects
distortion (poly3, poly5 and ptlens models) and linear lateral chromatic aberration. The
maths follows lensfun's own source: Hugin-normalised coefficients are rescaled into focal-length
units, calibrations are interpolated between focal lengths with the same Hermite spline,
and the 35 mm-equivalent focal length in the EXIF gives the crop factor. It is a regular
adjustment (one history step, `PROFILE OFF` removes it) and stacks with the manual
distortion and fringe sliders. Not implemented: vignetting, the ACM and poly3 TCA models,
lens-centre offsets, and matching by anything but the EXIF lens name (a lens whose name
the camera writes differently from the database finds no profile; the toast says so).

### Plugins

Clojure files in the `plugins` folder inside the data folder (next to `catalog.edn`; shown on
the PLUGINS tab when none are loaded) are loaded when the editor starts. A plugin can register
**image filters**, which get a slider per parameter on the PLUGINS tab, run after the built-in
stages on the float scene image, are cached, undoable, copied with settings and skipped at
their defaults; and **hooks** that run after an export or an import:

```clojure
(ns my-plugin (:require [darkroom.plugin :as plugin]))
(plugin/register-filter!
  {:id :my-look :name "MY LOOK"
   :params [{:key :amount :label "AMOUNT" :min 0 :max 1 :default 0.0}]
   :run (fn [img {:keys [amount]} opts] img)})        ; img {:width :height :data floats}, linear, may exceed 1
(plugin/on! :after-export (fn [{:keys [file]}] (println "exported" (str file))))
```

`examples/plugins/warm_glow.clj` is a complete example. A plugin is ordinary code run with the
app's full privileges: only install files you trust. Nothing is downloaded or loaded from
anywhere else. A plugin that fails to load, throws while rendering, or whose hook throws is
reported and skipped; it cannot stop the editor or other plugins.

### HDR merge and panoramas

Select two or more frames in the Library and use `HDR MERGE` or `PANORAMA` (right-hand panel).
The frames are read unedited at full resolution and the result is saved next to the first
frame as `<name>-HDR.tif` / `<name>-Pano.tif` (never overwriting), imported, and selected.

- **HDR merge** works in linear light on the float scene images: frames are aligned (phase
  correlation, whole-pixel translation, so a hand-held bracket is fine; rotation is not
  corrected), their relative exposures come from the EXIF shutter / ISO / aperture (or, when
  a frame lacks them, from the pictures, chained through neighbouring frames), and every
  pixel is the weighted mean of the frames' values divided by their exposure; clipped and
  noise-level samples weigh nothing and longer exposures weigh more. Highlights the middle
  frame clipped come back above 1.0. Best from RAW; JPEG brackets work but their tone curve
  is only approximately undone.
- **Panorama**: AKAZE features, homographies by RANSAC refined by ECC, frames chained through
  the strongest overlaps, gain compensation for exposure differences, feathered blending, and
  a crop applied to the new frame that hides the ragged edge (drag it larger on the CROP tab).
  Needs roughly 20–30% overlap and some texture. Planar homographies suit modest fields of
  view; there is no cylindrical / spherical projection or bundle adjustment, so wide sweeps or
  scenes with strong parallax will misalign. Linux needs the GTK 2 libraries (`libgtk2.0-0`)
  for OpenCV's feature modules.
- The result is a **32-bit float TIFF** in the linear working space (also an export format,
  `TIFF32`): bit-exact, nothing above 1.0 clipped, tagged with a linear ICC profile.

### Camera profiles (RAW)

COLOR tab → CAMERA PROFILE. By default RAW files use LibRaw's built-in camera matrix.
`ADD PROFILE…` takes DNG camera profiles (`.dcp`, e.g. from Lightroom / Camera Raw /
Adobe's DNG Profile Editor, or the open ones shipped with RawTherapee); `MATCH MY CAMERA`
picks the one made for the photo's EXIF make and model, or pick one by name. The frame is
then decoded as the camera's own white-balanced RGB and rendered the way the DNG
specification describes: the as-shot white picks a point between the profile's two
illuminants (interpolated in mireds), the ForwardMatrix (or the ColorMatrix, adapted to D50)
gives XYZ, and the profile's hue / saturation / value table and look table move colours
in ProPhoto-primaries HSV, the part that gives a profile its style. `PROFILE TONE CURVE`
adds the profile's own tone curve (off by default: this editor has its own tone controls).
The choice is a per-frame setting (one history step); presets leave it alone; changing it
reloads the frame. Verified against real profiles (table order checked by smoothness) and a
synthetic DNG whose matrix-only profile reproduces LibRaw's own conversion.

`ADD PROFILE…` also takes **ICC input profiles** (`.icc` / `.icm`, matrix or LUT based, as
made by profiling software from a photographed chart): the white-balanced camera RGB goes
through the profile's conversion to the connection space (a 33³ table sampled from the JDK's
colour engine, spaced for fine shadows) and on to the working space; values above 1.0 keep
their colour. Checked with real profiles (sRGB v4, a LUT-based display profile): white stays
white and greys stay neutral. A profile is applied to the camera's own values as they come
from the demosaic, so use profiles made for linear raw data with the camera's white balance.

Not implemented: the DCP's DefaultBlackRender (Adobe's behaviour for it is not specified
precisely enough to reproduce), the profile policy flags, profiles for non-RAW files, and
four-colour sensors (CMYG / RGBE: their matrices are 4×3 and there is no sample file to
verify against), which fall back to the default decode path.

### Hot folder and sharing a catalog

- `WATCH FOLDER` (Library toolbar) imports every image that appears in a folder into the
  open shoot once it has finished being written; the newest frame becomes current. Point
  your camera software's save-to-folder setting at it for tethered-style shooting. This
  does not control the camera or show live view.
- To use one catalog on several computers, put it in a folder your sync tool shares
  (Dropbox, iCloud Drive, Syncthing): start with `-Dgroovehaus.catalog=/path/catalog.edn`
  or set `GROOVEHAUS_CATALOG`. Open it on one computer at a time. If the file was changed
  by another computer since this one last read or wrote it, the other version is kept
  beside it as `catalog.edn.conflict-<time>` before being overwritten; nothing merges
  automatically.

### Metadata and sidecars

Ratings, colour labels, keywords, title / caption / creator / copyright and this app's
edits can be written next to each original as an XMP sidecar (`photo.xmp`, `WRITE XMP`),
using the standard `xmp:Rating`, `xmp:Label`, `dc:*` and `crs:` (Camera Raw) fields, so
Lightroom, Camera Raw and darktable can read the rating, label and keywords; the edits are
also kept under this app's own namespace and are restored from it on import. Importing a
file that has a sidecar reads it. Another program's tone settings are not translated back
into this app's sliders. Thumbnails are cached on disk (`thumbs/` in the data folder) keyed
by path, modification time and size.

## Phone companion and catalog sync

**Phone companion (opt-in).** `PHONE` in the Library starts a small web server on this computer and shows a QR code. Scan it on a phone on the same Wi-Fi to browse the grid, view previews, and set rating, flag and colour label. The address carries a random 128-bit token that is exchanged for an HttpOnly, SameSite=Strict cookie; changes are form POSTs that require an `X-Requested-With` header. No file paths are exposed (frames use opaque ids). It is plain HTTP on the LAN, so use it only on a network you trust; stop it with the same pill. The QR code uses ZXing (`com.google.zxing/core`).

**WebDAV catalog sync.** `UPLOAD`/`DOWNLOAD` in the Library push or pull the catalog file to a WebDAV URL (Nextcloud, ownCloud, any DAV server). Uploads use `If-Match`/`If-None-Match` with the ETag; if the remote changed since your last sync the server answers 412 and nothing is overwritten. There is no automatic merge: you choose to download (the local catalog is first backed up as `catalog-before-pull-<ms>.edn`) or to upload deliberately. The path map (`local-prefix=remote-prefix`) rewrites image folders between computers. Machine-specific settings (sync, lens database, camera profiles, segmentation model) are never synced. The password is held in memory only. Image files themselves are not synced.

## Layout

| Namespace | Role |
|---|---|
| `darkroom.imaging.color` | Colour science: RGB spaces, 3x3 matrices, transfer curves, Bradford adaptation, white-balance (temperature/tint) matrix, ICC profile generation |
| `darkroom.imaging.scene` | Float scene-linear images in the working space; conversion from 8-bit sRGB / LibRaw 16-bit and to 8/16-bit output spaces; LUT helpers; linear-light box downscale |
| `darkroom.imaging.geometry` | Crop, straighten, quarter turns, flips, perspective, lens distortion, CA correction in one resampling pass with an exact fill-zoom; resize |
| `darkroom.imaging.auto` | Auto tone (exposure/contrast/highlights/shadows/whites/blacks), auto white balance, white balance from a picked colour (Newton solve on the temperature/tint matrix) |
| `darkroom.imaging.mask` | Mask planes: gradients, brush strokes, luminance/colour range, GrabCut subject and heuristic sky selection |
| `darkroom.imaging.local` | Local-adjustment layers (mask + limits + tone/detail adjustments blended through the mask); layer helpers |
| `darkroom.imaging.heal` | Spot clone and heal (mean-value cloning), automatic source patch |
| `darkroom.imaging.crop` | The crop rectangle's drag maths (move, resize, draw, aspect lock, hit testing) |
| `darkroom.imaging.core` | 8-bit ARGB pixel helpers: load, `fit`, `orient`, legacy brightness/contrast/gamma ops |
| `darkroom.imaging.develop` | Tone engine on scene images: linear exposure + white balance, then perceptual-domain edits (whites/blacks, shadows/highlights, contrast, curves, HSL, vibrance, split toning, B&W, vignette, grain) |
| `darkroom.imaging.denoise` | OpenCV non-local-means denoise on 16-bit data (offline, CPU) |
| `darkroom.imaging.detail` | Texture, clarity, sharpening (luminance only), dehaze, chroma-noise reduction; Gaussian blur on a reduced copy for large radii |
| `darkroom.imaging.pipeline` | Stages denoise → colour NR → geometry → dehaze → tone → detail, with a per-stage cache for interactive use |
| `darkroom.imaging.raw` / `loader` / `exif` | LibRaw decode (16-bit linear, working space), single image loader, EXIF orientation/read/write |
| `darkroom.imaging.histogram` / `export` / `browser` | Histogram, JPEG / PNG / 16-bit TIFF / WebP writers (ICC, EXIF), folder scan + thumbnails |
| `darkroom.imaging.output` | File-name templates, output sharpening, watermark, metadata modes, rendering one or many frames to disk |
| `darkroom.catalog` | Pure library model: shoots, frames, ratings, reject flags, colour labels, keywords, notes, adjustments, undo / redo history, snapshots, user presets, virtual copies, copy / paste, search and sort; EDN persistence |
| `darkroom.plugin` | Plugin registry, loader and hooks (image filters appear as the pipeline's last stage and the PLUGINS tab) |
| `darkroom.imaging.merge` / `panorama` / `combine` | HDR bracket merge, panorama stitching in linear light, and saving the result as a float TIFF frame |
| `darkroom.imaging.dcp` / `camera` | DNG camera profile (.dcp) reader; camera RGB → XYZ → working space with the profile's matrices, hue/sat/val table, look table and tone curve |
| `darkroom.imaging.lens` / `watch` | lensfun profile lookup, interpolation and rescaling; hot-folder watcher |
| `darkroom.imaging.xmp` / `paths` / `thumbcache` | XMP sidecar read / write; virtual-copy paths (`file#vcN`); on-disk thumbnail cache |
| `darkroom.ui.state` | App state atom and actions |
| `darkroom.ui.theme` / `widgets` / `darkroom.css` | Fonts, stylesheet, tracked text, buttons, slider |
| `darkroom.ui.header` / `library` / `info` / `develop` / `curve` / `crop-overlay` / `local-overlay` / `export-overlay` / `app` | Views and wiring |
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

## Local adjustments and spots: what they are, and are not

- Masks and spots are positioned as fractions of the *cropped, turned* picture, so set the
  crop first; changing it afterwards moves them with the frame, not with the content.
- **Subject** and **sky** are classical image analysis (GrabCut from a box you draw;
  colour + brightness + smoothness connected to the top edge), not trained models. They
  work on clear subjects against distinct backgrounds and open skies; they do not
  recognise people, hair or busy cloudscapes the way a neural network does.
- **AI subject** (`+ AI SUBJECT`, LOCAL tab) runs a salient-object network instead: choose a
  U²-Net or U²-Net-small ONNX file with `AI MODEL…` (for example `u2netp.onnx` from the
  rembg project; not bundled). The picture is squeezed to 320 x 320, the network's fused
  saliency map is scaled back up and feathered, then Amount / Feather / Invert and the
  range limits refine it. Tried on real photographs (a person with a helmet, a cat, a cup
  on a table): it follows the outline of the main object, not people specifically, so a
  busy scene or several equal subjects can select the wrong thing. It runs on the CPU
  through OpenCV's DNN module, about 0.3–0.9 s on a laptop-class core.
- **Heal** copies a nearby patch (chosen automatically) and blends its edge mismatch
  smoothly; it cannot invent detail, so it is for dust and small blemishes, not for
  removing large objects.

## Notes on the design port

JavaFX has no CSS letter-spacing, so tracked labels insert thin/hair spaces.
Sliders are native `Slider`s styled in CSS with a gradient fill. The design's
BASIC tab has no denoise, tint or brightness/gamma controls: DENOISE and TINT were added as
BASIC sliders; the earlier brightness/gamma ops remain in `core` but are not in the UI.
The tone maths was moved from the design reference's 8-bit gamma-space engine to the float
pipeline above, so exposure and temperature no longer match the reference pixel for pixel
(the reference multiplied encoded values; exposure is now real light) and the presets look
slightly different from the prototype.

## Mac app

`scripts/package-mac.sh` (run on a Mac with JDK 21 and Leiningen) builds `dist/Groovehaus Darkroom-<version>.dmg`.
The workflow `.github/workflows/mac-app.yml` does the same on GitHub for Apple silicon and Intel and uploads the
`.dmg` as a build artifact. The app is not code-signed: on first launch right-click it and choose Open (or run
`xattr -dr com.apple.quarantine "/Applications/Groovehaus Darkroom.app"`).

## Static web prototype (in the repo root)

The repository root also holds an earlier, separate front end: a static web app in
`index.html`, `app.js`, `engine.js`, `styles.css`, `fonts.css`, `assets/` and `startup/`.
It needs no build step and shares no code with the Clojure/JavaFX app above.

```sh
python3 -m http.server 8000   # or any static server
# open http://localhost:8000
```

- **Library**: shoots as folders, pick/filter/rate, drag-drop import.
- **Develop**: Adjust / Curve / Crop / Presets / History, before/after, filmstrip, export (JPEG/PNG).
- Shortcuts: ⌘I import · ⌘E export · `\` before/after · 0–5 rating · ← → frames · G / D library / develop.
- Storage: adjustments in `localStorage`, original files in IndexedDB (non-destructive).
- Startup card photos: put images in `startup/` and list file names in `startup/startup.md`.
- `engine.js` is the per-pixel pipeline (canvas, main thread); move to WebGL or a Worker for large images.
