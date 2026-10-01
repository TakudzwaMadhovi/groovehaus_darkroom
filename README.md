# Groovehaus Darkroom

Desktop image processing app in Clojure + JavaFX.

## Run

Requires JDK 17+ and [Leiningen](https://leiningen.org).

    lein run                    # opens resources/sample.png
    lein run path/to/image.jpg  # opens another image
    lein run path/to/photo.dng  # RAW: DNG, ARW, CR2/CR3, NEF, ORF, RAF, RW2, ...
    lein test

JavaFX natives are picked automatically per OS/arch (see `project.clj`).

## Layout

| Namespace | Role |
|---|---|
| `darkroom.imaging.core` | Pure pixel logic: load, `fit` (preview downscale), brightness, contrast, gamma, saturation. No UI imports. |
| `darkroom.imaging.histogram` | `compute`: per-channel (R, G, B, luma) 256-bin counts. Pure logic. |
| `darkroom.imaging.export` | `save!`: write JPEG (quality 1–100) or PNG (lossless), atomically. |
| `darkroom.imaging.raw` | LibRaw (Bytedeco) RAW decoding: `decode-linear` -> 16-bit linear RGB, `linear->display` -> sRGB 8-bit. |
| `darkroom.imaging.loader` | `load-image`: RAW files via LibRaw, everything else via ImageIO. |
| `darkroom.imaging.browser` | `scan` (images in a folder, natural order) and `thumbnail`. |
| `darkroom.imaging.exif` | EXIF orientation reader (metadata-extractor); `core/orient` applies it. |
| `darkroom.imaging.denoise` | Non-local-means noise reduction via OpenCV (offline, CPU only). |
| `darkroom.imaging.pipeline` | Registry mapping settings (`{:brightness 20}`) to operations. |
| `darkroom.ui.histogram-view` | Canvas that draws histogram data. |
| `darkroom.ui.export-dialog` | Dialog for folder, file name, format, quality. |
| `darkroom.ui.browser-view` | Sidebar: folder picker + lazily loaded thumbnail list. |
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

## RAW support

`darkroom.imaging.raw/decode-linear` returns scene-linear data: interleaved 16-bit RGB, gamma 1.0, sRGB/Rec. 709 primaries, camera white balance applied, no auto-brightening. The editor currently works on sRGB-encoded 8-bit, so `loader/load-image` converts linear -> sRGB before the pipeline. Native LibRaw 0.21.2 comes from `org.bytedeco/libraw` (macOS x64/arm64, Linux x64, Windows x64; no Linux arm64 build). Native memory is outside the `-Xmx` heap cap.

## File browser

The left sidebar lists the images in the open image's folder (**Open Folder…** switches folders; sub-folders are not scanned). Click a thumbnail, or use the Up/Down arrow keys, to open it; sliders reset for each image. Thumbnails load only for rows scrolled into view, on a background thread, and the last 400 are cached in memory. Dot-files (including macOS `._` files) are ignored. JPEG/PNG/TIFF thumbnails and the main view honour the EXIF orientation tag (phone photos display upright); RAW thumbnails use the camera's embedded JPEG preview when it is at least 176 px, otherwise a half-size decode. Rapid navigation drops stale loads, so only the last-selected image is opened.

## Denoise

The **Denoise** slider (0-100) runs OpenCV's non-local-means colour denoiser (`fastNlMeansDenoisingColored`, Bytedeco JavaCPP build; no network, no GPU). It is the first pipeline stage so later contrast/brightness changes do not amplify noise. Native cost on Intel macOS: OpenCV ~28 MB + OpenBLAS ~16 MB jars (Linux/Windows/ARM Mac pick their own classifier in `project.clj`).

Keeping it responsive:
- Runs on the render thread, never the UI thread; stale requests are dropped.
- While a slider moves, a fast **draft** pass runs (small search window, see `denoise/quality-params`); 300 ms after it stops, a **preview**-quality pass refines it. Export uses the slowest **final** quality (7 / 21) on the full-resolution image.
- `pipeline/renderer` caches each stage, so moving brightness/contrast/gamma/saturation never re-runs denoise.
- OpenCV is limited to `cores - 1` threads and its natives are loaded in the background at start-up (about 1.5 s the first time).

Measured here (4 cores, 1600x1067 synthetic noisy image): draft ~350 ms, preview ~610 ms, final ~1.4 s. Quality is checked in tests by PSNR against a known clean image.
