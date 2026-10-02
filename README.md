# groovehaus_darkroom
Lightroom-like photo editor (Groovehaus brand, bone theme).

Static web app: no build step, no dependencies.

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
