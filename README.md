# groovehaus_darkroom
Lightroom like editor

## Looks (`groovehaus.looks.*`)

Non-destructive, stackable stylistic looks. Pure Clojure + `java.awt` (no JavaFX/cloud dependency); `groovehaus.looks.fx` bridges to JavaFX `Image` reflectively.

```clojure
(require '[groovehaus.looks.core :as looks])

(def render (looks/renderer buffered-image))      ; source is never modified
(render [{:look-id :vintage-bw
          :params {:intensity 0.8 :grain-amount 0.2 :vignette-radius 0.6 :fade 0.1}
          :blend-mode :overlay}
         {:look-id :light-leaks :params {:position :bottom-left}}])
;; => new BufferedImage. Re-calling with an edited stack re-renders only from the first changed layer.

(looks/catalog)                                    ; look/param metadata for building sliders
(looks/stack->edn stack) / (looks/edn->stack s)    ; sidecar persistence
```

Built-in looks: `:vintage-bw` `:film-grain` `:light-leaks` `:cross-process` `:duotone`.
Blend modes: `:normal :multiply :screen :overlay :soft-light :hard-light :darken :lighten :color-dodge :color-burn :difference :add`.
Every look takes `:intensity` (0..1 mix of original -> blended result). Numeric params are clamped; unknown keys/looks/modes throw `ex-info`.
Add looks with `groovehaus.looks.registry/register-look!`.

Notes: working data is float sRGB (not linear). Grain is seeded (deterministic) and scales with image size. Whole-image float buffers cost ~12 bytes/pixel (24 MP ≈ 290 MB for RGB), so full-res export of large RAWs should run with a sufficient `-Xmx`.

Tests: `clojure -X:test` / `clojure -M:test`.

## Vector overlays (`groovehaus.vector.*`)

Offline SVG/path engine for JavaFX. Everything is in image-pixel coordinates.

| ns | what |
|---|---|
| `vector.path` | pure-data SVG path parser (`M m L l H h V v C c S s Q q T t A a Z z`, implicit repeats, compact numbers/arc flags) -> absolute segments; `segments->d`, `quad->cubic`, `arc->cubics`, `->cubics` |
| `vector.core` | segments -> JavaFX `Path` (`MoveTo`/`LineTo`/`CubicCurveTo`/`QuadCurveTo`/`ArcTo`/`ClosePath`); hiccup or SVG string -> `Group`/`Shape` nodes (`svg g path rect circle ellipse line polyline polygon`, `transform`, `viewBox`, inherited presentation attrs, `style=""`) |
| `vector.style` | fills, `LinearGradient`/`RadialGradient`, strokes (width/cap/join/dash), fill-rule, opacity, `DropShadow`; partial updates |
| `vector.trace` | mask `BufferedImage` -> contours (Suzuki-Abe border following, holes included) -> RDP-simplified -> `d` string; optional Catmull-Rom smoothing |
| `vector.ui` | transparent, clipped overlay `Pane` mounted over an `ImageView`; scales image-pixel content to the displayed size; add/replace/style/hide/remove shapes by id |

```clojure
(require '[groovehaus.vector.trace :as trace] '[groovehaus.vector.ui :as ui])

(def overlay (ui/make-overlay))
(ui/mount! stack-pane image-view overlay)          ; FX thread
(ui/add-path! overlay :cutout (trace/trace->path-d mask-image {:epsilon 1.5})
              {:style {:fill {:type :radial-gradient :stops [[0 "#ffd54f"] [1 "#e65100"]]}
                       :fill-rule :even-odd :stroke "#fff" :stroke-width 2
                       :effect {:type :drop-shadow :radius 10}}})
(ui/add-shape! overlay :badge [:g {:transform "translate(40 40)"} [:circle {:r 20 :fill "#09f"}]])
```

Mask rules: if the image has any transparent pixel it is an alpha cutout (alpha >= `:threshold`, default 128); otherwise white-on-black luminance. `:invert?` flips. Traced paths run through boundary pixel centres (half a pixel inside the true edge) and drop 1px hairlines/specks by default (`:min-area 1.0`). Render with `:fill-rule :even-odd` so holes cut through.
Not supported: SVG gradients/patterns via `url(#id)`, clipPath, text, CSS classes, `<use>`, ImageView viewport panning.
