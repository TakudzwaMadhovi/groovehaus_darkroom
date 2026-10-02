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
