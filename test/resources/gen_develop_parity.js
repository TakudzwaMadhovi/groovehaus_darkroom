// Generates test/resources/develop_parity.edn: the design handoff's reference
// per-pixel loop (engine-reference.js renderTo, identity geometry) run in node
// on a fixed pseudo-random image, so the Clojure port can be checked against it.
//   node test/resources/gen_develop_parity.js > test/resources/develop_parity.edn
function lut(cv) {
  const X = [0, .25, .5, .75, 1], m = X.map((x, i) => { const a = Math.max(0, i - 1), b = Math.min(4, i + 1); return (cv[b] - cv[a]) / (X[b] - X[a]); });
  const L = new Float32Array(256);
  for (let i = 0; i < 256; i++) {
    const x = i / 255; let k = Math.min(3, Math.floor(x / .25)); const t = (x - X[k]) / .25, t2 = t * t, t3 = t2 * t;
    const y = (2 * t3 - 3 * t2 + 1) * cv[k] + (t3 - 2 * t2 + t) * .25 * m[k] + (-2 * t3 + 3 * t2) * cv[k + 1] + (t3 - t2) * .25 * m[k + 1];
    L[i] = Math.max(0, Math.min(1, y));
  }
  return L;
}
function render(d, w, h, adj) {
  const L = lut(adj.curve);
  const ex = Math.pow(2, adj.exposure), con = 1 + adj.contrast, sat = 1 + adj.saturation, tm = adj.temp;
  for (let y = 0, i = 0; y < h; y++) {
    const dy = (y / h - .5) * 2;
    for (let x = 0; x < w; x++, i += 4) {
      let r = d[i] / 255 * ex * (1 + tm * .2), g = d[i + 1] / 255 * ex, b = d[i + 2] / 255 * ex * (1 - tm * .2);
      let l = .2126 * r + .7152 * g + .0722 * b; l = l < 0 ? 0 : l > 1 ? 1 : l;
      const sh = adj.shadows * .35 * (1 - l) * (1 - l) + adj.highlights * .35 * l * l;
      r += sh; g += sh; b += sh;
      r = (r - .5) * con + .5; g = (g - .5) * con + .5; b = (b - .5) * con + .5;
      const f = adj.fade; r = r * (1 - f * .25) + f * .12; g = g * (1 - f * .25) + f * .12; b = b * (1 - f * .25) + f * .12;
      r = L[(Math.max(0, Math.min(1, r)) * 255) | 0]; g = L[(Math.max(0, Math.min(1, g)) * 255) | 0]; b = L[(Math.max(0, Math.min(1, b)) * 255) | 0];
      let l2 = .2126 * r + .7152 * g + .0722 * b;
      r = l2 + (r - l2) * sat; g = l2 + (g - l2) * sat; b = l2 + (b - l2) * sat;
      l2 = .2126 * r + .7152 * g + .0722 * b;
      r += (l2 - r) * adj.bw; g += (l2 - g) * adj.bw; b += (l2 - b) * adj.bw;
      if (adj.vignette) { const dx = (x / w - .5) * 2, v = 1 - adj.vignette * Math.max(0, dx * dx + dy * dy - .3) * .7; r *= v; g *= v; b *= v; }
      if (adj.grain) { const q = Math.sin(x * 12.9898 + y * 78.233) * 43758.5453, n = (q - Math.floor(q) - .5) * adj.grain * .25; r += n; g += n; b += n; }
      r = r < 0 ? 0 : r > 1 ? 1 : r; g = g < 0 ? 0 : g > 1 ? 1 : g; b = b < 0 ? 0 : b > 1 ? 1 : b;
      d[i] = r * 255; d[i + 1] = g * 255; d[i + 2] = b * 255;   // Uint8ClampedArray: round-half-even
    }
  }
}
let seed = 12345; const rnd = () => (seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff;
const W = 24, H = 16;
const cases = [
  { name: "neutral", adj: { exposure: 0, contrast: 0, highlights: 0, shadows: 0, temp: 0, saturation: 0, fade: 0, bw: 0, grain: 0, vignette: 0, curve: [0, .25, .5, .75, 1] } },
  { name: "basic",   adj: { exposure: .6, contrast: .35, highlights: -.4, shadows: .5, temp: .3, saturation: .4, fade: 0, bw: 0, grain: 0, vignette: 0, curve: [0, .25, .5, .75, 1] } },
  { name: "look",    adj: { exposure: -.3, contrast: -.2, highlights: .2, shadows: -.3, temp: -.5, saturation: -.3, fade: .4, bw: .6, grain: .5, vignette: .6, curve: [0, .25, .5, .75, 1] } },
  { name: "curve",   adj: { exposure: .1, contrast: .1, highlights: 0, shadows: 0, temp: 0, saturation: .2, fade: 0, bw: 0, grain: 0, vignette: .3, curve: [.05, .15, .6, .8, .95] } },
  { name: "bw-preset", adj: { exposure: .1, contrast: .35, highlights: 0, shadows: -.2, temp: 0, saturation: 0, fade: .05, bw: 1, grain: .5, vignette: .4, curve: [0, .25, .5, .75, 1] } },
];
const base = new Uint8ClampedArray(W * H * 4);
for (let i = 0; i < base.length; i += 4) { base[i] = rnd() * 256; base[i + 1] = rnd() * 256; base[i + 2] = rnd() * 256; base[i + 3] = 255; }
const ed = a => a.join(" ");
let out = "{:width " + W + " :height " + H + "\n :input [" + ed(Array.from(base).filter((_, i) => i % 4 !== 3)) + "]\n :cases [\n";
for (const c of cases) {
  const d = new Uint8ClampedArray(base); render(d, W, H, c.adj);
  const a = c.adj;
  out += "  {:name \"" + c.name + "\" :settings {" + Object.keys(a).map(k => ":" + k + " " + (k === "curve" ? "[" + a[k].join(" ") + "]" : a[k])).join(" ") + "}\n   :output [" + ed(Array.from(d).filter((_, i) => i % 4 !== 3)) + "]}\n";
}
out += " ]}\n";
process.stdout.write(out);
