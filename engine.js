/* Groovehaus Darkroom image engine: EXIF orientation, tone LUT, per-pixel render. */
const GH = (() => {
  const DEF = () => ({ exposure: 0, contrast: 0, highlights: 0, shadows: 0, temp: 0, saturation: 0, fade: 0, bw: 0, grain: 0, vignette: 0, angle: 0, aspect: 'orig', flip: false, curve: [0, .25, .5, .75, 1] });
  const PRESETS = [['ON THE DECKS', { bw: 1, contrast: .35, grain: .5, vignette: .4, fade: .05, exposure: .1, shadows: -.2 }], ['NEON EDGE', { contrast: .3, saturation: .35, shadows: -.3, vignette: .3, highlights: -.1 }], ['AFTER HOURS', { exposure: -.35, contrast: .2, temp: -.2, saturation: -.2, grain: .3, vignette: .5 }], ['SUNLIT', { temp: .45, exposure: .2, fade: .2, saturation: .1, highlights: -.2 }], ['SILVER', { bw: 1, contrast: -.1, fade: .3, grain: .25, highlights: -.2, shadows: .2 }], ['ORIGINAL', {}]];
  const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

  function exifOri(buf) {
    try {
      const v = new DataView(buf); if (v.getUint16(0) !== 0xFFD8) return 1;
      let o = 2;
      while (o + 4 < v.byteLength) {
        const m = v.getUint16(o), len = v.getUint16(o + 2);
        if (m === 0xFFE1 && v.getUint32(o + 4) === 0x45786966) {
          const t = o + 10, le = v.getUint16(t) === 0x4949, ifd = t + v.getUint32(t + 4, le), n = v.getUint16(ifd, le);
          for (let i = 0; i < n; i++) { const e = ifd + 2 + i * 12; if (v.getUint16(e, le) === 0x0112) return v.getUint16(e + 8, le) || 1; }
          return 1;
        }
        if ((m & 0xFF00) !== 0xFF00) break;
        o += 2 + len;
      }
    } catch (e) {}
    return 1;
  }

  // Decode a blob to an upright canvas, capped at maxEdge on the long side.
  async function orient(blob, maxEdge) {
    const ori = exifOri(await blob.arrayBuffer());
    const url = URL.createObjectURL(blob);
    const el = await new Promise((res, rej) => { const i = new Image(); i.onload = () => res(i); i.onerror = rej; i.src = url; });
    let src = el, o = 1;
    if (ori > 1) {
      try {
        const bm = await createImageBitmap(blob, { imageOrientation: 'none' });
        // if the browser already auto-rotated the <img>, don't rotate twice
        const raw = ori >= 5 ? bm.width !== el.naturalWidth || bm.width === bm.height : true;
        if (raw) { src = bm; o = ori; }
      } catch (e) {}
    }
    const w = src.width || src.naturalWidth, h = src.height || src.naturalHeight, sw = o >= 5;
    const k = Math.min(1, maxEdge / Math.max(w, h)), cw = Math.round((sw ? h : w) * k), ch = Math.round((sw ? w : h) * k);
    const c = document.createElement('canvas'); c.width = cw; c.height = ch;
    const x = c.getContext('2d'); x.scale(k, k);
    const T = { 2: [-1, 0, 0, 1, w, 0], 3: [-1, 0, 0, -1, w, h], 4: [1, 0, 0, -1, 0, h], 5: [0, 1, 1, 0, 0, 0], 6: [0, 1, -1, 0, h, 0], 7: [0, -1, -1, 0, h, w], 8: [0, -1, 1, 0, 0, w] }[o];
    if (T) x.transform(...T);
    x.drawImage(src, 0, 0, w, h);
    URL.revokeObjectURL(url);
    return c;
  }

  function meanTone(c) {
    const t = document.createElement('canvas'); t.width = t.height = 16;
    const x = t.getContext('2d', { willReadFrequently: true }); x.drawImage(c, 0, 0, 16, 16);
    const d = x.getImageData(0, 0, 16, 16).data; let s = 0;
    for (let i = 0; i < d.length; i += 4) s += .2126 * d[i] + .7152 * d[i + 1] + .0722 * d[i + 2];
    return s / (256 * 255);
  }

  function thumbOf(c) {
    const k = Math.min(1, 480 / Math.max(c.width, c.height)), t = document.createElement('canvas');
    t.width = Math.round(c.width * k); t.height = Math.round(c.height * k);
    t.getContext('2d').drawImage(c, 0, 0, t.width, t.height);
    return t.toDataURL('image/jpeg', .82);
  }

  // 5-point monotone cubic Hermite -> 256-entry LUT
  function lut(cv) {
    const X = [0, .25, .5, .75, 1], m = X.map((x, i) => { const a = Math.max(0, i - 1), b = Math.min(4, i + 1); return (cv[b] - cv[a]) / (X[b] - X[a]); });
    const L = new Float32Array(256);
    for (let i = 0; i < 256; i++) {
      const x = i / 255, k = Math.min(3, Math.floor(x / .25)), t = (x - X[k]) / .25, t2 = t * t, t3 = t2 * t;
      const y = (2 * t3 - 3 * t2 + 1) * cv[k] + (t3 - 2 * t2 + t) * .25 * m[k] + (-2 * t3 + 3 * t2) * cv[k + 1] + (t3 - t2) * .25 * m[k + 1];
      L[i] = Math.max(0, Math.min(1, y));
    }
    return L;
  }

  // Render `img` (canvas) with `adj` into `canvas`; returns the luma histogram.
  function render(canvas, img, adj, maxDim) {
    const iw = img.width, ih = img.height;
    const ar = adj.aspect === 'orig' ? iw / ih : { '1:1': 1, '4:5': .8, '16:9': 16 / 9, '3:2': 1.5 }[adj.aspect];
    const cw = Math.min(iw, ih * ar), ch = cw / ar, sc = Math.min(1, maxDim / Math.max(cw, ch));
    const w = Math.round(cw * sc), h = Math.round(ch * sc);
    canvas.width = w; canvas.height = h;
    const ctx = canvas.getContext('2d', { willReadFrequently: true }), th = adj.angle * Math.PI / 180, c = Math.abs(Math.cos(th)), sn = Math.abs(Math.sin(th));
    const z = Math.max(1, (cw * c + ch * sn) / iw, (cw * sn + ch * c) / ih) * sc;
    ctx.save(); ctx.translate(w / 2, h / 2); ctx.rotate(th); if (adj.flip) ctx.scale(-1, 1);
    ctx.drawImage(img, -iw * z / 2, -ih * z / 2, iw * z, ih * z); ctx.restore();
    const id = ctx.getImageData(0, 0, w, h), d = id.data, L = lut(adj.curve), hist = new Array(256).fill(0);
    const ex = Math.pow(2, adj.exposure), con = 1 + adj.contrast, sat = 1 + adj.saturation, tm = adj.temp, f = adj.fade;
    for (let y = 0, i = 0; y < h; y++) {
      const dy = (y / h - .5) * 2;
      for (let x = 0; x < w; x++, i += 4) {
        let r = d[i] / 255 * ex * (1 + tm * .2), g = d[i + 1] / 255 * ex, b = d[i + 2] / 255 * ex * (1 - tm * .2);
        let l = .2126 * r + .7152 * g + .0722 * b; l = l < 0 ? 0 : l > 1 ? 1 : l;
        const sh = adj.shadows * .35 * (1 - l) * (1 - l) + adj.highlights * .35 * l * l;
        r += sh; g += sh; b += sh;
        r = (r - .5) * con + .5; g = (g - .5) * con + .5; b = (b - .5) * con + .5;
        r = r * (1 - f * .25) + f * .12; g = g * (1 - f * .25) + f * .12; b = b * (1 - f * .25) + f * .12;
        r = L[(Math.max(0, Math.min(1, r)) * 255) | 0]; g = L[(Math.max(0, Math.min(1, g)) * 255) | 0]; b = L[(Math.max(0, Math.min(1, b)) * 255) | 0];
        let l2 = .2126 * r + .7152 * g + .0722 * b;
        r = l2 + (r - l2) * sat; g = l2 + (g - l2) * sat; b = l2 + (b - l2) * sat;
        l2 = .2126 * r + .7152 * g + .0722 * b;
        r += (l2 - r) * adj.bw; g += (l2 - g) * adj.bw; b += (l2 - b) * adj.bw;
        if (adj.vignette) { const dx = (x / w - .5) * 2, v = 1 - adj.vignette * Math.max(0, dx * dx + dy * dy - .3) * .7; r *= v; g *= v; b *= v; }
        if (adj.grain) { const q = Math.sin(x * 12.9898 + y * 78.233) * 43758.5453, n = (q - Math.floor(q) - .5) * adj.grain * .25; r += n; g += n; b += n; }
        r = r < 0 ? 0 : r > 1 ? 1 : r; g = g < 0 ? 0 : g > 1 ? 1 : g; b = b < 0 ? 0 : b > 1 ? 1 : b;
        d[i] = r * 255; d[i + 1] = g * 255; d[i + 2] = b * 255;
        hist[((.2126 * r + .7152 * g + .0722 * b) * 255) | 0]++;
      }
    }
    ctx.putImageData(id, 0, 0); return hist;
  }

  return { DEF, PRESETS, same, orient, meanTone, thumbOf, lut, render };
})();
