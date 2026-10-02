/* Groovehaus Darkroom UI. Vanilla JS; state in localStorage, originals in IndexedDB. */
(() => {
  const KEY = 'gh-darkroom-v4', $ = s => document.querySelector(s), app = $('#app'), fileEl = $('#file');
  const esc = s => String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  const BLANK = 'data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==';
  const DEF = GH.DEF;

  /* ---------- storage ---------- */
  const idb = new Promise(res => {
    const r = indexedDB.open('gh-darkroom', 1);
    r.onupgradeneeded = () => r.result.createObjectStore('blobs');
    r.onsuccess = () => res(r.result); r.onerror = () => res(null);
  });
  const blobOp = async (mode, fn) => { const db = await idb; if (!db) return; return new Promise(res => { const q = fn(db.transaction('blobs', mode).objectStore('blobs')); q.onsuccess = () => res(q.result); q.onerror = () => res(); }); };
  const putBlob = (id, b) => blobOp('readwrite', s => s.put(b, id));
  const getBlob = id => blobOp('readonly', s => s.get(id));

  let saved = null; try { saved = JSON.parse(localStorage.getItem(KEY)); } catch (e) {}
  const S = {
    shoots: saved?.shoots?.length ? saved.shoots : [{ id: 's1', name: 'FIRST SHOOT' }],
    photos: saved?.photos || [],
    view: 'library', shoot: null, cur: 0, tab: 'adjust', lf: 'all', tsz: 220,
    before: false, exporting: false, fmt: 'jpeg', size: 2048, q: 90, adding: false, newName: ''
  };
  S.shoot = S.shoots[0].id;
  const save = () => { try { localStorage.setItem(KEY, JSON.stringify({ shoots: S.shoots, photos: S.photos })); } catch (e) {} };

  /* ---------- image cache ---------- */
  const imgs = {}, thumbs = {}, tone = {};
  const getImg = p => imgs[p.id] ||= getBlob(p.id).then(b => GH.orient(b, 2400)).then(c => { thumbs[p.id] = GH.thumbOf(c); tone[p.id] = GH.meanTone(c); if (!drag) render(); return c; });
  let drag = false;

  /* ---------- helpers ---------- */
  const inShoot = () => S.photos.map((q, i) => i).filter(i => S.photos[i].shoot === S.shoot);
  const isEd = q => !GH.same(q.adj, DEF());
  const cur = () => S.photos[S.cur];
  const upd = (fn, i = S.cur) => { S.photos[i] = fn(S.photos[i]); save(); };
  const setAdj = (k, v) => upd(p => ({ ...p, adj: { ...p.adj, [k]: v } }));
  const commit = label => upd(p => { const last = p.history[p.history.length - 1]; if (last && GH.same(last.adj, p.adj)) return p; return { ...p, history: [...p.history.slice(-40), { label, adj: p.adj }] }; });
  const go = (view, c) => {
    c = c ?? S.cur;
    if (view === 'develop' && (!S.photos[c] || S.photos[c].shoot !== S.shoot)) { c = S.photos.findIndex(q => q.shoot === S.shoot); if (c < 0) return; }
    S.view = view; S.cur = c; render();
  };
  const nav = d => { const ix = inShoot(); if (!ix.length) return; const k = Math.max(0, ix.indexOf(S.cur)); S.cur = ix[(k + d + ix.length) % ix.length]; render(); };

  /* ---------- sliders ---------- */
  const stop = p => `calc(7px + (100% - 14px) * ${p})`;
  const trk = (p, from) => { const lo = Math.min(p, from), hi = Math.max(p, from), T = 'rgba(13,12,11,.2)', F = '#0D0C0B'; return `linear-gradient(to right,${T} 0,${T} ${stop(lo)},${F} ${stop(lo)},${F} ${stop(hi)},${T} ${stop(hi)},${T} 100%) center/100% 2px no-repeat`; };
  const SL = { exposure: ['Exposure', -2, 2, .01], contrast: ['Contrast', -1, 1, .01, 1], highlights: ['Highlights', -1, 1, .01, 1], shadows: ['Shadows', -1, 1, .01, 1], temp: ['Temperature', -1, 1, .01, 1], saturation: ['Saturation', -1, 1, .01, 1], bw: ['Black & white', 0, 1, .01, 1], fade: ['Fade', 0, 1, .01, 1], grain: ['Grain', 0, 1, .01, 1], vignette: ['Vignette', 0, 1, .01, 1], angle: ['Angle', -15, 15, .1] };
  const GROUPS = { adjust: [['LIGHT', ['exposure', 'contrast', 'highlights', 'shadows']], ['COLOUR', ['temp', 'saturation', 'bw']], ['EFFECTS', ['fade', 'grain', 'vignette']]], crop: [['STRAIGHTEN', ['angle']]] };
  const disp = k => { const [, min, , , pct] = SL[k], v = cur().adj[k]; return (v > 0 && min < 0 ? '+' : '') + (pct ? Math.round(v * 100) : v.toFixed(k === 'angle' ? 1 : 2)); };
  const track = k => { const [, min, max] = SL[k]; return trk((cur().adj[k] - min) / (max - min), min < 0 ? .5 : 0); };
  const slider = k => { const [label, min, max, step] = SL[k], v = cur().adj[k], ch = v !== DEF()[k];
    return `<div class="sl"><div class="r"><button data-a="reset" data-v="${k}" title="Click to reset">${label}</button><span id="v-${k}" class="${ch ? 'ch' : ''}">${disp(k)}</span></div><div class="w">${min < 0 ? '<span class="tick"></span>' : ''}<input class="gh-range" data-f="s-${k}" data-k="${k}" type="range" min="${min}" max="${max}" step="${step}" value="${v}" style="background:${track(k)}" aria-label="${label}"></div></div>`; };

  /* ---------- views ---------- */
  const seg = (items, cls = '') => `<div class="seg ${cls}">${items.map(([a, v, l, on]) => `<button data-a="${a}" data-v="${v}" class="${on ? 'on' : ''}" aria-pressed="${!!on}">${l}</button>`).join('')}</div>`;
  const header = () => `<header><div class="brand"><img src="assets/logo-medallion-white.png" alt=""><span><b>GROOVEHAUS</b><i>darkroom</i></span></div>
    ${seg([['view', 'library', 'Library', S.view === 'library'], ['view', 'develop', 'Develop', S.view === 'develop']])}
    <div class="hr"><button class="sb o" data-a="import">Import <span class="kbd">⌘I</span></button><button class="sb p" data-a="export">Export ↓ <span class="kbd">⌘E</span></button></div></header>`;

  function library() {
    const sp = inShoot().map(i => ({ q: S.photos[i], i })), picks = sp.filter(x => x.q.rating === 5), ed = sp.filter(x => isEd(x.q));
    const vis = sp.filter(({ q }) => S.lf === 'all' || (S.lf === 'picks' ? q.rating === 5 : isEd(q)));
    const sh = S.shoots.find(x => x.id === S.shoot);
    const tiles = vis.map(({ q, i }) => `<div class="tile ${i === S.cur ? 'on' : ''}" role="button" tabindex="0" data-f="t-${i}" aria-label="${esc(q.name)}${isEd(q) ? ', edited' : ''}${q.rating ? ', ' + q.rating + ' stars' : ''}. Enter to develop" aria-pressed="${i === S.cur}" data-a="select" data-v="${i}" data-dbl="${i}"><div class="im"><img src="${thumbs[q.id] || BLANK}" alt="" draggable="false" loading="lazy" decoding="async"><button class="star ${q.rating === 5 ? 'on' : ''}" aria-label="Pick ${esc(q.name)}" aria-pressed="${q.rating === 5}" title="Pick (5 stars)" data-a="pick" data-v="${i}">${q.rating === 5 ? '★' : '☆'}</button>${isEd(q) ? '<span class="badge">Edited</span>' : ''}</div><div class="nm"><b>${esc(q.name)}</b><span>${'★'.repeat(q.rating)}</span></div></div>`).join('');
    const empty = sp.length === 0 ? 'this shoot is empty — import frames to begin.' : vis.length === 0 ? 'nothing here — star a frame or change the filter.' : '';
    return `<main><aside><div class="sh-h"><span class="lbl">SHOOTS</span><button class="sm o" data-a="newshoot" title="New shoot">+ New</button></div>
      ${S.adding ? `<div class="newf" role="group" aria-label="New shoot"><input data-f="newname" aria-label="Shoot name" maxlength="28" placeholder="Name this shoot" value="${esc(S.newName)}"><div><button class="sm" data-a="create">Create ↵</button><button class="sm" style="color:var(--dim)" data-a="cancel">Cancel</button></div></div>` : ''}
      <div class="shl">${S.shoots.map(s => { const ps = S.photos.filter(q => q.shoot === s.id); return `<button class="sh ${s.id === S.shoot ? 'on' : ''}" data-a="shoot" data-v="${s.id}"><div class="cv"><img src="${ps[0] ? thumbs[ps[0].id] || BLANK : BLANK}" alt=""></div><div><b>${esc(s.name)}</b><span>${ps.length} ${ps.length === 1 ? 'frame' : 'frames'}</span></div></button>`; }).join('')}</div>
      <div class="cap">one folder per shoot. open it, pick, then develop.</div></aside>
    <section><div class="tb"><div class="t"><h1>${esc(sh.name)}</h1><div>${sp.length} frames · ${ed.length} edited · ${picks.length} picks</div></div>
      ${seg([['lf', 'all', 'All ' + sp.length, S.lf === 'all'], ['lf', 'picks', 'Picks ' + picks.length, S.lf === 'picks'], ['lf', 'edited', 'Edited ' + ed.length, S.lf === 'edited']], 's')}
      <div class="szr">Size<input class="gh-range" data-f="tsz" data-k="tsz" type="range" min="120" max="360" value="${S.tsz}" style="background:${trk((S.tsz - 120) / 240, 0)}" aria-label="Thumbnail size"></div>
      <button class="sb p" data-a="view" data-v="develop">Develop shoot →</button></div>
      <div class="grid-w" data-drop><div class="grid" id="grid" style="grid-template-columns:repeat(auto-fill,minmax(${S.tsz}px,1fr))">${tiles}<button class="add" data-a="import"><span>+</span><span>Import photos</span><span>or drop files here</span></button></div>${empty ? `<div class="empty">${empty}</div>` : ''}</div>
      <div class="status"><span>${sp.find(x => x.i === S.cur) ? esc(cur().name) + ' · ' : ''}${sp.length} in shoot</span><span>Double-click or ↵ to develop · 1–5 to rate · ☆ to pick</span></div></section></main>`;
  }

  function develop() {
    const p = cur(), adj = p.adj, sp = inShoot(), pos = sp.indexOf(S.cur) + 1;
    const nEd = Object.keys(DEF()).filter(k => !GH.same(adj[k], DEF()[k])).length;
    const grp = ([title, keys]) => `<div class="grp"><div><span class="lbl">${title}</span><button class="rs ${keys.some(k => adj[k] !== DEF()[k]) ? 'on' : ''}" data-a="greset" data-v="${title}">Reset</button></div>${keys.map(slider).join('')}</div>`;
    const tabs = [['adjust', 'Adjust'], ['curve', 'Curve'], ['crop', 'Crop'], ['presets', 'Presets'], ['history', 'History']];
    let body = (GROUPS[S.tab] || []).map(grp).join('');
    if (S.tab === 'curve') {
      const L = GH.lut(adj.curve), path = Array.from({ length: 41 }, (_, i) => { const x = i / 40; return (i ? 'L' : 'M') + (x * 240).toFixed(1) + ' ' + ((1 - L[Math.round(x * 255)]) * 240).toFixed(1); }).join('');
      body += `<div class="curve"><svg id="curve" viewBox="-8 -8 256 256"><rect width="240" height="240" rx="6" fill="#F7F1E3" stroke="rgba(13,12,11,.18)"/><path d="M60 0V240M120 0V240M180 0V240M0 60H240M0 120H240M0 180H240" stroke="rgba(13,12,11,.08)" fill="none"/><path d="M0 240L240 0" stroke="rgba(13,12,11,.2)" stroke-dasharray="3 4" fill="none"/><path id="cpath" d="${path}" stroke="#0D0C0B" stroke-width="2" fill="none"/>${adj.curve.map((y, i) => `<circle class="cpt" cx="${i * 60}" cy="${(1 - y) * 240}" r="6" fill="#F7F1E3" stroke="#0D0C0B" stroke-width="2"/>`).join('')}</svg><p>Drag the points to shape tone.</p><button class="sm" data-a="creset">Reset curve</button></div>`;
    }
    if (S.tab === 'crop') body += `<div class="lbl" style="padding-top:14px">ASPECT RATIO</div><div class="chips">${['orig', '1:1', '4:5', '3:2', '16:9'].map(a => `<button class="chip ${adj.aspect === a ? 'on' : ''}" aria-pressed="${adj.aspect === a}" data-a="aspect" data-v="${a}">${a === 'orig' ? 'Original' : a}</button>`).join('')}<button class="chip ${adj.flip ? 'on' : ''}" aria-pressed="${adj.flip}" data-a="flip">Flip ↔</button></div>`;
    if (S.tab === 'presets') body += `<div class="presets">${GH.PRESETS.map(([n], i) => `<button data-a="preset" data-v="${i}">${n}</button>`).join('')}</div>`;
    if (S.tab === 'history') body += `<div class="hl">${p.history.map((h, i) => `<button class="${i === p.history.length - 1 ? 'on' : ''}" data-a="revert" data-v="${i}"><span>${String(i + 1).padStart(2, '0')}</span>${esc(h.label)}</button>`).reverse().join('')}</div>`;
    return `<div class="dev"><div><div class="stage"><canvas id="cv"></canvas>
      <div class="fbar"><button class="n" data-a="prev" aria-label="Previous frame" title="Previous (←)">‹</button><div class="nm"><b>${esc(p.name)}</b><span>${pos} of ${sp.length}</span></div><button class="n" data-a="next" aria-label="Next frame" title="Next (→)">›</button><span class="sp"></span>
      <div style="display:flex">${[1, 2, 3, 4, 5].map(n => `<button class="st ${n <= p.rating ? 'on' : ''}" aria-pressed="${n <= p.rating}" data-a="rate" data-v="${n}" aria-label="Rate ${n}">${n <= p.rating ? '★' : '☆'}</button>`).join('')}</div><span class="sp"></span>
      <button class="ba ${S.before ? 'on' : ''}" aria-pressed="${S.before}" data-a="before" title="Hold \\ to compare">${S.before ? 'Before' : 'After'} <span class="kbd">\\</span></button></div></div>
      <div class="strip"><div class="sc">${sp.map(i => `<button class="${i === S.cur ? 'on' : ''}" data-a="frame" data-v="${i}" title="${esc(S.photos[i].name)}"><div class="th"><img src="${thumbs[S.photos[i].id] || BLANK}" alt=""></div></button>`).join('')}</div><span>${sp.length} frames · ${sp.filter(i => isEd(S.photos[i])).length} edited</span></div></div>
      <div class="panel" style="display:flex;flex-direction:column;min-height:0"><div class="hist"><canvas id="hv" width="640" height="160"></canvas></div>
      <div class="tabs">${tabs.map(([id, l]) => `<button class="${S.tab === id ? 'on' : ''}" aria-pressed="${S.tab === id}" data-a="tab" data-v="${id}">${l}</button>`).join('')}</div>
      <div class="pb">${body}</div>
      <div class="pf"><span id="sum">${nEd ? nEd + (nEd === 1 ? ' adjustment' : ' adjustments') : 'No edits yet'}</span><button class="sb o" data-a="resetall" ${nEd ? '' : 'disabled'}>Reset all</button></div></div></div>`;
  }

  function exportDlg() {
    const p = cur(), pill = (a, v, l, on) => `<button class="chip ${on ? 'on' : ''}" aria-pressed="${on}" data-a="${a}" data-v="${v}">${l}</button>`;
    return `<div class="scrim" data-a="closeexp"><div class="dlg" data-stop role="dialog" aria-modal="true" aria-label="Export ${esc(p.name)}"><div><div class="lbl" style="margin-bottom:6px">EXPORT</div><h2>${esc(p.name)}</h2></div>
      <div><div class="f">Format</div><div class="row">${pill('fmt', 'jpeg', 'JPEG', S.fmt === 'jpeg')}${pill('fmt', 'png', 'PNG', S.fmt === 'png')}</div></div>
      <div><div class="f">Long edge</div><div class="row">${[[1080, '1080 PX'], [2048, '2048 PX'], [0, 'FULL RES']].map(([v, l]) => pill('size', v, l, S.size === v)).join('')}</div></div>
      ${S.fmt === 'jpeg' ? `<div><div style="display:flex;justify-content:space-between;margin-bottom:6px"><span class="f" style="margin:0">Quality</span><b id="qv">${S.q}</b></div><input class="gh-range" data-f="q" data-k="q" type="range" min="50" max="100" value="${S.q}" style="background:${trk((S.q - 50) / 50, 0)}" aria-label="Quality"></div>` : ''}
      <div class="fn">groovehaus_${esc(p.name.toLowerCase())}.${S.fmt === 'png' ? 'png' : 'jpg'}</div>
      <div class="ft"><button class="sb" style="color:var(--dim)" data-a="closeexp">Cancel</button><button class="sb p" data-f="doexport" data-a="doexport" ${S.busy ? 'disabled' : ''}>${S.busy ? 'Exporting…' : 'Export ↓'}</button></div></div></div>`;
  }

  /* ---------- render ---------- */
  function render() {
    const af = document.activeElement?.dataset?.f, scroll = app.querySelector('.pb')?.scrollTop;
    if (S.exporting && !cur()) S.exporting = false;
    app.innerHTML = `<div class="app">${header()}${S.view === 'develop' && cur() ? develop() : library()}${S.exporting ? exportDlg() : ''}</div>`;
    if (scroll) app.querySelector('.pb').scrollTop = scroll;
    if (af && af !== 'newname') app.querySelector(`[data-f="${af}"]`)?.focus();
    if (S.adding) { const n = app.querySelector('[data-f="newname"]'); if (n) { n.focus(); n.setSelectionRange(n.value.length, n.value.length); } }
    draw();
  }

  let raf = 0, tok = 0;
  const schedule = () => { if (!raf) raf = requestAnimationFrame(() => { raf = 0; draw(); }); };
  async function draw() {
    const cv = $('#cv'); if (S.view !== 'develop' || !cv || !cur()) return;
    const p = cur(), t = ++tok, img = await getImg(p);
    if (t !== tok || !$('#cv')) return;
    const hist = GH.render($('#cv'), img, S.before ? DEF() : p.adj, devicePixelRatio > 1 ? 2000 : 1400), h = $('#hv'); if (!h) return;
    const c = h.getContext('2d'), W = h.width, H = h.height, mx = hist.slice().sort((a, b) => a - b)[250] || 1;
    c.clearRect(0, 0, W, H); c.fillStyle = 'rgba(242,233,213,.75)';
    for (let i = 0; i < 256; i++) { const v = Math.min(1, hist[i] / mx) * (H - 2); c.fillRect(i * W / 256, H - v, Math.ceil(W / 256), v); }
  }

  /* ---------- import / export ---------- */
  async function addFiles(files) {
    let first = -1, n = 0;
    for (const f of Array.from(files)) {
      if (!/^image\//.test(f.type)) continue;
      const id = 'u' + Date.now() + n++; await putBlob(id, f);
      S.photos.push({ id, name: f.name.replace(/\.[^.]+$/, '').slice(0, 14).toUpperCase(), shoot: S.shoot, rating: 0, adj: DEF(), history: [{ label: 'IMPORTED', adj: DEF() }] });
      if (first < 0) first = S.photos.length - 1;
    }
    if (first < 0) return;
    S.view = 'library'; S.cur = first; save(); render(); S.photos.slice(first).forEach(getImg);
  }
  async function doExport() {
    if (S.busy) return; S.busy = true; render(); await new Promise(r => setTimeout(r, 30));
    const p = cur(), img = await getImg(p), c = document.createElement('canvas'), png = S.fmt === 'png';
    GH.render(c, img, p.adj, S.size === 0 ? Infinity : S.size);
    c.toBlob(b => { const a = document.createElement('a'); a.href = URL.createObjectURL(b); a.download = 'groovehaus_' + p.name.toLowerCase() + (png ? '.png' : '.jpg'); a.click(); S.busy = false; S.exporting = false; render(); }, png ? 'image/png' : 'image/jpeg', S.q / 100);
  }
  const createShoot = () => { const n = S.newName.trim().toUpperCase().slice(0, 28); if (!n) return; const id = 's' + Date.now(); S.shoots.push({ id, name: n }); S.shoot = id; S.adding = false; S.newName = ''; S.lf = 'all'; save(); render(); };

  /* ---------- events ---------- */
  const A = {
    view: v => go(v), lf: v => { S.lf = v; render(); }, tab: v => { S.tab = v; render(); },
    import: () => fileEl.click(), export: () => { if (cur()) { S.exporting = true; render(); const d = $('[data-f=doexport]'); if (d) d.focus(); } },
    closeexp: () => { S.exporting = false; render(); }, doexport: doExport,
    fmt: v => { S.fmt = v; render(); }, size: v => { S.size = +v; render(); },
    select: v => { S.cur = +v; document.querySelectorAll('.tile').forEach(t => t.classList.toggle('on', t.dataset.v === v)); const st = $('.status span'); if (st) st.firstChild.textContent = cur().name + ' · ' + inShoot().length + ' in shoot'; },
    pick: v => { upd(q => ({ ...q, rating: q.rating === 5 ? 0 : 5 }), +v); render(); },
    shoot: v => { const f = S.photos.findIndex(q => q.shoot === v); S.shoot = v; if (f >= 0) S.cur = f; S.lf = 'all'; render(); },
    newshoot: () => { S.adding = true; render(); }, create: createShoot, cancel: () => { S.adding = false; S.newName = ''; render(); },
    prev: () => nav(-1), next: () => nav(1), frame: v => { S.cur = +v; render(); },
    rate: v => { upd(q => ({ ...q, rating: q.rating === +v ? 0 : +v })); render(); },
    before: () => { S.before = !S.before; render(); },
    reset: k => { setAdj(k, DEF()[k]); commit(SL[k][0] + ' reset'); render(); },
    greset: t => { const keys = GROUPS[S.tab].find(g => g[0] === t)[1]; upd(q => ({ ...q, adj: { ...q.adj, ...Object.fromEntries(keys.map(k => [k, DEF()[k]])) } })); commit(t[0] + t.slice(1).toLowerCase() + ' reset'); render(); },
    resetall: () => { upd(q => ({ ...q, adj: DEF() })); commit('Reset all'); render(); },
    aspect: v => { setAdj('aspect', v); commit('Crop ' + (v === 'orig' ? 'original' : v)); render(); },
    flip: () => { setAdj('flip', !cur().adj.flip); commit('Flip'); render(); },
    preset: i => { const [name, o] = GH.PRESETS[+i]; upd(p => { const adj = { ...DEF(), ...o, angle: p.adj.angle, aspect: p.adj.aspect, flip: p.adj.flip }; return { ...p, adj, history: [...p.history.slice(-40), { label: name, adj }] }; }); render(); },
    revert: i => { const h = cur().history[+i]; upd(q => ({ ...q, adj: h.adj })); render(); },
    creset: () => { setAdj('curve', DEF().curve); commit('Curve reset'); render(); }
  };
  app.addEventListener('click', e => {
    if (e.target.closest('[data-stop]') && !e.target.closest('button')) return;
    const el = e.target.closest('[data-a]'); if (!el) return;
    e.stopPropagation(); A[el.dataset.a]?.(el.dataset.v);
  });
  app.addEventListener('dblclick', e => { const t = e.target.closest('[data-dbl]'); if (t) go('develop', +t.dataset.dbl); const r = e.target.closest('input[data-k]'); if (r && SL[r.dataset.k]) A.reset(r.dataset.k); });
  app.addEventListener('input', e => {
    const t = e.target, k = t.dataset.k; if (!k) return;
    if (SL[k]) { // live slider: update readout, track and preview without re-rendering
      setAdj(k, +t.value); t.style.background = track(k);
      const v = $('#v-' + k); v.textContent = disp(k); v.className = cur().adj[k] !== DEF()[k] ? 'ch' : ''; schedule();
    } else if (k === 'tsz') { S.tsz = +t.value; t.style.background = trk((S.tsz - 120) / 240, 0); $('#grid').style.gridTemplateColumns = `repeat(auto-fill,minmax(${S.tsz}px,1fr))`; }
    else if (k === 'q') { S.q = +t.value; t.style.background = trk((S.q - 50) / 50, 0); $('#qv').textContent = S.q; }
    else return;
  });
  app.addEventListener('change', e => { const k = e.target.dataset.k; if (SL[k]) { commit(SL[k][0]); render(); } });
  app.addEventListener('keydown', e => {
    if (e.target.dataset.f !== 'newname') return;
    e.stopPropagation(); S.newName = e.target.value;
    if (e.key === 'Enter') createShoot(); else if (e.key === 'Escape') A.cancel();
  });
  app.addEventListener('keyup', e => { if (e.target.dataset.f === 'newname') S.newName = e.target.value; });
  app.addEventListener('dragover', e => { if (e.target.closest('[data-drop]')) e.preventDefault(); });
  app.addEventListener('drop', e => { if (e.target.closest('[data-drop]')) { e.preventDefault(); addFiles(e.dataTransfer.files); } });
  fileEl.addEventListener('change', e => { addFiles(e.target.files); e.target.value = ''; });

  // tone curve drag
  const cpt = e => { const r = $('#curve').getBoundingClientRect(), x = (e.clientX - r.left - r.width * 8 / 256) / (r.width * 240 / 256), y = 1 - (e.clientY - r.top - r.height * 8 / 256) / (r.height * 240 / 256); return [Math.max(0, Math.min(4, Math.round(x * 4))), Math.max(0, Math.min(1, y))]; };
  const curveSet = e => { const [i, y] = cpt(e), cv = cur().adj.curve.slice(); cv[i] = Math.round(y * 100) / 100; setAdj('curve', cv); const L = GH.lut(cv); $('#cpath').setAttribute('d', Array.from({ length: 41 }, (_, j) => { const x = j / 40; return (j ? 'L' : 'M') + (x * 240).toFixed(1) + ' ' + ((1 - L[Math.round(x * 255)]) * 240).toFixed(1); }).join('')); $('#curve').querySelectorAll('.cpt')[i].setAttribute('cy', (1 - cv[i]) * 240); schedule(); };
  app.addEventListener('pointerdown', e => { if (e.target.closest('#curve')) { drag = true; $('#curve').setPointerCapture(e.pointerId); curveSet(e); } });
  app.addEventListener('pointermove', e => { if (drag && $('#curve')) curveSet(e); });
  app.addEventListener('pointerup', () => { if (drag) { drag = false; commit('Tone curve'); render(); } });

  // keyboard
  const onKey = (e, down) => {
    const t = e.target, tag = t?.tagName;
    if (down && e.key === 'Enter' && t?.dataset?.dbl) { e.preventDefault(); go('develop', +t.dataset.dbl); return; }
    if (down && e.key === ' ' && t?.dataset?.dbl) { e.preventDefault(); A.select(t.dataset.v); return; }
    if ((e.metaKey || e.ctrlKey) && down && (e.key === 'e' || e.key === 'i')) { e.preventDefault(); e.key === 'e' ? A.export() : fileEl.click(); return; }
    if (e.metaKey || e.ctrlKey || e.altKey) return;
    if (e.key === '\\') { if (S.view === 'develop') { S.before = down; render(); } return; }
    if (!down || (tag === 'INPUT' && t.type !== 'range')) return;
    if (e.key === 'ArrowRight' && tag !== 'INPUT') nav(1);
    else if (e.key === 'ArrowLeft' && tag !== 'INPUT') nav(-1);
    else if (e.key === 'Enter' && S.view === 'library' && tag !== 'BUTTON') go('develop');
    else if (/^[0-5]$/.test(e.key) && cur()) { upd(q => ({ ...q, rating: +e.key === q.rating ? 0 : +e.key })); render(); }
    else if (e.key === 'g') go('library'); else if (e.key === 'd') go('develop'); else if (e.key === 'e') A.export();
    else if (e.key === 'Escape' && S.exporting) A.closeexp();
    else if (e.key === 'Escape' && sp < 320) { sp = 320; drawSplash(); }
  };
  document.addEventListener('keydown', e => onKey(e, true));
  document.addEventListener('keyup', e => onKey(e, false));

  /* ---------- startup card ---------- */
  const splash = document.createElement('div'); document.body.append(splash);
  let sp = 0, urls = [];
  async function loadSplash() {
    try {
      const r = await fetch('startup/startup.md', { cache: 'no-store' }); if (!r.ok) return;
      const names = (await r.text()).replace(/<!--[\s\S]*?-->/g, '').split('\n').map(l => l.trim()).filter(l => l && l[0] !== '#')
        .map(l => (l.match(/\(([^)]+)\)/) || [0, l.replace(/^[-*]\s+/, '')])[1].trim()).filter(n => /\.(jpe?g|png|webp|gif|avif)$/i.test(n));
      const ok = await Promise.all(names.map(n => new Promise(res => { const u = 'startup/' + encodeURI(n), im = new Image(); im.onload = () => res(u); im.onerror = () => res(null); im.src = u; })));
      urls = ok.filter(Boolean);
    } catch (e) {}
  }
  const msg = () => sp < 60 ? 'opening your shoots…' : sp < 130 ? 'reading frames…' : sp < 200 ? 'developing previews…' : sp < 270 ? 'restoring your edits…' : sp < 300 ? 'almost there…' : 'ready';
  let sig = null;
  function drawSplash() {
    if (sp >= 320) { splash.remove(); return; }
    const pics = urls.length ? urls : S.photos.filter(q => thumbs[q.id]).slice(0, 5).map(q => thumbs[q.id]), idx = pics.length ? Math.floor(Math.max(0, sp - 3) / 60) % pics.length : -1;
    if (sig !== pics.join('|')) { // rebuild only when the photo set changes so crossfades keep running
      sig = pics.join('|');
      splash.innerHTML = `<div class="splash" data-skip role="status" aria-label="Loading Groovehaus Darkroom. Click or press Escape to skip."><div class="card"><div class="l"><img src="assets/logo-medallion-white.png" alt=""><div><h1>GROOVEHAUS</h1><em>darkroom</em></div><div><div class="bar"><div id="sbar"></div></div><div class="m"><span id="smsg"></span><span>Click to skip</span></div></div></div>
        <div class="r">${pics.map(s => `<div class="p"><img class="bg" src="${s}" alt=""><img class="fg" src="${s}" alt=""></div>`).join('')}</div></div></div>`;
    }
    splash.firstChild.style.opacity = sp >= 305 ? 0 : 1;
    $('#sbar').style.width = Math.min(100, sp / 3).toFixed(1) + '%'; $('#smsg').textContent = msg();
    splash.querySelectorAll('.p').forEach((el, i) => el.classList.toggle('on', i === idx));
  }
  splash.addEventListener('click', () => { sp = 320; drawSplash(); });
  const tick = setInterval(() => { if (++sp >= 320) clearInterval(tick); drawSplash(); }, 100);

  /* ---------- boot ---------- */
  render(); drawSplash(); loadSplash();
  S.photos.forEach(getImg);
})();
