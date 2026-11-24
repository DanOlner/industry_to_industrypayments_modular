// 33_yearly_positions.cjs
// Scoping (plan/paymentflowsviz_plan.md, section 17): could an industry's position on the page follow the period shown,
// one set of positions for each year, instead of being worked out once on all the months? Three questions.
//
// 1. Data. What would the page need that it doesn't already load? Positions come from three things: the level in the
//    supplier-to-buyer order, the measure a column is ranked on, and a pull between industries that pay each other. All
//    three can be worked out from the monthly payments the page already holds, if the level can be worked out in the
//    browser. So this works the levels out here in JavaScript and checks them against the build's (R), for all the
//    months and for each calendar year (the build ships levels on the 12 months to each month, for the side panel).
//
// 2. Time. How long does one year's positions take to work out, for each kind of view?
//
// 3. Signal. When positions change from one year to the next, how much of that is the year and how much is chance?
//    For each pair of neighbouring years, A then B, two comparisons:
//      real    positions on year A against positions on year B
//      mixed   positions on two made-up years, each with every calendar month once, half its months from A and half
//              from B (one takes January from A, February from B, March from A...; the other takes the rest)
//    If nothing about the year mattered, the two comparisons would give the same amount of movement. So real over mixed
//    says how much of the movement belongs to the year: about 1 is chance, well over 1 is something that changed.
//    Movement is in pixels on a chart 1,064 wide and 745 high (the page at a 1,440-pixel window), for industries that
//    are not "too small to place".
//    The force-driven views are worked out two ways. Cold: from scratch each year, as the page works out a view now.
//    Warm: starting from the all-months positions, so an industry moves only as far as the year pulls it.
//
// 4. Rolling. The same question for positions on the twelve months to each month, 82 windows, which would let circles
//    drift instead of jumping each January. How far does a circle step from one month to the next? Three ways of working
//    it out: each month on its own, starting from the all-months positions; each month starting from the month before
//    ("chained"); and working positions out only at year-ends and gliding between them, with the height still on the
//    twelve months to each month and a few steps to push overlapping circles apart. The page uses the last.
//
// Intermediaries are left out, as the page opens. Values include held values, as the page's positions do now: a hidden
// month is held at the link's average published month in the same year, or in the nearest years if that year has none.
// Where they are borrowed from other years they damp the change from one year to the next.
//
// Run from the repo root: node docs/chunks/paymentflowsviz_ynh/tests/33_yearly_positions.cjs [region | all]   (default TLE)
const fs = require('fs'), path = require('path');
const d3 = require(path.join(__dirname, '..', 'lib', 'd3.v7.9.0.min.js'));
const dir = path.join(__dirname, '..', 'data'), outDir = path.join(__dirname, 'outputs');
const REGIONS = ['TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN'];
const arg = process.argv[2] || 'TLE', todo = arg === 'all' ? REGIONS : [arg];
const W = 1064, H = 745, K = 1, pad = { l: 46, r: 132, t: 46, b: 22 };       // the canvas, as flows.js sizes it
const load = code => { global.window = {}; require(path.join(dir, `flows_${code}.js`)); return global.window.FLOWS_DATA[code]; };
const median = a => d3.median(a), q90 = a => d3.quantile(a.slice().sort(d3.ascending), 0.9);
const f1 = v => (v == null || !isFinite(v) ? '' : v.toFixed(1)), f2 = v => (v == null || !isFinite(v) ? '' : v.toFixed(2));
const rows = [];

function study(code, detail) {
  const D = load(code), NM = D.months.length, nodes = D.nodes.map((n, i) => ({ ...n, i })), NN = nodes.length, NL = D.links.supplier.length;
  const sup = D.links.supplier, buy = D.links.buyer, act = nodes.map(d => d.role === 'industry'), lact = sup.map((s, k) => act[s] && act[buy[k]]);
  const live = nodes.filter(d => act[d.i]), placed = live.filter(d => !d.small);
  const say = (...a) => { if (detail) console.log(...a); };

  // ---- a set of months: what each link carried, and each industry's own and outside payments
  const sumOver = (a, months) => { let t = 0; for (const m of months) t += a[m] || 0; return t; };
  const dataFor = months => { const tot = new Float64Array(NL), recv = new Float64Array(NN), paid = new Float64Array(NN); let held = 0, all = 0;
    for (let k = 0; k < NL; k++) { if (!lact[k]) continue; const v = D.links.value[k], b = D.links.basis[k]; let t = 0;
      for (const m of months) { t += v[m]; if (b[m] === 'h') held += v[m]; } tot[k] = t; all += t; recv[sup[k]] += t; paid[buy[k]] += t; }
    return { months, tot, recv, paid, held: held / all }; };

  // ---- levels, as tests/functions_hierarchy.R works them out: solve (diag(u) - W - W') h = v on the largest connected set,
  // then measure from the flow-weighted average industry
  function levels(ds) {
    const ids = live.map(d => d.i), n = ids.length, at = new Map(ids.map((i, j) => [i, j])), Wm = Array.from({ length: n }, () => new Float64Array(n));
    for (let k = 0; k < NL; k++) if (lact[k] && ds.tot[k] > 0) Wm[at.get(sup[k])][at.get(buy[k])] += ds.tot[k];       // W[supplier][buyer]
    const row = Wm.map(r => d3.sum(r)), col = new Float64Array(n); for (let i = 0; i < n; i++) for (let j = 0; j < n; j++) col[j] += Wm[i][j];
    const comp = new Int32Array(n); let nc = 0;
    for (let s = 0; s < n; s++) { if (comp[s] || row[s] + col[s] <= 0) continue; nc++; const stack = [s]; comp[s] = nc;
      while (stack.length) { const a = stack.pop(); for (let b = 0; b < n; b++) if (!comp[b] && (Wm[a][b] > 0 || Wm[b][a] > 0)) { comp[b] = nc; stack.push(b); } } }
    const size = new Map(); for (const c of comp) if (c) size.set(c, (size.get(c) || 0) + 1);
    const bigC = [...size].sort((a, b) => b[1] - a[1])[0][0], big = []; for (let i = 0; i < n; i++) if (comp[i] === bigC) big.push(i);
    const m = big.length, A = Array.from({ length: m - 1 }, () => new Float64Array(m));                                 // the first one is pinned at zero
    for (let a = 1; a < m; a++) { const i = big[a]; let u = 0, v = 0;
      for (const j of big) { u += Wm[i][j] + Wm[j][i]; v += Wm[j][i] - Wm[i][j]; }
      for (let b = 1; b < m; b++) { const j = big[b]; A[a - 1][b - 1] = (a === b ? u : 0) - Wm[i][j] - Wm[j][i]; } A[a - 1][m - 1] = v; }
    for (let c = 0; c < m - 1; c++) { let p = c; for (let r = c + 1; r < m - 1; r++) if (Math.abs(A[r][c]) > Math.abs(A[p][c])) p = r; [A[c], A[p]] = [A[p], A[c]];
      for (let r = c + 1; r < m - 1; r++) { const f = A[r][c] / A[c][c]; if (f) for (let j = c; j < m; j++) A[r][j] -= f * A[c][j]; } }
    const hb = new Float64Array(m); for (let r = m - 2; r >= 0; r--) { let s = A[r][m - 1]; for (let j = r + 1; j < m - 1; j++) s -= A[r][j] * hb[j + 1]; hb[r + 1] = s / A[r][r]; }
    let sw = 0, swh = 0; big.forEach((i, a) => { const w = row[i] + col[i]; sw += w; swh += w * hb[a]; });
    const h = new Float64Array(NN).fill(NaN); big.forEach((i, a) => { h[ids[i]] = hb[a] - swh / sw; }); return h;
  }

  // ---- the measures a column can be ranked on, as flows.js works them out, on a set of months
  function measures(ds) {
    const inside = new Float64Array(NN), passed = new Float64Array(NN), ties = new Float64Array(NN), two = new Float64Array(NN), netIn = new Float64Array(NN), netOut = new Float64Array(NN);
    const num = new Float64Array(NN), linkAt = new Map(); for (let k = 0; k < NL; k++) linkAt.set(sup[k] * 1000 + buy[k], k);
    for (let k = 0; k < NL; k++) { if (!lact[k]) continue; const s = sup[k], b = buy[k], t = ds.tot[k], r = linkAt.has(b * 1000 + s) ? linkAt.get(b * 1000 + s) : -1;
      num[s] += t * (nodes[b].group === 'Manufacturing' ? 1 : 0); num[b] += t * (nodes[s].group === 'Manufacturing' ? 1 : 0);
      if (r >= 0 && r < k) continue; const back = r >= 0 ? ds.tot[r] : 0, mn = Math.min(t, back), v = Math.abs(t - back); two[s] += 2 * mn; two[b] += 2 * mn;
      if (v) { netIn[t >= back ? s : b] += v; netOut[t >= back ? b : s] += v; } }
    for (const d of live) { const i = d.i, g = ds.paid[i] + ds.recv[i], own = sumOver(d.self, ds.months), out = sumOver(d.ext.sell_core, ds.months) + sumOver(d.ext.buy_core, ds.months);
      inside[i] = g + 2 * own + out > 0 ? (g + 2 * own) / (g + 2 * own + out) : 0; passed[i] = g ? 2 * Math.min(netIn[i], netOut[i]) / g : 0; ties[i] = g ? num[i] / g : 0; }
    return { inside, passed, ties };
  }
  const rankOf = v => { const r = new Float64Array(NN).fill(NaN); placed.slice().sort((a, b) => v[a.i] - v[b.i]).forEach((d, j) => { r[d.i] = j; }); return r; };

  // ---- the sets of months: all of them, each calendar year, the latest twelve, and the mixed years
  const whole = dataFor(d3.range(NM)); whole.level = levels(whole);
  const years = [...new Set(D.months.map(m => m.slice(0, 4)))].filter(y => D.months.filter(m => m.startsWith(y)).length === 12);
  const yearMonths = y => d3.range(NM).filter(m => D.months[m].startsWith(y));
  const fill = ds => { const h = levels(ds); let gaps = 0; for (const d of live) if (!isFinite(h[d.i])) { h[d.i] = whole.level[d.i]; if (!d.small) gaps++; } ds.level = h; ds.gaps = gaps; ds.m = measures(ds); return ds; };
  whole.m = measures(whole);
  const Y = years.map(y => fill(dataFor(yearMonths(y))));
  const mixes = years.slice(1).map((y, j) => { const a = yearMonths(years[j]), b = yearMonths(y);
    return [fill(dataFor(a.map((m, i) => (i % 2 ? b[i] : m)))), fill(dataFor(a.map((m, i) => (i % 2 ? m : b[i]))))]; });

  // ---- 1. data: do the levels worked out here match the build's?
  const dWhole = d3.max(placed, d => Math.abs(whole.level[d.i] - d.level)), dSmall = d3.max(live.filter(d => d.small), d => Math.abs(whole.level[d.i] - d.level)) || 0;
  const dYear = d3.max(years.map((y, j) => { const dec = d3.max(yearMonths(y)); return d3.max(placed, d => (d.level_rolling[dec] == null ? 0 : Math.abs(Y[j].level[d.i] - d.level_rolling[dec]))); }));
  const span = d3.extent(placed, d => d.level);
  say(`\n== ${D.name}: ${live.length} industries, ${placed.length} placed, ${years.length} whole calendar years (${years[0]} to ${years[years.length - 1]}), ${NM} months ==`);
  say('\n1. Data');
  say(`Levels worked out here against the build's, industries large enough to place: largest difference ${dWhole.toFixed(4)} on all months, ${dYear.toFixed(4)} on a calendar year (levels span ${f2(span[1] - span[0])}).`);
  say(`Industries too small to place: ${dSmall.toFixed(4)} on all months. The file rounds payments to GBP 1,000, which matters when an industry trades a few thousand pounds a month.`);
  say(`Held values as a share of the value in a year: ${Y.map(d => Math.round(100 * d.held) + '%').join(', ')}. Where a link has no published month in a year they come from the nearest years.`);
  say(`Placed industries with no level in a year (not joined to the rest that year; the all-months level stands in): ${Y.map(d => d.gaps).join(', ')}.`);
  const jsonSize = o => JSON.stringify(o).length;
  say(`Levels on the 12 months to each month, already in the file: ${Math.round(jsonSize(nodes.map(d => [d.level_rolling, d.level_all_rolling])) / 1024)} KB of ${Math.round(fs.statSync(path.join(dir, `flows_${code}.js`)).size / 1024)} KB. Nothing more would be needed.`);

  // ---- the page's layout (flows.js, computeLayout), on a set of months. start: positions to begin from (warm), or none (cold)
  const wholeTot = new Float64Array(NN); for (const d of live) wholeTot[d.i] = whole.recv[d.i] + whole.paid[d.i];
  const nodeRef = d3.max(wholeTot) / NM, rNode = tot => K * (3 + 30 * Math.sqrt(Math.max(0, tot) / nodeRef)), rad = nodes.map(d => rNode(wholeTot[d.i] / NM));   // sizes stay those of all months
  const lev = d3.extent(placed, d => whole.level[d.i]), x0 = pad.l + 24 * K, x1 = W - pad.r, y0 = pad.t + 22 * K, y1 = H - pad.b - 22 * K;
  const yOf = d3.scaleLinear().domain(lev).range([y1, y0]).clamp(true);
  const GROUPS = D.sector_groups, need = GROUPS.map(g => Math.sqrt(d3.sum(live.filter(d => d.group === g), d => 2 * rad[d.i] + 8 * K)) || 1), cx = {}, cw = {};
  { const total = d3.sum(need), a = pad.l + 8 * K, b = W - pad.r + 24 * K; let acc = 0; GROUPS.forEach((g, j) => { const w = (b - a) * need[j] / total; cx[g] = a + acc + w / 2; cw[g] = w; acc += w; }); }
  let layoutMs = [];
  function layout(mode, cols, ds, start, alpha0 = 0.5, steps = 300, fixedDom = null) {
    const t0 = process.hrtime.bigint(), banded = mode === 'order' && cols === 'group', rnd = d3.randomLcg(7);
    const base = start || (mode === 'order' ? null : layout('order', 'free', ds, null));
    const sn = live.map(d => ({ i: d.i, level: ds.level[d.i], col: d.group, r: rad[d.i], x: base ? base[d.i].x : banded ? cx[d.group] : x0 + rnd() * (x1 - x0), y: base ? base[d.i].y : yOf(ds.level[d.i]) }));
    const pairs = new Map(); let grand = 0; for (let k = 0; k < NL; k++) { if (!lact[k]) continue; const i = Math.min(sup[k], buy[k]), j = Math.max(sup[k], buy[k]); pairs.set(i * 1000 + j, (pairs.get(i * 1000 + j) || 0) + ds.tot[k]); grand += ds.tot[k]; }
    const sl = [], floor = 1000 * ds.months.length / NM;                    // the page's floor of GBP 1m is for all the months; scaled to the months in hand
    for (const [kk, t] of pairs) { const i = Math.floor(kk / 1000), j = kk % 1000, ti = ds.recv[i] + ds.paid[i], tj = ds.recv[j] + ds.paid[j];
      if (mode === 'order') { const s = t / Math.min(ti, tj); if (s > 0.03) sl.push({ source: i, target: j, s }); }
      else { const aff = t / ((ds.recv[i] * ds.paid[j] + ds.recv[j] * ds.paid[i]) / grand); if (t > floor && aff > 1.5) sl.push({ source: i, target: j, s: Math.min(1, (aff - 1) / 8) * 0.45 }); } }
    const sim = d3.forceSimulation(sn).randomSource(rnd)
      .force('link', d3.forceLink(sl).id(d => d.i).strength(l => Math.min(1, l.s * 2.2) * (mode === 'order' ? 0.25 : 0.7)).distance(l => (50 + 130 * (1 - Math.min(1, l.s * 2))) * K))
      .force('charge', d3.forceManyBody().strength((mode === 'order' ? (banded ? -60 : -420) : -260) * K))
      .force('collide', d3.forceCollide(d => d.r + (mode === 'order' ? 7 : 10) * K).iterations(3))
      .force('x', d3.forceX(d => (banded ? cx[d.col] : (x0 + x1) / 2)).strength(banded ? 0.8 : mode === 'force' ? 0.05 : 0.012))
      .force('y', d3.forceY(d => (mode === 'force' ? (y0 + y1) / 2 : yOf(d.level))).strength(mode === 'order' ? 1 : mode === 'hybrid' ? 0.35 : 0.07)).stop();
    const slack = 14 * K, pin = () => { for (const d of sn) {
      if (mode === 'order') { const ty = yOf(d.level); d.y = Math.max(ty - slack, Math.min(ty + slack, d.y)); }
      if (banded) { const c = cx[d.col], hw = Math.max(0, cw[d.col] / 2 - Math.min(d.r, cw[d.col] / 2) - 1); d.x = Math.max(c - hw, Math.min(c + hw, d.x)); }
      d.x = Math.max(x0, Math.min(x1 + (banded ? 24 * K : 0), d.x)); d.y = Math.max(y0, Math.min(y1, d.y)); } };
    if (start) sim.alpha(alpha0);                                          // warm: a gentler start, and half the steps (or as asked)
    for (let i = 0; i < (start ? steps : 600); i++) { sim.tick(); pin(); }
    const raw = nodes.map(() => null); for (const d of sn) raw[d.i] = { x: d.x, y: d.y };          // where the pulls left each circle, before the stretch to fill the frame
    let dom = fixedDom;
    if (!banded) { const xs = sn.map(d => d.x).sort(d3.ascending), ys = sn.map(d => d.y).sort(d3.ascending), p = mode === 'order' ? 0 : 0.04;
      if (!dom) dom = { x: [d3.quantile(xs, p), d3.quantile(xs, 1 - p)], y: [d3.quantile(ys, p), d3.quantile(ys, 1 - p)] };
      const fx = d3.scaleLinear().domain(dom.x).range([x0, x1]).clamp(true), fy = d3.scaleLinear().domain(dom.y).range([y0, y1]).clamp(true);
      for (const d of sn) { d.tx = fx(d.x); d.ty = mode === 'order' ? d.y : fy(d.y); d.x = d.tx; d.y = d.ty; }
      const relax = d3.forceSimulation(sn).force('collide', d3.forceCollide(d => d.r + 6 * K).iterations(2)).force('x', d3.forceX(d => d.tx).strength(0.25)).force('y', d3.forceY(d => d.ty).strength(0.25)).stop();
      for (let i = 0; i < 60; i++) { relax.tick(); pin(); } }
    const out = nodes.map(() => null); for (const d of sn) out[d.i] = { x: d.x, y: d.y }; out.raw = raw; out.dom = dom;
    if (!start || mode === 'order' || base === start) layoutMs.push([mode + '/' + cols + (start ? ' warm' : ' cold'), Number(process.hrtime.bigint() - t0) / 1e6]);
    return out;
  }

  // ---- 3. movement between two sets of months, for each kind of view
  const step = ((W - pad.r + 24 * K - 62 * K - 16 * K) - (pad.l + 30 * K)) / placed.length;             // pixels a rank, on a ranked axis
  const views = [
    ['Height: level in the order', (a, b) => placed.map(d => Math.abs(yOf(a.level[d.i]) - yOf(b.level[d.i])))],
    ['Sideways: rank by trade inside the region', (a, b) => { const ra = rankOf(a.m.inside), rb = rankOf(b.m.inside); return placed.map(d => step * Math.abs(ra[d.i] - rb[d.i])); }],
    ['Sideways: rank by share passed along', (a, b) => { const ra = rankOf(a.m.passed), rb = rankOf(b.m.passed); return placed.map(d => step * Math.abs(ra[d.i] - rb[d.i])); }],
    ['Sideways: rank by ties to manufacturing', (a, b) => { const ra = rankOf(a.m.ties), rb = rankOf(b.m.ties); return placed.map(d => step * Math.abs(ra[d.i] - rb[d.i])); }]];
  const wholeLay = {};
  for (const [mode, cols, label] of [['order', 'group', 'Supplier order, sector columns'], ['order', 'free', 'Supplier order, no columns'], ['hybrid', '-', 'Hybrid'], ['force', '-', 'Connections']]) {
    wholeLay[mode + cols] = layout(mode, cols, whole, null);
    for (const warm of [false, true]) { const key = mode + cols + warm;
      const lay = ds => ds[key] || (ds[key] = layout(mode, cols, ds, warm ? wholeLay[mode + cols] : null));
      views.push([`${label}, ${warm ? 'warm' : 'cold'}`, (a, b) => { const A = lay(a), B = lay(b); return placed.map(d => Math.hypot(A[d.i].x - B[d.i].x, A[d.i].y - B[d.i].y)); }]); } }
  const table = [];
  for (const [name, moved] of views) {
    const real = [], mixed = [], real90 = [], mixed90 = [], far = [], realMean = [], mixedMean = [];
    for (let j = 1; j < Y.length; j++) { const r = moved(Y[j - 1], Y[j]), x = moved(mixes[j - 1][0], mixes[j - 1][1]);
      real.push(median(r)); mixed.push(median(x)); real90.push(q90(r)); mixed90.push(q90(x)); far.push(r.filter(v => v > 40).length / r.length); realMean.push(d3.mean(r)); mixedMean.push(d3.mean(x)); }
    // ranks move in whole places, so their medians are coarse: the mean is given too, and its ratio is the one to quote
    const row = { region: code, view: name, real_mean_px: median(realMean), mixed_mean_px: median(mixedMean), ratio_mean: median(realMean) / median(mixedMean),
                  real_median_px: median(real), mixed_median_px: median(mixed), ratio: median(real) / median(mixed), real_p90_px: median(real90), mixed_p90_px: median(mixed90),
                  ratio_p90: median(real90) / median(mixed90), share_moving_over_40px: median(far) };
    table.push(row); rows.push(row);
  }
  say('\n3. Signal: movement from one year to the next (real) against two made-up years mixing the same months (mixed). Pixels; the middle pair of years.');
  if (detail) console.table(table.map(r => ({ view: r.view, 'real, average': f1(r.real_mean_px), 'mixed, average': f1(r.mixed_mean_px), 'real over mixed': f2(r.ratio_mean), 'real, typical industry': f1(r.real_median_px), 'mixed, typical': f1(r.mixed_median_px), 'real, one in ten moves over': f1(r.real_p90_px),
    'mixed, one in ten': f1(r.mixed_p90_px), 'moving over 40px': Math.round(100 * r.share_moving_over_40px) + '%' })));

  // Levels: does an industry travel, or wobble? Its net change from the first year to the last, over its average step
  // from one year to the next. Six steps that all go the same way give 6; a random walk about 2.4; a wobble about a fixed
  // place a little over 1. And how many end the period more than a tenth of the chart's height from where they began.
  const travel = placed.map(d => { const st = d3.mean(d3.range(1, Y.length), j => Math.abs(Y[j].level[d.i] - Y[j - 1].level[d.i])), net = Math.abs(Y[Y.length - 1].level[d.i] - Y[0].level[d.i]);
    return { d, st, net, r: st > 0 ? net / st : 0 }; });
  const tenth = 0.1 * (lev[1] - lev[0]), travelled = travel.filter(t => t.net > tenth);
  const tr = { region: code, steps: Y.length - 1, net_over_step_median: median(travel.map(t => t.r)), ended_over_a_tenth_away: travelled.length, placed: placed.length,
               largest: travel.slice().sort((a, b) => b.net - a.net).slice(0, 5).map(t => t.d.short).join('; ') };
  say(`\nLevels, travel or wobble: net change ${years[0]} to ${years[years.length - 1]} over the average yearly step, typical industry ${f2(tr.net_over_step_median)} (${tr.steps} steps: all one way gives ${tr.steps}, a random walk about ${f1(Math.sqrt(tr.steps))}, a wobble about 1).`);
  say(`Industries ending more than a tenth of the chart's height from where they began: ${travelled.length} of ${placed.length}.`);

  // which industries' levels move most between years, and is it the same ones in the mixed years? (for the write-up)
  if (detail) { const mv = placed.map(d => ({ d, real: d3.mean(d3.range(1, Y.length), j => Math.abs(Y[j].level[d.i] - Y[j - 1].level[d.i])), mixed: d3.mean(mixes, mx => Math.abs(mx[0].level[d.i] - mx[1].level[d.i])),
      perMonth: wholeTot[d.i] / NM / 1000 })).sort((a, b) => b.real - a.real);
    console.log('\nLevels: the ten industries that move most from year to year (average change in level; levels span ' + f2(lev[1] - lev[0]) + ')');
    console.table(mv.slice(0, 10).map(r => ({ industry: r.d.short, 'year to year': f2(r.real), 'mixed years': f2(r.mixed), 'real over mixed': f2(r.real / r.mixed), 'GBP m a month': f1(r.perMonth) })));
    const big = mv.filter(r => r.real / r.mixed >= 2 && r.real >= 0.05).sort((a, b) => b.real / b.mixed - a.real / a.mixed);
    console.log(`Industries whose level moves at least twice as much between real years as between mixed ones, and by 0.05 or more: ${big.length} of ${placed.length}` + (big.length ? ': ' + big.slice(0, 12).map(r => `${r.d.short} (${f2(r.real)} against ${f2(r.mixed)})`).join(', ') : ''));
    const drift = placed.map(d => ({ d, v: Y[Y.length - 1].level[d.i] - Y[0].level[d.i] })).sort((a, b) => Math.abs(b.v) - Math.abs(a.v));
    console.log(`Largest changes in level from ${years[0]} to ${years[years.length - 1]}: ` + drift.slice(0, 8).map(r => `${r.d.short} ${r.v > 0 ? '+' : ''}${f2(r.v)}`).join(', ')); }

  // ---- 4. rolling: positions on the twelve months to each month. How far does a circle move from one month to the next?
  // Neighbouring windows share eleven of their twelve months, so steps should be small. A step of 20 pixels or more would
  // show as a jump when played.
  let roll = null;
  if (detail || process.argv.includes('--rolling')) {
    const R = d3.range(11, NM).map(m => fill(dataFor(d3.range(m - 11, m + 1))));
    roll = views.filter(v => !/cold/.test(v[0])).map(([name, moved]) => { const med = [], p90 = [], all = [];
      for (let j = 1; j < R.length; j++) { const r = moved(R[j - 1], R[j]); med.push(median(r)); p90.push(q90(r)); for (const v of r) all.push(v); }
      // the same, after averaging each month's positions with the month before and after (possible only for the layouts, so left blank for the rest)
      return { region: code, view: name.replace(', warm', ''), typical_step_px: median(med), one_in_ten_px: median(p90), steps_over_20px: all.filter(v => v >= 20).length / all.length, largest_px: d3.max(all),
               year_on_year_px: d3.mean(d3.range(12, R.length), j => median(moved(R[j - 12], R[j]))) }; });
    say(`\n4. Rolling: the twelve months to each month, ${R.length} windows. Movement of a circle from one month to the next, pixels.`);
    say('Each month worked out on its own, starting from the all-months positions:');
    const show = rr => console.table(rr.map(r => ({ view: r.view, 'typical step': f1(r.typical_step_px), 'one in ten steps over': f1(r.one_in_ten_px), 'steps of 20px or more': (100 * r.steps_over_20px).toFixed(1) + '%', 'largest step': f1(r.largest_px),
      'typical move over 12 months': f1(r.year_on_year_px), ...(r.ms ? { 'ms a month': f1(r.ms), 'from the stand-alone positions': f1(r.apart_px), 'over 12 months, real over chance': f2(r.signal) } : {}) })));
    if (detail) show(roll);
    // Chained: each month starts from the month before (the first from the all-months positions), with a gentle start and few
    // steps. A circle then moves only as far as one more month's payments pull it. Is that smooth, how far does it stray from
    // the stand-alone positions, and does it still tell real years from mixed ones?
    for (const [alpha0, steps] of [[0.3, 100], [0.15, 60]]) {
      const ch = [['order', 'group', 'Supplier order, sector columns'], ['order', 'free', 'Supplier order, no columns'], ['hybrid', '-', 'Hybrid'], ['force', '-', 'Connections']].map(([mode, cols, label]) => {
        const t0 = process.hrtime.bigint(), L = []; let prev = wholeLay[mode + cols]; for (const ds of R) { prev = layout(mode, cols, ds, prev, alpha0, steps); L.push(prev); }
        const ms = Number(process.hrtime.bigint() - t0) / 1e6 / R.length, dist = (A, B) => placed.map(d => Math.hypot(A[d.i].x - B[d.i].x, A[d.i].y - B[d.i].y));
        const med = [], p90 = [], all = []; for (let j = 1; j < L.length; j++) { const r = dist(L[j - 1], L[j]); med.push(median(r)); p90.push(q90(r)); for (const v of r) all.push(v); }
        const apart = d3.mean(R, (ds, j) => median(dist(L[j], ds[mode + cols + true])));
        // chance: the same chain run on the months in a shuffled order within each year would be the fair test, but that changes the windows; so compare
        // movement over twelve months (no shared months) with twelve single steps laid end to end: a drift adds up, a wobble doesn't
        const year = d3.mean(d3.range(12, L.length), j => median(dist(L[j - 12], L[j]))), path = d3.mean(d3.range(12, L.length), j => d3.sum(d3.range(j - 11, j + 1), i => med[i - 1]));
        return { region: code, view: label, typical_step_px: median(med), one_in_ten_px: median(p90), steps_over_20px: all.filter(v => v >= 20).length / all.length, largest_px: d3.max(all), year_on_year_px: year, ms, apart_px: apart, signal: year / path }; });
      // Gliding: work the positions out only for each year-end (each from the all-months positions, as the yearly switch
      // does), and for the months between slide sideways from one year-end's place to the next. Height in the supplier
      // order still follows the level on the twelve months to that month. Then a few steps that only push apart circles
      // that have come to overlap. How smooth is that, and how many circles overlap without the push?
      if (alpha0 === 0.3) {
        const ends = [...d3.range(11, NM - 1, 12), NM - 1].filter((m, i, a) => a.indexOf(m) === i);          // each December, and the last month
        const gl = [['order', 'group', 'Supplier order, sector columns'], ['order', 'free', 'Supplier order, no columns'], ['hybrid', '-', 'Hybrid'], ['force', '-', 'Connections']].map(([mode, cols, label]) => {
          const banded = cols === 'group', A = {}, anchor = m => A[m] || (A[m] = layout(mode, cols, R[m - 11], wholeLay[mode + cols]));
          const t0 = process.hrtime.bigint(), L = [], lapsBefore = [], lapsAfter = [];
          const laps = P => { let n = 0; for (let a = 0; a < live.length; a++) for (let b = a + 1; b < live.length; b++) { const i = live[a].i, j = live[b].i; if (Math.hypot(P[i].x - P[j].x, P[i].y - P[j].y) < rad[i] + rad[j] - 1) n++; } return n; };
          for (let m = 11; m < NM; m++) { const e1 = ends.find(e => e >= m), e0 = m === e1 ? e1 : (ends[ends.indexOf(e1) - 1] ?? e1), f = e1 === e0 ? 0 : (m - e0) / (e1 - e0), A0 = anchor(e0), A1 = anchor(e1), ds = R[m - 11];
            const sn = live.map(d => { const i = d.i, x = A0[i].x + (A1[i].x - A0[i].x) * f;
              const y = mode === 'order' ? yOf(ds.level[i]) + (A0[i].y - yOf(R[e0 - 11].level[i])) * (1 - f) + (A1[i].y - yOf(R[e1 - 11].level[i])) * f : A0[i].y + (A1[i].y - A0[i].y) * f;
              return { i, level: ds.level[i], col: d.group, r: rad[i], x, y, tx: x, ty: y }; });
            const before = nodes.map(() => null); for (const d of sn) before[d.i] = { x: d.x, y: d.y }; lapsBefore.push(laps(before));
            const relax = d3.forceSimulation(sn).force('collide', d3.forceCollide(d => d.r + 3 * K).iterations(2)).force('x', d3.forceX(d => d.tx).strength(0.2)).force('y', d3.forceY(d => d.ty).strength(0.2)).stop();
            for (let i = 0; i < 40; i++) { relax.tick(); for (const d of sn) {
              if (mode === 'order') { const ty = yOf(d.level); d.y = Math.max(ty - 14 * K, Math.min(ty + 14 * K, d.y)); }
              if (banded) { const c = cx[d.col], hw = Math.max(0, cw[d.col] / 2 - Math.min(d.r, cw[d.col] / 2) - 1); d.x = Math.max(c - hw, Math.min(c + hw, d.x)); }
              d.x = Math.max(x0, Math.min(x1 + (banded ? 24 * K : 0), d.x)); d.y = Math.max(y0, Math.min(y1, d.y)); } }
            const out = nodes.map(() => null); for (const d of sn) out[d.i] = { x: d.x, y: d.y }; L.push(out); lapsAfter.push(laps(out)); }
          const ms = Number(process.hrtime.bigint() - t0) / 1e6 / R.length, dist = (A, B) => placed.map(d => Math.hypot(A[d.i].x - B[d.i].x, A[d.i].y - B[d.i].y));
          const med = [], p90 = [], all = []; for (let j = 1; j < L.length; j++) { const r = dist(L[j - 1], L[j]); med.push(median(r)); p90.push(q90(r)); for (const v of r) all.push(v); }
          return { view: label, 'typical step': f1(median(med)), 'one in ten steps over': f1(median(p90)), 'steps of 20px or more': (100 * all.filter(v => v >= 20).length / all.length).toFixed(1) + '%', 'largest step': f1(d3.max(all)),
                   'typical move over 12 months': f1(d3.mean(d3.range(12, L.length), j => median(dist(L[j - 12], L[j])))), 'ms a month, year-ends included': f1(ms),
                   'overlapping pairs a month, before the push': f1(d3.mean(lapsBefore)), after: f1(d3.mean(lapsAfter)), 'most in a month, after': d3.max(lapsAfter) }; });
        say('Gliding between year-end positions, height on the twelve months to each month, then a push apart:'); if (detail) console.table(gl); }
      if (alpha0 === 0.3) for (const [a2, n2] of [[0.3, 100], [0.1, 60]]) {
        const ch2 = [['order', 'group', 'Supplier order, sector columns'], ['order', 'free', 'Supplier order, no columns'], ['hybrid', '-', 'Hybrid'], ['force', '-', 'Connections']].map(([mode, cols, label]) => {
          const t0 = process.hrtime.bigint(), L = [], W0 = wholeLay[mode + cols]; let prev = W0; for (const ds of R) { prev = layout(mode, cols, ds, prev.raw, a2, n2, W0.dom); L.push(prev); }
          const ms = Number(process.hrtime.bigint() - t0) / 1e6 / R.length, dist = (A, B) => placed.map(d => Math.hypot(A[d.i].x - B[d.i].x, A[d.i].y - B[d.i].y));
          const med = [], p90 = [], all = []; for (let j = 1; j < L.length; j++) { const r = dist(L[j - 1], L[j]); med.push(median(r)); p90.push(q90(r)); for (const v of r) all.push(v); }
          const year = d3.mean(d3.range(12, L.length), j => median(dist(L[j - 12], L[j]))), path = d3.mean(d3.range(12, L.length), j => d3.sum(d3.range(j - 11, j + 1), i => med[i - 1]));
          return { region: code, view: label, typical_step_px: median(med), one_in_ten_px: median(p90), steps_over_20px: all.filter(v => v >= 20).length / all.length, largest_px: d3.max(all), year_on_year_px: year, ms,
                   apart_px: d3.mean(R, (ds, j) => median(dist(L[j], ds[mode + cols + true]))), signal: year / path, end: median(dist(L[L.length - 1], W0)) }; });
        say(`Chained on the unstretched positions with the all-months stretch kept, starting strength ${a2}, ${n2} steps a month:`); if (detail) show(ch2);
        say('  typical distance of the last month from the all-months positions: ' + ch2.map(r => `${r.view} ${f1(r.end)}`).join(', ')); }
      say(`Chained, starting strength ${alpha0}, ${steps} steps a month (last column: how far it gets in twelve months over the length of the twelve steps; 1 is a straight line, near 0 a wobble on the spot):`); if (detail) show(ch);
    }
  }

  // ---- 2. time
  if (detail) { const by = d3.rollups(layoutMs, v => d3.median(v, d => d[1]), d => d[0]); console.log('\n2. Time to work one view out, in Node on this machine (milliseconds, median):');
    console.table(by.map(([view, ms]) => ({ view, ms: Math.round(ms) }))); const t0 = process.hrtime.bigint(); for (let i = 0; i < 20; i++) levels(Y[0]); console.log(`Levels for one year: ${(Number(process.hrtime.bigint() - t0) / 2e7).toFixed(1)} ms.`); }
  return { code, name: D.name, dWhole, dYear, dSmall, table, tr, roll, ms: d3.rollups(layoutMs, v => d3.median(v, d => d[1]), d => d[0]) };
}

const done = todo.map(code => study(code, todo.length === 1 || code === 'TLE'));
if (todo.length > 1) {
  console.log('\n== All regions: real over mixed (movement of the average industry), and the real movement in pixels ==');
  const names = [...new Set(rows.map(r => r.view))];
  console.table(names.map(v => { const r = rows.filter(x => x.view === v); return { view: v, 'real over mixed, middle region': f2(median(r.map(x => x.ratio_mean))), lowest: f2(d3.min(r, x => x.ratio_mean)), highest: f2(d3.max(r, x => x.ratio_mean)),
    'real px (average industry), middle region': f1(median(r.map(x => x.real_mean_px))), 'mixed px': f1(median(r.map(x => x.mixed_mean_px))), 'real px, typical industry': f1(median(r.map(x => x.real_median_px))), 'moving over 40px': Math.round(100 * median(r.map(x => x.share_moving_over_40px))) + '%' }; }));
  console.log('Levels, travel or wobble, by region:'); console.table(done.map(d => ({ region: d.name, 'net over yearly step': f2(d.tr.net_over_step_median), 'ended over a tenth of the height away': `${d.tr.ended_over_a_tenth_away} of ${d.tr.placed}`, 'moved furthest': d.tr.largest })));
  console.log('Time to work one view out, slowest region (ms): ' + done[0].ms.map(([v]) => `${v} ${Math.round(d3.max(done, d => d.ms.find(x => x[0] === v)[1]))}`).join(', '));
  console.log('Levels worked out here against the build\'s, largest difference in any region, industries large enough to place: ' + d3.max(done, d => d.dWhole).toFixed(4) + ' (all months), ' + d3.max(done, d => d.dYear).toFixed(4) +
    ' (a calendar year). Industries too small to place: ' + d3.max(done, d => d.dSmall).toFixed(4) + ' (all months).');
}
const sig = v => (typeof v === 'number' ? +v.toPrecision(4) : v), cols = Object.keys(rows[0]);
if (todo.length > 1) { fs.writeFileSync(path.join(outDir, 'yearly_positions.csv'), [cols.join(','), ...rows.map(r => cols.map(c => (typeof r[c] === 'string' && r[c].includes(',') ? `"${r[c]}"` : sig(r[c]))).join(','))].join('\n') + '\n');
  console.log('\nWrote yearly_positions.csv'); }
