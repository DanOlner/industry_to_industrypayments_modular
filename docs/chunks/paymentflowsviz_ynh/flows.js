/* Regional payment flows: the page's drawing and controls.
   Data, built by build_data.R: data/regions.js (window.FLOWS_REGIONS, a small index of all twelve ITL1 regions, loaded with
   the page) and data/flows_<region>.js (window.FLOWS_DATA[region], fetched when that region is chosen).
   Values are GBP thousands; counts are payments.
   Drawing is on a 2D canvas, one fill per link: tests/outputs/drawing_timing.csv shows that is the fast way.
   Money runs from the paying industry (the buyer) to the industry paid (the supplier). */
(() => {
  'use strict';
  const d3 = window.d3, IDX = window.FLOWS_REGIONS;
  const $ = id => document.getElementById(id);
  if (!IDX || !d3) { document.body.insertAdjacentText('afterbegin', 'The data or drawing library did not load.'); return; }

  // The region's data in use. Everything here is set by setData(), which runs again whenever the region changes.
  let D = null, NM = 0, NL = 0, NN = 0, nodes = [], GROUPS = [], PAYGROUPS = [], monthDate = [];
  const NOGROUP = 'Not in a group';
  const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  const SHORT_HEAD = { 'Primary and utilities': 'Primary', 'Manufacturing': 'Manuf.', 'Construction': 'Constr.', 'Trade and transport': 'Trade', 'Business services': 'Business',
                       'Public services': 'Public', 'Hospitality, arts and other': 'Other', 'Distribution': 'Distrib.', 'Engineering and metals': 'Engin.', 'Local and public services': 'Local', 'Not in a group': 'None' };
  const DOT_UNIT = 100;                                  // one dot per 100 payments: the unit ONS rounds counts to
  const fmtMonth = d3.timeFormat('%B %Y'), fmtMonthShort = d3.timeFormat('%b %Y');

  // ---------------------------------------------------------------- formatting
  const fmtMoney = k => { const a = Math.abs(k);          // k is GBP thousands
    if (a >= 1e6) return '£' + (k / 1e6).toFixed(a >= 1e7 ? 1 : 2) + 'bn';
    if (a >= 1e3) return '£' + (k / 1e3).toFixed(a >= 1e5 ? 0 : 1) + 'm';
    return a >= 0.5 ? '£' + Math.round(k) + 'k' : '£0'; };
  const fmtCount = n => n < 50 ? 'under 50' : 'about ' + d3.format(',')(Math.round(n / 100) * 100);
  const fmtPct = f => (f * 100).toFixed(f >= 0.1 ? 0 : f >= 0.01 ? 1 : 2) + '%';
  const fmtAvg = (k, n) => n >= 50 ? '£' + d3.format(',.2r')(k * 1000 / n) : 'not known';
  const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };

  // ---------------------------------------------------------------- state, readable from the address bar
  const q = new URLSearchParams(location.search);
  const pick = (v, allowed, dflt) => allowed.includes(v) ? v : dflt;
  const monthFromParam = v => { const i = v == null ? -1 : D.months.indexOf(v); return i >= 0 ? i : /^\d+$/.test(v || '') && +v < NM ? +v : NM - 1; };   // anything unrecognised: the latest month
  const regionOf = code => IDX.regions.find(r => r.code === code);
  let wantMonth = q.get('m'), wantSel = q.get('sel');     // resolved once the region's data has arrived
  const DEFAULTS = { region: IDX.default, layout: 'order', cols: 'group', lens: 'none', show: 'money', dir: 'both', measure: 'value', scale: 'share', net: 'net', cover: '0.8',
                     smooth: true, dots: true, inter: false, halo: false, pubonly: false, trace: false, speed: '3' };
  const S = { region: (regionOf(q.get('region')) || {}).built ? q.get('region') : IDX.default, layout: pick(q.get('layout'), ['order', 'hybrid', 'force'], DEFAULTS.layout), cols: pick(q.get('cols'), ['group', 'paygroup', 'shift', 'ties', 'sells', 'buys', 'inside', 'sellsinside', 'buysinside', 'passed', 'free'], DEFAULTS.cols),
              lens: q.get('lens') || 'none', measure: pick(q.get('measure'), ['value', 'count'], DEFAULTS.measure),
              scale: pick(q.get('scale'), ['share', 'amount'], DEFAULTS.scale), net: pick(q.get('net'), ['net', 'gross'], DEFAULTS.net),
              cover: pick(q.get('cover'), ['0.5', '0.8', '0.9', '1'], DEFAULTS.cover), speed: pick(q.get('speed'), ['1.5', '3', '6', '12'], DEFAULTS.speed),
              show: pick(q.get('show'), ['money', 'goods'], DEFAULTS.show), dir: pick(q.get('dir'), ['both', 'down', 'up'], DEFAULTS.dir),
              smooth: q.get('smooth') !== '0', dots: q.get('dots') !== '0', inter: q.get('inter') === '1', halo: q.get('halo') === '1',
              pubonly: q.get('pubonly') === '1', trace: q.get('trace') === '1', t: 0, sel: -1 };

  // ---------------------------------------------------------------- colours, from the stylesheet so light and dark both work
  let C = {};
  function readTheme() {
    const cs = getComputedStyle(document.documentElement), v = n => cs.getPropertyValue(n).trim();
    C = { surface: v('--surface'), ink: v('--ink'), ink2: v('--ink2'), muted: v('--muted'), grid: v('--grid'), axis: v('--axis'),
          withc: v('--with'), against: v('--against'), dotWith: v('--dot-with'), dotAgainst: v('--dot-against'),
          node: v('--node'), nodeDim: v('--node-dim'), nodeLine: v('--node-line'), nodeOn: v('--node-on'), haloIn: v('--halo-in'), haloOut: v('--halo-out') };
  }

  // ---------------------------------------------------------------- link series
  let sup, buy, basis, rev, REF;                           // per link: supplier and buyer (indices into nodes), flags by month, the reverse link; reference sizes
  const pairKey = (a, b) => a * 1000 + b;
  // [link * NM + month]. smooth: trailing three-month average. pub: published months only (a hidden month counts as nothing,
  // and the average is taken over the published months in the window).
  function flatten(rows, pub, smooth) {
    const a = new Float32Array(NL * NM);
    for (let k = 0; k < NL; k++) { const r = rows[k], b = basis[k];
      for (let m = 0; m < NM; m++) {
        let s = 0, c = 0;
        for (let j = smooth ? Math.max(0, m - 2) : m; j <= m; j++) if (!pub || b[j] === 'p') { s += r[j]; c++; }
        a[k * NM + m] = c ? s / c : 0;
      } }
    return a;
  }
  const seriesCache = {};
  const series = (measure, smooth, pub) => { const key = measure + (smooth ? 's' : 'r') + (pub ? 'p' : 'h');
    return seriesCache[key] || (seriesCache[key] = flatten(measure === 'value' ? D.links.value : D.links.count, pub, smooth)); };
  const quantilePos = (a, p) => d3.quantile(Float32Array.from(a).filter(v => v > 0).sort(), p);

  // ---------------------------------------------------------------- node series (rest of the UK, and payments within an industry)
  const smooth3 = a => Float32Array.from(a, (_, m) => { let s = 0, c = 0; for (let j = Math.max(0, m - 2); j <= m; j++) { s += a[j]; c++; } return s / c; });
  const both = a => [Float32Array.from(a), smooth3(a)];                       // [single month, three-month average]
  const plus = (a, b) => a.map((v, i) => v + b[i]);
  const lerp = (a, t) => { const m0 = Math.floor(t), m1 = Math.min(NM - 1, m0 + 1), f = t - m0; return a[m0] * (1 - f) + a[m1] * f; };
  const xAt = (d, name, t) => { const s = d.x[name]; return name === 'self' ? lerp(s[S.smooth ? 1 : 0], t) : lerp(s[S.inter ? 1 : 0][S.smooth ? 1 : 0], t); };

  // ---------------------------------------------------------------- taking on a region's data
  function setData(data) {
    D = data; NM = D.months.length; NL = D.links.supplier.length;
    GROUPS = D.sector_groups; PAYGROUPS = [...D.pay_groups, NOGROUP];
    nodes = D.nodes.map((n, i) => ({ ...n, i, paygroup: n.pay_group || NOGROUP })); NN = nodes.length;
    monthDate = D.months.map(d3.timeParse('%Y-%m'));
    sup = Int16Array.from(D.links.supplier); buy = Int16Array.from(D.links.buyer); basis = D.links.basis;
    const linkAt = new Map(); for (let k = 0; k < NL; k++) linkAt.set(pairKey(sup[k], buy[k]), k);
    rev = Int32Array.from({ length: NL }, (_, k) => linkAt.has(pairKey(buy[k], sup[k])) ? linkAt.get(pairKey(buy[k], sup[k])) : -1);
    for (const cache of [seriesCache, totalsCache, shiftCache, tieCache, insideCache, chainCache, traceCache, passCache, layouts]) for (const k in cache) delete cache[k];
    REF = { value: quantilePos(series('value', true, false), 0.9995), count: quantilePos(series('count', true, false), 0.9995) };   // fixed, so widths compare across months
    for (const d of nodes) { const e = d.ext;                                 // [intermediaries out, in][single, smoothed]
      d.x = { self: both(d.self), buy: [both(e.buy_core), both(plus(e.buy_core, e.buy_int))], sell: [both(e.sell_core), both(plus(e.sell_core, e.sell_int))],
              buyLon: [both(e.buy_lon_core), both(plus(e.buy_lon_core, e.buy_lon_int))], sellLon: [both(e.sell_lon_core), both(plus(e.sell_lon_core, e.sell_lon_int))] }; }
    curV = new Float32Array(NL); curN = new Float32Array(NL); disp = new Float32Array(NL); shown = new Float32Array(NL);
    paidD = new Float64Array(NN); recvD = new Float64Array(NN); paidV = new Float64Array(NN); recvV = new Float64Array(NN);
    geo = new Float32Array(NL * (NSEG + 1) * 4); ctrl = new Float32Array(NL * 3); widthNow = new Float32Array(NL); radNow = new Float32Array(NN);
    hoverNode = hoverLink = -1; drawn = []; pos = null; tween = null; geoDirty = true; shownMonth = -1;
    buildOptions(); $('month').max = NM - 1;
  }

  // ---------------------------------------------------------------- which industries are switched in, and the totals that follow
  let act, lact, wholeTotal, nodeRef, lev;
  const totalsCache = {};
  const L = d => S.inter ? d.level_all : d.level;                             // each view uses levels worked out on its own set of industries
  const Lc = d => Math.max(lev[0], Math.min(lev[1], L(d)));                   // small industries are clamped into the range of the rest
  function monthTotals(measure, smooth, pub) {                                // sum over the links switched in, by month
    const key = [measure, smooth, pub, S.inter].join('|');
    if (!totalsCache[key]) { const a = series(measure, smooth, pub), t = new Float64Array(NM);
      for (let k = 0; k < NL; k++) if (lact[k]) for (let m = 0; m < NM; m++) t[m] += a[k * NM + m];
      totalsCache[key] = { byMonth: t, mean: d3.mean(t) }; }
    return totalsCache[key];
  }
  function setActive() {
    act = Uint8Array.from(nodes, d => (d.role === 'industry' || S.inter) ? 1 : 0);
    lact = Uint8Array.from({ length: NL }, (_, k) => (act[sup[k]] && act[buy[k]]) ? 1 : 0);
    const a = series('value', false, false); wholeTotal = new Float64Array(NN);
    for (let k = 0; k < NL; k++) if (lact[k]) { let t = 0; for (let m = 0; m < NM; m++) t += a[k * NM + m]; wholeTotal[sup[k]] += t; wholeTotal[buy[k]] += t; }
    nodeRef = d3.max(wholeTotal) / NM;
    lev = d3.extent(nodes.filter(d => act[d.i] && !d.small), L);
    if (S.sel >= 0 && !act[S.sel]) S.sel = -1;
  }

  // ---------------------------------------------------------------- money, or goods and services
  // The data are payments. "Money" draws each link from the industry paying to the industry paid, with buyers at the top.
  // "Goods and services" turns every link round, on the assumption that a payment bought something that travelled the
  // other way, and turns the order upside down so that suppliers are at the top and things still mostly run down the page.
  const goods = () => S.show === 'goods';
  const from = k => goods() ? sup[k] : buy[k], to = k => goods() ? buy[k] : sup[k];

  // ---------------------------------------------------------------- canvas and sizes
  const canvas = $('chart'), ctx = canvas.getContext('2d'), wrap = $('chartwrap');
  let W = 1000, H = 680, dpr = 1, K = 1;
  const pad = { l: 46, r: 132, t: 46, b: 22 };
  function resize() {
    W = Math.max(300, wrap.clientWidth); K = Math.max(0.5, Math.min(1, W / 1000));
    H = Math.round(Math.max(520, Math.min(860, W * 0.7)));
    pad.r = W < 620 ? 84 : 132; pad.l = W < 620 ? 30 : 46;
    dpr = window.devicePixelRatio || 1; canvas.width = Math.round(W * dpr); canvas.height = Math.round(H * dpr); canvas.style.height = H + 'px';
  }
  const rNode = tot => K * (3 + 30 * Math.sqrt(Math.max(0, tot) / nodeRef));   // area follows payments made plus received (GBP thousands a month)
  const wLink = (v, measure) => K * (0.5 + 13 * Math.sqrt(Math.max(0, v) / REF[measure]));

  // ---------------------------------------------------------------- layouts: worked out once each, then held still while time moves
  const layouts = {};
  // Columns: each one's width follows what it has to hold, so a column with the largest circles (or the most) gets room
  function columns(cols) {
    const names = cols === 'paygroup' ? PAYGROUPS : GROUPS, colOf = d => cols === 'paygroup' ? d.paygroup : d.group;
    const need = names.map(g => Math.sqrt(d3.sum(nodes.filter(d => act[d.i] && colOf(d) === g), d => 2 * rNode(wholeTotal[d.i] / NM) + 8 * K)) || 1);
    const total = d3.sum(need), x0 = pad.l + 8 * K, x1 = W - pad.r + 24 * K, centre = {}, width = {}; let acc = 0;
    names.forEach((g, j) => { const w = (x1 - x0) * need[j] / total; centre[g] = x0 + acc + w / 2; width[g] = w; acc += w; });
    return { names, centre, width, colOf };
  }
  // How much an industry's place in the order shifts: the standard deviation of its level, worked out on the 12 months
  // to each month (the line in the side panel). Small industries are left out of the ranking: their levels are noise.
  const ranking = sd => { const rank = new Int16Array(NN).fill(-1), ranked = nodes.filter(d => act[d.i] && !d.small).sort((a, b) => sd[a.i] - sd[b.i]);
    ranked.forEach((d, j) => { rank[d.i] = j; }); return { sd, rank, n: ranked.length, sorted: ranked.map(d => sd[d.i]) }; };
  const shiftCache = {};
  function shifts() {
    const key = S.inter ? 'all' : 'core'; if (shiftCache[key]) return shiftCache[key];
    const sd = new Float64Array(NN);
    for (const d of nodes) if (act[d.i]) sd[d.i] = d3.deviation((S.inter ? d.level_all_rolling : d.level_rolling).filter(v => v != null)) || 0;
    return (shiftCache[key] = ranking(sd));
  }
  // Columns "by how much its place shifts": industries in rank order, steadiest on the left, evenly spaced, with a
  // strip at the far right for the small ones. xOfSd places a standard deviation on that rank axis, for the tick marks.
  // Ties to the highlighted industries: of everything an industry pays to and is paid by other industries here, over the
  // whole period, the share that is with the highlighted set. A highlighted industry's partner counts by its share in the
  // set, so a partner that is 40% in the set counts for 40% of the payment. Same shape as shifts(), so it can use the same axis.
  // Three versions: 'ties' counts payments both ways; 'sells' only what the industry is paid (how much of it comes from the
  // highlighted industries); 'buys' only what it pays (how much of it goes to them).
  const tieCache = {}, isTies = c => c === 'ties' || c === 'sells' || c === 'buys';
  function ties(kind = 'ties') {
    const key = [kind, S.inter ? 'all' : 'core', S.lens].join('|'); if (tieCache[key]) return tieCache[key];
    const sh = nodes.map(lensShare), num = new Float64Array(NN), den = new Float64Array(NN), sd = new Float64Array(NN);
    const a = series('value', false, false);
    for (let k = 0; k < NL; k++) { if (!lact[k]) continue; let t = 0; for (let m = 0; m < NM; m++) t += a[k * NM + m];
      const s = sup[k], b = buy[k];
      if (kind !== 'buys') { num[s] += t * sh[b]; den[s] += t; }         // what s is paid, and how much of it by highlighted industries
      if (kind !== 'sells') { num[b] += t * sh[s]; den[b] += t; } }      // what b pays, and how much of it to highlighted industries
    for (let i = 0; i < NN; i++) sd[i] = den[i] > 0 ? num[i] / den[i] : 0;
    return (tieCache[key] = ranking(sd));
  }
  // Trade inside the region: of everything an industry pays to and is paid by industries anywhere in the UK, over the whole
  // period, the share that is with industries in this region. Payments within an industry are treated like any others:
  // the links leave them out, so the ones inside the region are added back here (the rest-of-the-UK figures already
  // include the same industry elsewhere). Three versions, as for ties: 'inside' counts
  // payments both ways; 'sellsinside' only what the industry is paid; 'buysinside' only what it pays. No highlight is needed.
  const insideCache = {}, isInside = c => c === 'inside' || c === 'sellsinside' || c === 'buysinside';
  function insides(kind = 'inside') {
    const key = [kind, S.inter ? 'all' : 'core'].join('|'); if (insideCache[key]) return insideCache[key];
    const paid = new Float64Array(NN), recv = new Float64Array(NN), sd = new Float64Array(NN), a = series('value', false, false), j = S.inter ? 1 : 0;
    for (let k = 0; k < NL; k++) { if (!lact[k]) continue; let t = 0; for (let m = 0; m < NM; m++) t += a[k * NM + m]; recv[sup[k]] += t; paid[buy[k]] += t; }
    for (const d of nodes) { if (!act[d.i]) continue; const own = d3.sum(d.x.self[0]);
      const ins = (kind !== 'buysinside' ? recv[d.i] + own : 0) + (kind !== 'sellsinside' ? paid[d.i] + own : 0);
      const out = (kind !== 'buysinside' ? d3.sum(d.x.sell[j][0]) : 0) + (kind !== 'sellsinside' ? d3.sum(d.x.buy[j][0]) : 0);
      sd[d.i] = ins + out > 0 ? ins / (ins + out) : 0; }
    return (insideCache[key] = ranking(sd));
  }
  // Chains. Over the whole period, what an industry pays to and is paid by other industries here splits three ways, with no
  // use of the levels. Two-way: paid to a partner that pays the same back (twice the smaller direction, pair by pair).
  // Passed along: what is left is net, some partners paying it and it paying others; twice the smaller of net-in and
  // net-out comes in from one industry and goes on to another. End of the line: the rest (more out than in is a buyer
  // end, more in than out a supplier end). Also how much of its value runs down the order, net and with both directions
  // counted, and each industry's net partners, largest first, for tracing a chain (tests/23_middle_links.cjs).
  const chainCache = {}, traceCache = {};
  function chains() {
    const key = S.inter ? 'all' : 'core'; if (chainCache[key]) return chainCache[key];
    const a = series('value', false, false), f = () => new Float64Array(NN), paid = f(), recv = f(), two = f(), netIn = f(), netOut = f(), gDown = f(), nDown = f();
    const tot = new Float64Array(NL), ups = nodes.map(() => []), downs = nodes.map(() => []), lv = nodes.map(L);       // ups[i]: who pays i, net; downs[i]: whom i pays, net
    for (let k = 0; k < NL; k++) if (lact[k]) { let t = 0; for (let m = 0; m < NM; m++) t += a[k * NM + m]; tot[k] = t; }
    for (let k = 0; k < NL; k++) { if (!lact[k]) continue; const s = sup[k], b = buy[k], t = tot[k], r = rev[k];
      recv[s] += t; paid[b] += t; if (lv[b] > lv[s]) { gDown[s] += t; gDown[b] += t; }
      if (r >= 0 && r < k) continue;                                       // each pair once
      const back = r >= 0 ? tot[r] : 0, mn = Math.min(t, back), v = Math.abs(t - back); two[s] += 2 * mn; two[b] += 2 * mn; if (!v) continue;
      const payer = t >= back ? b : s, payee = t >= back ? s : b, kk = t >= back ? k : r;
      netIn[payee] += v; netOut[payer] += v; if (lv[payer] > lv[payee]) { nDown[payee] += v; nDown[payer] += v; }
      ups[payee].push({ i: payer, k: kk, v }); downs[payer].push({ i: payee, k: kk, v }); }
    const twoWay = f(), passed = f(), end = f(), netDown = f(), grossDown = f(), buyerEnd = new Uint8Array(NN); let G = 0, T = 0, Pa = 0;
    for (const d of nodes) { const i = d.i, g = paid[i] + recv[i]; if (!g) continue;
      twoWay[i] = two[i] / g; passed[i] = 2 * Math.min(netIn[i], netOut[i]) / g; end[i] = Math.abs(netIn[i] - netOut[i]) / g; buyerEnd[i] = netOut[i] > netIn[i] ? 1 : 0;
      netDown[i] = netIn[i] + netOut[i] ? nDown[i] / (netIn[i] + netOut[i]) : 0; grossDown[i] = gDown[i] / g;
      ups[i].sort((p, q) => q.v - p.v); downs[i].sort((p, q) => q.v - p.v);
      if (act[i] && !d.small) { G += g; T += two[i]; Pa += 2 * Math.min(netIn[i], netOut[i]); } }
    const placed = nodes.filter(d => act[d.i] && !d.small), mid = arr => d3.median(placed, d => arr[d.i]);
    return (chainCache[key] = { twoWay, passed, end, buyerEnd, netDown, grossDown, netIn, netOut, ups, downs, midNetDown: mid(netDown), midGrossDown: mid(grossDown),
                                all: { twoWay: T / G, passed: Pa / G } });
  }
  // The chain traced from one industry: upwards through the net payers that give a tenth or more of its net receipts,
  // downwards through the net payees that take a tenth or more of its net payments, two steps each way. links: link -> step
  // (both directions of each pair, so whichever the month draws is picked out); members: industry -> step; up, down: the tree.
  const TRACE_SHARE = 0.1, TRACE_STEPS = 2;
  function traceOf(i) {
    const key = (S.inter ? 'all|' : 'core|') + i; if (traceCache[key]) return traceCache[key];
    const ch = chains(), links = new Map(), members = new Map([[i, 0]]);
    const walk = (j, up, step, seen) => { const base = up ? ch.netIn[j] : ch.netOut[j], out = [];
      for (const p of up ? ch.ups[j] : ch.downs[j]) { const share = p.v / base; if (share < TRACE_SHARE) break;
        for (const k of [p.k, rev[p.k]]) if (k >= 0 && !links.has(k)) links.set(k, step);
        if (!members.has(p.i)) members.set(p.i, step);
        const item = { i: p.i, v: p.v, share, next: [] }; out.push(item);
        if (!seen.has(p.i) && step < TRACE_STEPS) { seen.add(p.i); item.next = walk(p.i, up, step + 1, seen); } }
      return out; };
    return (traceCache[key] = { links, members, up: walk(i, true, 1, new Set([i])), down: walk(i, false, 1, new Set([i])) });
  }
  // Columns "by how much it passes along": the same ranked axis, on the share passed along
  const passCache = {};
  function passes() { const key = S.inter ? 'all' : 'core'; return passCache[key] || (passCache[key] = ranking(chains().passed)); }
  const isRanked = c => c === 'shift' || isTies(c) || isInside(c) || c === 'passed';         // columns that put industries in order of a measure
  // The columns in use. The three "by ties" choices need a highlight to measure against; without one they fall back to sector groups.
  const colsNow = () => (isTies(S.cols) && S.lens === 'none' ? 'group' : S.cols);
  // Words for the two ends of the axis: wide left, wide right, narrow left, narrow right
  const AXIS_ENDS = { shift: ['place in the order shifts least', 'shifts most', 'steadier', 'shifts more'],
    ties: ['least tied to the highlighted industries', 'most tied', 'less tied', 'more tied'],
    sells: ['sells least to the highlighted industries', 'sells most', 'sells less', 'sells more'],
    buys: ['buys least from the highlighted industries', 'buys most', 'buys less', 'buys more'],
    inside: ['trades least inside the region', 'trades most', 'less inside', 'more inside'],
    sellsinside: ['sells least inside the region', 'sells most', 'sells less here', 'sells more'],
    buysinside: ['buys least inside the region', 'buys most', 'buys less here', 'buys more'],
    passed: ['passes least along to another industry', 'passes most', 'passes less on', 'passes more'] };
  function shiftAxis(cols) {
    const st = isTies(cols) ? ties(cols) : isInside(cols) ? insides(cols) : cols === 'passed' ? passes() : shifts(), xa = pad.l + 30 * K, xEnd = W - pad.r + 24 * K, stripW = 62 * K, xb = xEnd - stripW - 16 * K, step = (xb - xa) / st.n;
    const xOfRank = j => xa + (j + 0.5) * step;
    const xOfSd = v => { const a = st.sorted, j = d3.bisectLeft(a, v); return j <= 0 ? xOfRank(0) : j >= a.length ? xOfRank(a.length - 1) : xOfRank(j - 1 + (v - a[j - 1]) / ((a[j] - a[j - 1]) || 1)); };
    return { st, xa, xb, step, xOfRank, xOfSd, stripW, stripX: xEnd - stripW / 2, stripLeft: xb + 8 * K };
  }
  // Height on the canvas of a level, in the supplier-order layout (turned over in the goods view)
  function yOfLevel(v) {
    const y0 = pad.t + 22 * K, y1 = H - pad.b - 22 * K, y = d3.scaleLinear().domain(lev).range([y1, y0]).clamp(true)(v);
    return goods() ? y0 + y1 - y : y;
  }
  const layoutKey = (mode, cols) => [mode, mode === 'order' ? cols : '-', S.inter ? 'all' : 'core', mode === 'order' && isTies(cols) ? S.lens : ''].join('|');
  function computeLayout(mode, cols) {
    const byShift = mode === 'order' && isRanked(cols), banded = mode === 'order' && (cols === 'group' || cols === 'paygroup');
    const cc = columns(byShift ? 'group' : cols), colOf = cc.colOf, gx = g => cc.centre[g], ax = byShift ? shiftAxis(cols) : null;
    const txOf = d => d.small ? ax.stripX : ax.xOfRank(ax.st.rank[d.i]);       // by shift: where an industry belongs sideways
    const x0 = pad.l + 24 * K, x1 = W - pad.r, y0 = pad.t + 22 * K, y1 = H - pad.b - 22 * K;
    const y = d3.scaleLinear().domain(lev).range([y1, y0]).clamp(true);
    const rnd = d3.randomLcg(7), base = mode === 'order' ? null : layoutFor('order', 'free');
    const sn = nodes.filter(d => act[d.i]).map(d => ({ i: d.i, level: L(d), col: colOf(d), small: d.small, r: rNode(wholeTotal[d.i] / NM), tx0: byShift ? txOf(d) : 0,
      x: base ? base[d.i].x : (byShift ? txOf(d) : banded ? gx(colOf(d)) : x0 + rnd() * (x1 - x0)), y: base ? base[d.i].y : y(L(d)) }));
    // Pull between industries. Supplier order: a pair's two-way total as a share of the smaller partner's payments.
    // Hybrid and connections: affinity, the pair's payments over what the two industries' sizes alone would predict,
    // so the biggest industries do not drag everything to the middle (tests/08_clusters.R).
    const a = series('value', false, false), pairs = new Map(), recvTot = new Float64Array(NN), paidTot = new Float64Array(NN); let grand = 0;
    for (let k = 0; k < NL; k++) { if (!lact[k]) continue; let t = 0; for (let m = 0; m < NM; m++) t += a[k * NM + m];
      const i = Math.min(sup[k], buy[k]), j = Math.max(sup[k], buy[k]); pairs.set(pairKey(i, j), (pairs.get(pairKey(i, j)) || 0) + t);
      recvTot[sup[k]] += t; paidTot[buy[k]] += t; grand += t; }
    const sl = [];
    for (const [kk, t] of pairs) { const i = Math.floor(kk / 1000), j = kk % 1000;
      if (byShift) continue;                             // by shift: position comes from the rank alone
      if (mode === 'order') { const s = t / Math.min(wholeTotal[i], wholeTotal[j]); if (s > 0.03) sl.push({ source: i, target: j, s }); }
      else { const aff = t / ((recvTot[i] * paidTot[j] + recvTot[j] * paidTot[i]) / grand); if (t > 1000 && aff > 1.5) sl.push({ source: i, target: j, s: Math.min(1, (aff - 1) / 8) * 0.45 }); } }
    const sim = d3.forceSimulation(sn).randomSource(rnd)
      .force('link', d3.forceLink(sl).id(d => d.i).strength(l => Math.min(1, l.s * 2.2) * (mode === 'order' ? 0.25 : 0.7)).distance(l => (50 + 130 * (1 - Math.min(1, l.s * 2))) * K))
      .force('charge', d3.forceManyBody().strength((mode === 'order' ? (banded ? -60 : byShift ? -20 : -420) : -260) * K))
      .force('collide', d3.forceCollide(d => d.r + (mode === 'order' ? 7 : 10) * K).iterations(3))
      .force('x', d3.forceX(d => byShift ? d.tx0 : banded ? gx(d.col) : (x0 + x1) / 2).strength(banded || byShift ? 0.8 : mode === 'force' ? 0.05 : 0.012))
      .force('y', d3.forceY(d => mode === 'force' ? (y0 + y1) / 2 : y(d.level)).strength(mode === 'order' ? 1 : mode === 'hybrid' ? 0.35 : 0.07))
      .stop();
    // Supplier order: height is the level, give or take a few pixels so circles at the same level can sit side by side
    // (the slack is under half the month-to-month wobble in a level). With columns, a circle stays inside its own column.
    const slack = 14 * K;
    const pin = () => { for (const d of sn) {
      if (mode === 'order') { const ty = y(d.level); d.y = Math.max(ty - slack, Math.min(ty + slack, d.y)); }
      if (banded) { const c = gx(d.col), hw = Math.max(0, cc.width[d.col] / 2 - Math.min(d.r, cc.width[d.col] / 2) - 1); d.x = Math.max(c - hw, Math.min(c + hw, d.x)); }
      if (byShift) { const tol = d.small ? Math.max(0, ax.stripW / 2 - d.r) : 22 * K; d.x = Math.max(d.tx0 - tol, Math.min(d.tx0 + tol, d.x)); }   // stays near its rank
      d.x = Math.max(x0, Math.min(x1 + (banded || byShift ? 24 * K : 0), d.x)); d.y = Math.max(y0, Math.min(y1, d.y)); } };
    for (let i = 0; i < 600; i++) { sim.tick(); pin(); }
    if (!banded && !byShift) {
      // Stretch to fill the frame. For hybrid and connections the stretch ignores the outermost few industries,
      // which are barely connected and would otherwise squeeze everything else; they are held at the edge.
      const xs = sn.map(d => d.x).sort(d3.ascending), ys = sn.map(d => d.y).sort(d3.ascending), p = mode === 'order' ? 0 : 0.04;
      const fx = d3.scaleLinear().domain([d3.quantile(xs, p), d3.quantile(xs, 1 - p)]).range([x0, x1]).clamp(true);
      const fy = d3.scaleLinear().domain([d3.quantile(ys, p), d3.quantile(ys, 1 - p)]).range([y0, y1]).clamp(true);
      for (const d of sn) { d.tx = fx(d.x); d.ty = mode === 'order' ? d.y : fy(d.y); d.x = d.tx; d.y = d.ty; }
      const relax = d3.forceSimulation(sn).force('collide', d3.forceCollide(d => d.r + 6 * K).iterations(2))
        .force('x', d3.forceX(d => d.tx).strength(0.25)).force('y', d3.forceY(d => d.ty).strength(0.25)).stop();
      for (let i = 0; i < 60; i++) { relax.tick(); pin(); }
    }
    const out = nodes.map(() => ({ x: -300, y: -300 }));          // switched-out industries are parked off the canvas
    for (const d of sn) out[d.i] = { x: d.x, y: d.y };
    return out;
  }
  function layoutFor(mode, cols) { const k = layoutKey(mode, cols); return layouts[k] || (layouts[k] = computeLayout(mode, cols)); }
  function targetLayout() {
    const base = layoutFor(S.layout, colsNow());
    if (!goods() || S.layout === 'force') return base;
    const k = layoutKey(S.layout, colsNow()) + '|flipped', mid = (pad.t + 22 * K) + (H - pad.b - 22 * K);
    return layouts[k] || (layouts[k] = base.map(p => (p.y < -100 ? p : { x: p.x, y: mid - p.y })));
  }
  let pos = null, tween = null;
  function moveTo(instant) {
    const target = targetLayout();
    if (instant || !pos || reduceMotion) { pos = target.map(p => ({ ...p })); tween = null; geoDirty = true; requestDraw(); return; }
    tween = { from: pos.map((p, i) => (p.x < -100 ? { ...target[i] } : { ...p })), to: target, t0: performance.now() };   // industries coming in appear in place
    requestDraw();
  }

  // ---------------------------------------------------------------- values at a (possibly fractional) month
  let curV, curN, disp, shown, paidD, recvD, paidV, recvV;  // allocated by setData()
  let scaleNow = 1, monthTotalNow = 0;
  function valuesAt(t) {
    const va = series('value', S.smooth, S.pubonly), na = series('count', S.smooth, S.pubonly);
    const m0 = Math.floor(t), m1 = Math.min(NM - 1, m0 + 1), f = t - m0, g = 1 - f, isVal = S.measure === 'value';
    const tot = monthTotals(S.measure, S.smooth, S.pubonly); monthTotalNow = tot.byMonth[m0] * g + tot.byMonth[m1] * f;
    scaleNow = S.scale === 'share' && monthTotalNow > 0 ? tot.mean / monthTotalNow : 1;
    paidD.fill(0); recvD.fill(0); paidV.fill(0); recvV.fill(0);
    for (let k = 0; k < NL; k++) {
      if (!lact[k]) { curV[k] = curN[k] = disp[k] = 0; continue; }
      const v = va[k * NM + m0] * g + va[k * NM + m1] * f, n = na[k * NM + m0] * g + na[k * NM + m1] * f, d = (isVal ? v : n) * scaleNow;
      curV[k] = v; curN[k] = n; disp[k] = d; const s = sup[k], b = buy[k];
      recvD[s] += d; paidD[b] += d; recvV[s] += v; paidV[b] += v;
    }
    const net = S.net === 'net';
    for (let k = 0; k < NL; k++) shown[k] = net && rev[k] >= 0 ? Math.max(0, disp[k] - disp[rev[k]]) : disp[k];
  }

  // ---------------------------------------------------------------- highlight
  function lensShare(d) {                                 // 0 = not in the highlighted set; up to 1 = the share of the industry's jobs that is
    const i = S.lens.indexOf(':'), kind = S.lens.slice(0, i), name = S.lens.slice(i + 1);
    if (kind === 'group') return d.group === name ? 1 : 0;
    if (kind === 'pay') return d.paygroup === name ? 1 : 0;
    if (kind === 'fe') { const f = d.fe || [0, 0, 0]; return Math.min(1, name === 'material' ? f[0] : name === 'providential' ? f[1] : name === 'overlooked' ? f[2] : f[0] + f[1]); }
    if (kind === 'is8') { if (name === 'any') return Math.min(1, d.is8_any_share || 0); const h = d.is8.find(e => e.name === name); return h ? Math.min(1, h.share) : 0; }
    return 0;
  }

  // ---------------------------------------------------------------- link geometry: fixed while the layout is still
  const NSEG = 8; let geo, ctrl;                           // geo: x, y, normal x, normal y for each point along a link; ctrl: control x, y, length
  let geoDirty = true;
  function buildGeo() {
    for (let k = 0; k < NL; k++) {
      const a = pos[from(k)], z = pos[to(k)], dx = z.x - a.x, dy = z.y - a.y, len = Math.hypot(dx, dy) || 1;
      const cx = (a.x + z.x) / 2 - dy * 0.16, cy = (a.y + z.y) / 2 + dx * 0.16;         // bowed to one side, so the two directions of a pair separate
      ctrl[k * 3] = cx; ctrl[k * 3 + 1] = cy; ctrl[k * 3 + 2] = len;
      for (let i = 0; i <= NSEG; i++) {
        const u = i / NSEG, v = 1 - u, o = (k * (NSEG + 1) + i) * 4;
        const tx = 2 * v * (cx - a.x) + 2 * u * (z.x - cx), ty = 2 * v * (cy - a.y) + 2 * u * (z.y - cy), tl = Math.hypot(tx, ty) || 1;
        geo[o] = v * v * a.x + 2 * v * u * cx + u * u * z.x; geo[o + 1] = v * v * a.y + 2 * v * u * cy + u * u * z.y; geo[o + 2] = -ty / tl; geo[o + 3] = tx / tl;
      }
    }
    geoDirty = false;
  }
  const THIN = 2.4;                                       // px: below this a held link is drawn dashed; above it, with a dashed centre line
  const TAPER = 0.75;                                     // a link narrows to a quarter of its width at the end it runs to
  function ribbon(c, k, w) {
    const o = k * (NSEG + 1) * 4;
    for (let i = 0; i <= NSEG; i++) { const p = o + i * 4, hw = w * (1 - TAPER * i / NSEG) / 2; if (i) c.lineTo(geo[p] + geo[p + 2] * hw, geo[p + 1] + geo[p + 3] * hw); else c.moveTo(geo[p] + geo[p + 2] * hw, geo[p + 1] + geo[p + 3] * hw); }
    for (let i = NSEG; i >= 0; i--) { const p = o + i * 4, hw = w * (1 - TAPER * i / NSEG) / 2; c.lineTo(geo[p] - geo[p + 2] * hw, geo[p + 1] - geo[p + 3] * hw); }
    c.closePath();
  }

  // ---------------------------------------------------------------- drawing
  let hoverNode = -1, hoverLink = -1, phase = 0, drawn = [], lastScriptMs = 0;
  let widthNow, radNow;
  function wrapText(text, maxW) { const words = text.split(' '), lines = []; let line = '';
    for (const w of words) { const t = line ? line + ' ' + w : w; if (line && ctx.measureText(t).width > maxW) { lines.push(line); line = w; } else line = t; }
    lines.push(line); return lines; }

  function draw() {
    if (!D || !pos) return;
    const t0 = performance.now(), t = S.t, m0 = Math.round(t), P = pos;
    valuesAt(t);
    if (geoDirty) buildGeo();
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0); ctx.clearRect(0, 0, W, H);
    const lensOn = S.lens !== 'none', share = lensOn ? nodes.map(lensShare) : null, live = nodes.filter(n => act[n.i]);
    const focus = hoverNode >= 0 ? hoverNode : S.sel, isVal = S.measure === 'value';
    const chain = S.trace && S.sel >= 0 ? traceOf(S.sel) : null, tr = hoverNode < 0 ? chain : null;      // the pinned industry's chain; hovering another industry shows that one's links as usual
    const toValue = isVal ? 1 : monthTotals('value', S.smooth, S.pubonly).mean / monthTotals('count', S.smooth, S.pubonly).mean;   // counts are sized on the value scale
    for (const d of live) radNow[d.i] = rNode((paidD[d.i] + recvD[d.i]) * toValue);
    const font = px => `${px}px system-ui, -apple-system, "Segoe UI", sans-serif`, fs = W < 620 ? 10 : 11;

    // ---- axis cue and column headings
    ctx.font = font(fs); ctx.textBaseline = 'middle'; ctx.fillStyle = C.muted;
    if (S.layout !== 'force') {
      ctx.save(); ctx.translate(pad.l * 0.42, H / 2); ctx.rotate(-Math.PI / 2); ctx.textAlign = 'center';
      const lo = goods() ? 'buyer' : 'supplier', hi = goods() ? 'supplier' : 'buyer';
      ctx.fillText(W < 620 ? `${lo}s  ←    →  ${hi}s` : `more of a ${lo}   ←   position in the order   →   more of a ${hi}`, 0, 0); ctx.restore();
    }
    if (S.layout === 'order') {                           // the average industry: buyers on one side of this line, suppliers on the other
      const ya = Math.round(yOfLevel(0)) + 0.5;
      ctx.strokeStyle = C.axis; ctx.lineWidth = 1; ctx.beginPath(); ctx.moveTo(pad.l - 4, ya); ctx.lineTo(W - 6, ya); ctx.stroke();
      ctx.textAlign = 'right'; ctx.fillStyle = C.muted; ctx.fillText('average industry', W - 8, ya - fs * 0.75);
    }
    if (S.layout === 'order' && isRanked(colsNow())) {
      const c = colsNow(), asPct = c !== 'shift', ax = shiftAxis(c), narrow = W < 620, ends = AXIS_ENDS[c];
      const ticks = isInside(c) || c === 'passed' ? [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.05, 0.15, 0.25, 0.35, 0.45] : isTies(c) ? [0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7] : [0.02, 0.05, 0.1, 0.15];
      ctx.fillStyle = C.ink2; ctx.textAlign = 'left'; ctx.fillText(ends[narrow ? 2 : 0], ax.xa, 12);
      ctx.textAlign = 'right'; ctx.fillText(ends[narrow ? 3 : 1], ax.xb - 10 * K, 12);
      ctx.textAlign = 'center'; (narrow ? ['Small'] : ['Too small', 'to say']).forEach((ln, i) => ctx.fillText(ln, ax.stripX + 4 * K, 12 + i * (fs + 3)));
      ctx.fillStyle = C.muted; ctx.strokeStyle = C.grid; ctx.lineWidth = 1; ctx.beginPath(); const placed = [];
      for (const v of ticks) { if (v < ax.st.sorted[0] || v > ax.st.sorted[ax.st.n - 1]) continue;       // ticks: the measure at that point in the ranking
        const x = Math.round(ax.xOfSd(v)) + 0.5; if (placed.some(p => Math.abs(x - p) < Math.max(30, 38 * K))) continue; placed.push(x);       // earlier in the list wins: skip a tick that would sit on top of one already drawn
        ctx.moveTo(x, pad.t - 8); ctx.lineTo(x, H - pad.b + 6); ctx.fillText(asPct ? Math.round(100 * v) + '%' : v.toFixed(2), x, 28); }
      const xs = Math.round(ax.stripLeft) + 0.5; ctx.moveTo(xs, pad.t - 8); ctx.lineTo(xs, H - pad.b + 6); ctx.stroke();
    }
    if (S.layout === 'order' && (colsNow() === 'group' || colsNow() === 'paygroup')) {
      const cc = columns(colsNow());
      ctx.textAlign = 'center'; ctx.fillStyle = C.ink2;
      if (W < 620) cc.names.forEach((g, j) => ctx.fillText(SHORT_HEAD[g] || g.split(/,| and /)[0], cc.centre[g], 11 + (j % 2) * (fs + 3)));      // narrow: short names on two staggered rows
      else for (const g of cc.names) wrapText(g, cc.width[g] - 8).slice(0, 3).forEach((ln, i) => ctx.fillText(ln, cc.centre[g], 12 + i * (fs + 2)));
      ctx.strokeStyle = C.grid; ctx.lineWidth = 1; ctx.beginPath();                                                           // hairlines between the columns
      cc.names.slice(1).forEach(g => { const x = Math.round(cc.centre[g] - cc.width[g] / 2) + 0.5; ctx.moveTo(x, pad.t - 6); ctx.lineTo(x, H - pad.b + 6); }); ctx.stroke();
    }

    // ---- halos: payments to and from the rest of the UK, as areas on the circle's own scale.
    // Each flow keeps its colour. Money: what an industry is paid from outside sits above it (buyers are above) and what it
    // pays out sits below. Goods: what it buys from outside sits above (suppliers are above) and what it sells sits below.
    if (S.halo && isVal) {
      const c = K * K * 900 / nodeRef;                   // px^2 of radius-squared per GBP thousand, matching rNode without its floor
      const upper = goods() ? 'buy' : 'sell', lower = goods() ? 'sell' : 'buy', colOf = name => name === 'sell' ? C.haloIn : C.haloOut;
      for (const d of live) {
        const p = P[d.i], r = radNow[d.i], dim = lensOn && share[d.i] === 0 && d.i !== focus, f = dim ? 0.4 : 1;
        for (const [name, a0, a1] of [[upper, Math.PI, 2 * Math.PI], [lower, 0, Math.PI]]) {
          const R = Math.sqrt(r * r + 2 * c * xAt(d, name, t) * scaleNow);
          ctx.beginPath(); ctx.arc(p.x, p.y, R, a0, a1); ctx.closePath(); ctx.fillStyle = ctx.strokeStyle = colOf(name);
          ctx.globalAlpha = 0.3 * f; ctx.fill(); ctx.globalAlpha = 0.85 * f; ctx.lineWidth = 1.1; ctx.stroke();
        }
      }
      ctx.globalAlpha = 1;
    }

    // ---- which links: the largest, until they carry the chosen share of the month's total
    const cand = []; let total = 0;
    // Direction: both, or only the links that run down the order (blue), or only those that run up it (orange).
    // The share-of-the-month cut-off below is then taken within what is left.
    const want = S.dir === 'both' ? -1 : S.dir === 'down' ? 1 : 0;
    for (let k = 0; k < NL; k++) if (shown[k] > 0 && (want < 0 || (L(nodes[buy[k]]) > L(nodes[sup[k]]) ? 1 : 0) === want)) { cand.push(k); total += shown[k]; }
    cand.sort((i, j) => shown[j] - shown[i]);
    const cover = +S.cover; let n = cand.length;
    if (cover < 1) { let acc = 0; n = 0; while (n < cand.length && acc < cover * total) acc += shown[cand[n++]]; }
    drawn = cand.slice(0, n); widthNow.fill(0);
    if (chain) for (let j = n; j < cand.length; j++) if (chain.links.has(cand[j])) drawn.push(cand[j]);      // a traced chain's links are drawn however small
    const touching = k => tr ? tr.links.has(k) : focus >= 0 ? (sup[k] === focus || buy[k] === focus) : (!lensOn || share[sup[k]] > 0 || share[buy[k]] > 0);

    // ---- links, smallest first so the largest sit on top
    for (let i = drawn.length - 1; i >= 0; i--) {
      const k = drawn[i], w = wLink(shown[k], S.measure), down = L(nodes[buy[k]]) > L(nodes[sup[k]]); widthNow[k] = w;
      const alpha = hoverLink >= 0 ? (k === hoverLink ? 0.92 : 0.07) : tr ? (tr.links.has(k) ? (tr.links.get(k) === 1 ? 0.88 : 0.55) : 0.035) : focus >= 0 ? (touching(k) ? 0.85 : 0.05) : lensOn ? (touching(k) ? 0.62 : 0.06) : 0.22 + 0.4 * Math.min(1, w / (8 * K));
      ctx.globalAlpha = alpha;
      if (w <= THIN * K && !S.pubonly && basis[k][m0] === 'h') {    // a thin link in a hidden month: drawn dashed, as there is no room for a centre line
        const g = k * 3, a = P[from(k)], z = P[to(k)];
        ctx.setLineDash([4, 3]); ctx.lineWidth = Math.max(0.8, w * 0.7); ctx.strokeStyle = down ? C.withc : C.against;
        ctx.beginPath(); ctx.moveTo(a.x, a.y); ctx.quadraticCurveTo(ctrl[g], ctrl[g + 1], z.x, z.y); ctx.stroke(); ctx.setLineDash([]);
      } else { ctx.beginPath(); ribbon(ctx, k, w); ctx.fillStyle = down ? C.withc : C.against; ctx.fill(); }
    }
    // ---- on top of the links: a dashed centre line where the month is hidden and held, and the payment dots
    for (let i = drawn.length - 1; i >= 0; i--) {
      const k = drawn[i], w = widthNow[k];
      if (hoverLink >= 0 ? k !== hoverLink : !touching(k)) continue;
      const a = P[from(k)], z = P[to(k)], g = k * 3, cx = ctrl[g], cy = ctrl[g + 1], len = ctrl[g + 2], down = L(nodes[buy[k]]) > L(nodes[sup[k]]);
      if (!S.pubonly && basis[k][m0] === 'h' && w > THIN * K) {
        ctx.globalAlpha = 0.9; ctx.setLineDash([3, 4]); ctx.strokeStyle = C.surface; ctx.lineWidth = Math.max(0.8, w * 0.28);
        ctx.beginPath(); ctx.moveTo(a.x, a.y); ctx.quadraticCurveTo(cx, cy, z.x, z.y); ctx.stroke(); ctx.setLineDash([]);
      }
      const nd = S.dots ? Math.round(curN[k] / DOT_UNIT) : 0;
      if (nd > 0) {
        // a dot inside a wide link is drawn in the page colour; on a thin link it is a darker shade of the link
        const r = 1.7 * Math.max(0.8, K), inside = [], outside = [];
        for (let j = 0; j < nd; j++) { const u = ((phase * 26 / len) + j / nd) % 1, v = 1 - u;
          (w * (1 - TAPER * u) / 2 > r + 1 ? inside : outside).push(v * v * a.x + 2 * v * u * cx + u * u * z.x, v * v * a.y + 2 * v * u * cy + u * u * z.y); }
        ctx.globalAlpha = 0.95;
        for (const [pts, col] of [[inside, C.surface], [outside, down ? C.dotWith : C.dotAgainst]]) { if (!pts.length) continue;
          ctx.beginPath(); for (let j = 0; j < pts.length; j += 2) { ctx.moveTo(pts[j] + r, pts[j + 1]); ctx.arc(pts[j], pts[j + 1], r, 0, 6.2832); } ctx.fillStyle = col; ctx.fill(); }
      }
    }
    ctx.globalAlpha = 1;

    // ---- industries
    for (const d of live) {
      const p = P[d.i], r = radNow[d.i], sh = lensOn ? share[d.i] : 0, off = tr && !tr.members.has(d.i), dim = off || (lensOn && sh === 0 && d.i !== focus);
      ctx.beginPath(); ctx.arc(p.x, p.y, r, 0, 6.2832);
      ctx.fillStyle = d.small ? C.surface : lensOn || off ? C.nodeDim : C.node; ctx.fill();
      if (sh > 0) { ctx.beginPath(); ctx.moveTo(p.x, p.y); ctx.arc(p.x, p.y, r, -Math.PI / 2, -Math.PI / 2 + 6.2832 * sh); ctx.closePath(); ctx.fillStyle = C.nodeOn; ctx.fill(); }
      ctx.beginPath(); ctx.arc(p.x, p.y, r, 0, 6.2832);
      ctx.lineWidth = d.i === focus ? 2.25 : 1; ctx.strokeStyle = d.i === focus ? C.ink : (dim || d.small) ? C.muted : C.nodeLine; ctx.stroke();
      if (d.i === S.sel) { ctx.beginPath(); ctx.arc(p.x, p.y, r + 3.5, 0, 6.2832); ctx.lineWidth = 1.5; ctx.strokeStyle = C.ink; ctx.stroke(); }
    }
    // ---- names: the focus first, then highlighted industries, then by size; skipped where they would collide
    ctx.font = font(fs); ctx.textAlign = 'left'; ctx.textBaseline = 'middle'; ctx.lineJoin = 'round';
    const boxes = [], rank = d => (d.i === focus ? 8 : 0) + (tr && tr.members.has(d.i) ? 4 : 0) + (lensOn && share[d.i] > 0 ? 2 : 0) + (d.small ? 0 : 1);
    const byRank = live.slice().sort((a, b) => rank(b) - rank(a) || radNow[b.i] - radNow[a.i]);
    for (const d of byRank) {
      const p = P[d.i], r = radNow[d.i] + (d.i === S.sel ? 4 : 0), tw = ctx.measureText(d.short).width;
      let x = p.x + r + 3; if (x + tw > W - 2) x = p.x - r - 3 - tw;                    // flip to the left at the right-hand edge
      const bx = { x0: x - 1, y0: p.y - fs * 0.62, x1: x + tw + 1, y1: p.y + fs * 0.62 };
      if (d.i !== focus && boxes.some(o => bx.x0 < o.x1 && bx.x1 > o.x0 && bx.y0 < o.y1 && bx.y1 > o.y0)) continue;
      boxes.push(bx); ctx.lineWidth = 3; ctx.strokeStyle = C.surface; ctx.strokeText(d.short, x, p.y);
      ctx.fillStyle = (tr ? !tr.members.has(d.i) : (lensOn && share[d.i] === 0 && d.i !== focus) || d.small) ? C.muted : C.ink; ctx.fillText(d.short, x, p.y);
    }
    lastScriptMs = performance.now() - t0;
  }

  // ---------------------------------------------------------------- the frame loop: runs only while something is moving
  let raf = 0, dirty = true, playing = false, lastNow = 0, shownMonth = -1, skip = false;
  const dotsMoving = () => S.dots && !reduceMotion;
  function frame(now) {
    raf = 0;
    const dt = Math.min(0.1, (now - lastNow) / 1000); lastNow = now;
    if (playing) { S.t = Math.min(NM - 1, S.t + dt * +S.speed); if (S.t >= NM - 1) setPlaying(false); dirty = true; }
    if (tween) { const u = Math.min(1, (now - tween.t0) / 700), e = d3.easeCubicInOut(u);
      pos = tween.from.map((p, i) => ({ x: p.x + (tween.to[i].x - p.x) * e, y: p.y + (tween.to[i].y - p.y) * e })); geoDirty = true; dirty = true; if (u >= 1) tween = null; }
    if (dotsMoving()) { phase += dt; if (!playing && !tween) { skip = !skip; if (!skip) dirty = true; } else dirty = true; }   // dots alone redraw every other frame
    if (dirty) { draw(); dirty = false; if (Math.round(S.t) !== shownMonth) updateReadouts(); }
    if (playing || tween || dotsMoving()) schedule();
  }
  function schedule() { if (!raf) { if (!playing && !tween && !dotsMoving()) lastNow = performance.now(); raf = requestAnimationFrame(frame); } }
  function requestDraw() { dirty = true; if (!raf) lastNow = performance.now(); schedule(); }
  // The side panel and table read the values the last draw worked out, so a change of state draws first, then refreshes them
  function refresh() { if (!pos) return; draw(); dirty = false; updateReadouts(); schedule(); }

  // ---------------------------------------------------------------- read-outs outside the canvas
  const lowerFirst = t => t.startsWith('Three') ? 't' + t.slice(1) : t;
  const periodText = m => S.smooth ? `Three months to ${fmtMonth(monthDate[m])}` : fmtMonth(monthDate[m]);
  let stripMax = 1;
  function stripTotals(m) {                                // GBP thousands, always with held values
    const va = series('value', S.smooth, false); let inside = 0, out = 0, inn = 0;
    for (let k = 0; k < NL; k++) if (lact[k]) inside += va[k * NM + m];
    for (const d of nodes) if (act[d.i]) { inside += xAt(d, 'self', m); out += xAt(d, 'buy', m); inn += xAt(d, 'sell', m); }
    return { inside, out, inn };
  }
  function updateStripMax() { stripMax = 1; for (let m = 0; m < NM; m++) { const s = stripTotals(m); stripMax = Math.max(stripMax, s.inside, s.out, s.inn); } }
  function updateReadouts() {
    const m = Math.round(S.t); shownMonth = m;
    $('monthLabel').replaceChildren(document.createTextNode(fmtMonthShort(monthDate[m])), el('small', null, S.smooth ? 'three-month average' : 'single month'));
    const slider = $('month'); if (+slider.value !== m) slider.value = m; slider.setAttribute('aria-valuetext', fmtMonth(monthDate[m]));
    const s = stripTotals(m);
    for (const [id, v] of [['inside', s.inside], ['out', s.out], ['in', s.inn]]) { $('t-' + id).textContent = fmtMoney(v); $('b-' + id).style.width = (100 * v / stripMax).toFixed(1) + '%'; }
    $('strip-note').textContent = `${periodText(m)}, a month, with hidden months held at their average. ` + (goods()
      ? `For every £1 of buying and selling inside the region, £${(s.out / s.inside).toFixed(2)} is bought from the rest of the UK and £${(s.inn / s.inside).toFixed(2)} is sold to it.`
      : `For every £1 paid inside the region, £${(s.out / s.inside).toFixed(2)} goes out to the rest of the UK and £${(s.inn / s.inside).toFixed(2)} comes in.`) + (S.inter ? '' : ' Intermediaries are left out.');
    sparkMarker(m);
    canvas.setAttribute('aria-label', `Network of payments between ${nodes.filter(n => act[n.i]).length} industries in Yorkshire and the Humber, ${lowerFirst(periodText(m))}. ${drawn.length} links are drawn. The table below the chart lists them.`);
    if ($('tablebox').open) renderTable();
    renderDetail();
  }

  // ---- the small line of monthly totals above the slider
  let sparkX = null, sparkY = null;
  function buildSpark() {
    const svg = d3.select('#spark'), w = $('spark').clientWidth || 600, h = 40, tot = monthTotals('value', false, false).byMonth;
    sparkX = d3.scaleLinear().domain([0, NM - 1]).range([8, w - 8]);          // lines up with the slider's thumb travel
    sparkY = d3.scaleLinear().domain([0, d3.max(tot)]).range([h - 13, 3]);
    svg.attr('viewBox', `0 0 ${w} ${h}`).selectAll('*').remove();
    svg.append('path').attr('d', d3.line().x((_, i) => sparkX(i)).y(v => sparkY(v))(Array.from(tot))).attr('fill', 'none').attr('stroke', 'var(--muted)').attr('stroke-width', 1.5).attr('stroke-linejoin', 'round');
    D.months.forEach((mm, i) => { if (!mm.endsWith('-01')) return;
      svg.append('line').attr('x1', sparkX(i)).attr('x2', sparkX(i)).attr('y1', h - 12).attr('y2', h - 8).attr('stroke', 'var(--axis)');
      svg.append('text').attr('x', sparkX(i)).attr('y', h - 1).attr('font-size', 10).attr('fill', 'var(--muted)').attr('text-anchor', i === 0 ? 'start' : 'middle').text(mm.slice(0, 4)); });
    svg.append('circle').attr('id', 'sparkdot').attr('r', 4).attr('fill', 'var(--ink)').attr('stroke', 'var(--surface)').attr('stroke-width', 2);
    svg.append('title').text('Payments between industries inside the region, each month');
  }
  function sparkMarker(m) { if (!sparkX) return; const tot = monthTotals('value', false, false).byMonth; d3.select('#sparkdot').attr('cx', sparkX(m)).attr('cy', sparkY(tot[m])); }

  // ---- the key
  function buildKey() {
    const key = $('key'); key.replaceChildren(el('h2', null, 'Key'));
    const svgNS = 'http://www.w3.org/2000/svg';
    const svg = (w, h) => { const s = document.createElementNS(svgNS, 'svg'); s.setAttribute('width', w); s.setAttribute('height', h); s.setAttribute('viewBox', `0 0 ${w} ${h}`); return s; };
    const add = (s, tag, attrs) => { const e = document.createElementNS(svgNS, tag); for (const a in attrs) e.setAttribute(a, attrs[a]); s.appendChild(e); return e; };
    const rib = (s, y, w, fill, x0 = 3, x1 = 63) => add(s, 'path', { d: `M${x0},${y - w / 2}L${x1},${y - w * (1 - TAPER) / 2}L${x1},${y + w * (1 - TAPER) / 2}L${x0},${y + w / 2}Z`, fill, 'fill-opacity': 0.75 });
    const row = (graphic, html) => { const r = el('div', 'k-row'); r.appendChild(graphic); const t = el('div'); t.innerHTML = html; r.appendChild(t); key.appendChild(r); };
    let s = svg(66, 16); rib(s, 8, 7, 'var(--with)'); row(s, goods() ? '<b>Blue:</b> goods and services run down the page, from a supplier to a buyer below it.' : '<b>Blue:</b> money runs down the order, from a buyer to a supplier below it.');
    s = svg(66, 16); rib(s, 8, 7, 'var(--against)'); row(s, goods() ? '<b>Orange:</b> goods and services run up the page.' : '<b>Orange:</b> money runs up the order.');
    const tot = monthTotals(S.measure, S.smooth, S.pubonly).mean, isVal = S.measure === 'value';
    const samples = S.scale === 'share' ? [[0.005, '0.5%'], [0.02, '2%'], [0.05, '5%']].map(([f, lab]) => [f * tot, lab]) : isVal ? [[1e3, '£1m'], [1e4, '£10m'], [5e4, '£50m']] : [[500, '500'], [2000, '2,000'], [10000, '10,000']];
    s = svg(66, 44); samples.forEach(([v], i) => rib(s, 6 + i * 15 + (i ? i * 2 : 0), Math.min(18, wLink(v, S.measure)), 'var(--muted)'));
    row(s, `<b>Width:</b> ${samples.map(x => x[1]).join(', ')} ${S.scale === 'share' ? `of the month's ${isVal ? 'payments' : 'number of payments'}` : isVal ? 'a month' : 'payments a month'}. A link is widest at the ${goods() ? 'supplying' : 'paying'} end${S.net === 'net' ? ' and shows what is left after the two directions are set against each other' : ''}.`);
    s = svg(66, 16); rib(s, 8, 9, 'var(--with)'); [14, 30, 46].forEach(x => add(s, 'circle', { cx: x, cy: 8, r: 1.9, fill: 'var(--surface)' }));
    row(s, `<b>Dots:</b> each is 100 payments that month${goods() ? ', drawn moving with what was paid for' : ''}. A wide link with few dots is a few large payments; a thin crowded one is many small ones. No dots means under 50 payments.`);
    s = svg(66, 24); rib(s, 7, 9, 'var(--with)'); add(s, 'line', { x1: 5, x2: 61, y1: 7, y2: 7, stroke: 'var(--surface)', 'stroke-width': 2, 'stroke-dasharray': '3 4' });
    add(s, 'line', { x1: 3, x2: 63, y1: 20, y2: 20, stroke: 'var(--with)', 'stroke-width': 1.2, 'stroke-dasharray': '4 3' });
    row(s, '<b>Dashed:</b> this month is hidden by ONS and shown at the average of the link’s published months.');
    const cs = S.scale === 'share' ? [[0.02 * monthTotals('value', S.smooth, S.pubonly).mean, '2%'], [0.2 * monthTotals('value', S.smooth, S.pubonly).mean, '20%']] : [[5e3, '£5m'], [1e5, '£100m']];
    s = svg(66, 2 * rNode(cs[1][0]) + 4); const R = rNode(cs[1][0]); cs.slice().reverse().forEach(([v]) => add(s, 'circle', { cx: 33, cy: 2 * R + 2 - rNode(v), r: rNode(v), fill: 'var(--node)', stroke: 'var(--node-line)' }));
    row(s, isVal ? `<b>Circle:</b> an industry. Area is what it pays plus what it is paid by other industries here: ${cs[0][1]} and ${cs[1][1]} ${S.scale === 'share' ? 'of the month’s payments' : 'a month'}.`
      : '<b>Circle:</b> an industry. Area is the number of payments it makes to, plus receives from, other industries here.');
    s = svg(66, 22); add(s, 'circle', { cx: 16, cy: 11, r: 9, fill: 'var(--node-on)', stroke: 'var(--node-line)' });
    add(s, 'circle', { cx: 46, cy: 11, r: 9, fill: 'var(--node-dim)', stroke: 'var(--node-line)' }); add(s, 'path', { d: 'M46,11L46,2A9,9 0 0 1 54.6,13.8Z', fill: 'var(--node-on)' });
    row(s, '<b>Highlight:</b> a filled circle is in the chosen set. A wedge is the share of the industry’s jobs that is.');
    s = svg(66, 20); add(s, 'circle', { cx: 33, cy: 10, r: 5, fill: 'var(--surface)', stroke: 'var(--muted)' });
    row(s, '<b>Hollow:</b> under £0.5m a month, too little to place reliably.');
    if (S.halo) { s = svg(66, 44); const up = goods() ? 'out' : 'in', dn = goods() ? 'in' : 'out';
      add(s, 'path', { d: 'M13,22A20,20 0 0 1 53,22Z', fill: `var(--halo-${up})`, 'fill-opacity': 0.3, stroke: `var(--halo-${up})`, 'stroke-opacity': 0.85 });
      add(s, 'path', { d: 'M19,22A14,14 0 0 0 47,22Z', fill: `var(--halo-${dn})`, 'fill-opacity': 0.3, stroke: `var(--halo-${dn})`, 'stroke-opacity': 0.85 });
      add(s, 'circle', { cx: 33, cy: 22, r: 8, fill: 'var(--node)', stroke: 'var(--node-line)' });
      row(s, goods() ? '<b>Halo:</b> above, bought from the rest of the UK; below, sold to it. Areas are on the circle’s scale.' : '<b>Halo:</b> above, received from the rest of the UK; below, paid to it. Areas are on the circle’s scale.'); }
    if (S.layout === 'order' && isTies(S.cols)) { const what = {
        ties: 'from the industry least tied to the highlighted industries to the one most tied. The measure is the share of an industry’s payments here, both paid and received, that are with the highlighted industries',
        sells: 'from the industry that sells least to the highlighted industries to the one that sells most. The measure is the share of what an industry is paid by other industries here that comes from the highlighted ones',
        buys: 'from the industry that buys least from the highlighted industries to the one that buys most. The measure is the share of what an industry pays to other industries here that goes to the highlighted ones' }[S.cols];
      key.appendChild(el('p', 'd-small', S.lens === 'none' ? 'Columns: choose a highlight, and the industries are put in order of how much of their payments here are with it.'
        : `Columns: left to right, ${what}, over the whole period. The percentages along the top are that share. Highlighted industries are placed the same way, by their payments with each other. Industries too small to place are kept apart on the right.`)); }
    if (S.layout === 'order' && isInside(S.cols)) { const what = {
        inside: 'from the industry that trades least inside the region to the one that trades most. The measure is the share of an industry’s payments with industries anywhere in the UK, both paid and received, that are with industries in this region',
        sellsinside: 'from the industry that sells least inside the region to the one that sells most. The measure is the share of what an industry is paid by industries anywhere in the UK that comes from industries in this region',
        buysinside: 'from the industry that buys least inside the region to the one that buys most. The measure is the share of what an industry pays to industries anywhere in the UK that goes to industries in this region' }[S.cols];
      key.appendChild(el('p', 'd-small', `Columns: left to right, ${what}, over the whole period. The percentages along the top are that share. Payments within the industry count. Households, payers whose industry isn’t recorded and public administration are on neither side of it. Industries too small to place are kept apart on the right.`)); }
    if (S.layout === 'order' && S.cols === 'passed') key.appendChild(el('p', 'd-small', 'Columns: left to right, from the industry that passes least along to the one that passes most. '
      + (goods() ? 'The measure is the share of an industry’s trade with other industries here that is goods and services bought from one industry and sold on to another, over the whole period, measured by what was paid for them. It is what is left once two-way trade with the same partner, and any excess of selling over buying or the reverse, are taken off. An industry that sells a great deal here but buys little here passes little along: what it sells came from outside the region.'
        : 'The measure is the share of an industry’s payments with other industries here that is money paid to it by one industry and paid on to another, over the whole period. It is what is left once two-way payments with the same partner, and any excess of paying over being paid or the reverse, are taken off. Read as goods and services it is the same figure.')
      + ' The percentages along the top are that share. Industries near the average of the order pass most along, so this partly repeats height. Industries too small to place are kept apart on the right.'));
    if (S.trace) key.appendChild(el('p', 'd-small', 'Trace: with an industry pinned, the strong links are its chain over the whole period. '
      + (goods() ? 'Upwards it follows the industries that supply a tenth or more of what it buys net, and then their suppliers; downwards the industries that take a tenth or more of what it sells net, and then their customers.'
        : 'Upwards it follows the industries whose net payments make up a tenth or more of what it is paid net, and then theirs; downwards the industries that take a tenth or more of what it pays net, and then theirs.')
      + ` Second-step links are fainter, and other industries are greyed. A trail stops where an industry’s ${goods() ? 'customers' : 'payers'} aren’t in the data.`));
    if (S.layout === 'order' && S.cols === 'shift') key.appendChild(el('p', 'd-small', 'Columns: left to right, from the industry whose place in the order shifts least to the one whose place shifts most. The measure is the standard deviation of its level, worked out on the 12 months to each month; the numbers along the top are that figure, in levels. Industries too small to place are kept apart on the right.'));
    if (goods()) { const note = el('p', 'd-small', 'Goods and services are an inference: each payment turned round. Nothing in the data says what was bought. Services such as rent and insurance are in here with goods, and so are payments that bought nothing, such as taxes and dividends.'); key.appendChild(note); }
  }

  // ---- detail panel for the pinned industry
  function partners(i, asPayer, n = 5) {                   // the industries i pays most (asPayer) or is paid most by
    const out = [];
    for (let k = 0; k < NL; k++) if (lact[k] && curV[k] > 0 && (asPayer ? buy[k] : sup[k]) === i) out.push({ name: nodes[asPayer ? sup[k] : buy[k]].short, v: curV[k], n: curN[k], held: !S.pubonly && basis[k][Math.round(S.t)] === 'h' });
    return out.sort((a, b) => b.v - a.v).slice(0, n);
  }
  let partnerTable = false;                                // the side panel's partners: a chart, or the two lists behind "Show table"
  function renderDetail() {
    const box = $('detail'); box.replaceChildren();
    if (S.sel < 0) { box.append(el('h2', null, 'Pick an industry'), el('p', 'd-hint', 'Click a circle to pin it. Its payments, partners and place in the order appear here. Hover over a circle or a link for its figures.')); return; }
    const d = nodes[S.sel], m = Math.round(S.t), i = d.i;
    const close = el('button', 'd-close', 'Close'); close.type = 'button'; close.addEventListener('click', () => select(-1));
    box.append(close, el('h2', null, d.short), el('p', 'd-sub', `SIC ${d.id}: ${d.name}`));
    const dl = el('dl'); const item = (k, v) => { dl.append(el('dt', null, k), el('dd', null, v)); };
    item('Sector group', `${d.group} (section ${d.section})`);
    item('Payment group', d.role === 'intermediary' ? 'Not grouped (intermediary)' : d.pay_status === 'core' ? d.pay_group
      : d.pay_status === 'too small' ? 'Too small to place' : d.pay_group_loose ? `None firmly. Most often with ${d.pay_group_loose} (${d.pay_group_years} of 8 years)` : 'None firmly');
    if (d.fe) { const z = [['material', d.fe[0]], ['providential', d.fe[1]], ['overlooked', d.fe[2]]].filter(e => e[1] >= 0.005), cap = t => t[0].toUpperCase() + t.slice(1);
      item('Foundational economy', !z.length ? 'Not in it' : z.length === 1 && z[0][1] >= 0.995 ? cap(z[0][0]) : cap(z.map(e => `${e[0]} ${fmtPct(e[1])}`).join(', ')) + ' of its jobs'); }
    item('Industrial strategy', d.is8.length ? d.is8.map(e => e.name + (e.whole ? '' : ` (${fmtPct(e.share)} of its jobs)`)).join('; ') : 'Not on the list');
    box.append(dl);

    // where its money goes and comes from: inside the region, London, the rest of the UK. Inside includes payments between
    // firms in the industry itself (which no link shows), because the two outside figures include the same industry elsewhere.
    box.append(el('h3', null, `${periodText(m)}, a month`));
    const bar = (label, inside, lon, rest) => { const tot = inside + lon + rest || 1, r = el('div');
      const head = el('div', 'd-row'); head.append(el('span', null, label), el('span', 'v', fmtMoney(inside + lon + rest))); r.append(head);
      const b = el('div', 'bar3'); b.setAttribute('role', 'img'); b.setAttribute('aria-label', `${label}: ${fmtMoney(inside)} inside the region, ${fmtMoney(lon)} ${D.facts.partner}, ${fmtMoney(rest)} rest of the UK`);
      for (const [cls, v] of [['s-inside', inside], ['s-london', lon], ['s-rest', rest]]) { if (v <= 0) continue; const sgm = el('span', cls); sgm.style.flex = `${v / tot} 1 0`; b.append(sgm); }
      r.append(b, el('div', 'd-small', `${fmtMoney(inside)} inside (${fmtPct(inside / tot)}), ${fmtMoney(lon)} ${D.facts.partner}, ${fmtMoney(rest)} elsewhere`)); return r; };
    const self = xAt(d, 'self', m), bl = xAt(d, 'buyLon', m), sl = xAt(d, 'sellLon', m);
    box.append(bar(goods() ? 'It buys' : 'It pays', paidV[i] + self, bl, xAt(d, 'buy', m) - bl), bar(goods() ? 'It sells' : 'It is paid', recvV[i] + self, sl, xAt(d, 'sell', m) - sl));
    const keyRow = el('div', 'bar3-key'); for (const [c, tx] of [['var(--seg-inside)', 'Industries here'], ['var(--seg-london)', D.facts.partner], ['var(--seg-rest)', 'Rest of the UK']]) { const sp = el('span'); const sw = el('i'); sw.style.background = c; sp.append(sw, document.createTextNode(tx)); keyRow.append(sp); }
    box.append(keyRow, el('p', 'd-small', `The inside figures include ${fmtMoney(self)} a month that firms in this industry ${goods() ? 'buy from' : 'pay'} each other, which no link shows.`));
    { const it = insides(), r = it.rank[i];
      box.append(el('p', 'd-small', `Over the whole period, ${fmtPct(insides('buysinside').sd[i])} of what it ${goods() ? 'buys' : 'pays'} and ${fmtPct(insides('sellsinside').sd[i])} of what it ${goods() ? 'sells' : 'is paid'} is inside the region: ${fmtPct(it.sd[i])} of the two together${r < 0 ? '' : `, ${r + 1} of ${it.n} counting from the least`}.`)); }

    // Its main partners here. A chart by default: for the eight industries most passes between, what it pays each (left)
    // and what each pays it (right), back to back, ordered as the chart is (buyers above in the money view, suppliers
    // above in the goods view). "Show table" swaps it for the two lists, which also give the number of payments.
    { const g = goods(), count = n => (n >= 50 ? d3.format(',')(Math.round(n / 100) * 100) : 'under 50') + ' payments';
      const head = el('h3', 'd-headrow'); head.append(document.createTextNode('Its main partners here'));
      const tog = el('button', 'd-toggle', partnerTable ? 'Show chart' : 'Show table'); tog.type = 'button'; tog.addEventListener('click', () => { partnerTable = !partnerTable; renderDetail(); });
      head.append(tog); box.append(head);
      if (partnerTable) for (const [title, asPayer] of [[g ? 'It buys most from' : 'It pays most to', true], [g ? 'It sells most to' : 'It is paid most by', false]]) {
        box.append(el('p', 'd-chainhead', title)); const list = partners(i, asPayer);
        if (!list.length) box.append(el('p', 'd-small', 'Nothing published.'));
        for (const p of list) { const r = el('div', 'd-row'); r.append(el('span', 'n', p.name + (p.held ? ' (held)' : '')), el('span', 'v', `${fmtMoney(p.v)}, ${count(p.n)}`)); box.append(r); } }
      else { const by = new Map(), get = j => by.get(j) || by.set(j, { name: nodes[j].short, r: 0, p: 0, rn: 0, pn: 0, rh: false, ph: false }).get(j);
        for (let k = 0; k < NL; k++) { if (!lact[k] || !(curV[k] > 0)) continue; const held = !S.pubonly && basis[k][m] === 'h';
          if (sup[k] === i) { const o = get(buy[k]); o.r = curV[k]; o.rn = curN[k]; o.rh = held; }               // that industry pays it
          else if (buy[k] === i) { const o = get(sup[k]); o.p = curV[k]; o.pn = curN[k]; o.ph = held; } }        // it pays that industry
        const rows = [...by.values()].sort((a, b) => (b.r + b.p) - (a.r + a.p)).slice(0, 8).sort((a, b) => (g ? 1 : -1) * ((a.r - a.p) - (b.r - b.p)));
        if (!rows.length) box.append(el('p', 'd-small', 'Nothing published.'));
        else { const w = 290, cx = w / 2, half = cx - 48, rh = 31, top = 20, hgt = top + rows.length * rh, mx = d3.max(rows, o => Math.max(o.r, o.p)), len = v => Math.max(v > 0 ? 1 : 0, half * v / mx);
          const left = g ? 'it buys from them' : 'it pays them', right = g ? 'it sells to them' : 'they pay it';
          const svg = d3.create('svg').attr('class', 'pchart').attr('viewBox', `0 0 ${w} ${hgt}`).attr('role', 'img')
            .attr('aria-label', `What ${d.short} ${g ? 'buys from and sells to' : 'pays to and is paid by'} its eight largest partners here. ` + rows.map(o => `${o.name}: ${g ? 'buys' : 'pays'} ${fmtMoney(o.p)}, ${g ? 'sells' : 'is paid'} ${fmtMoney(o.r)}`).join('; '));
          svg.append('text').attr('x', cx - 8).attr('y', 10).attr('text-anchor', 'end').text('\u2190 ' + left); svg.append('text').attr('x', cx + 8).attr('y', 10).text(right + ' \u2192');
          const bar = (x0, x1, y, out) => { const r = Math.min(3, Math.abs(x1 - x0) / 2), sg = out > 0 ? 1 : -1;      // square at the centre line, rounded at the far end
            return `M${x0},${y}H${x1 - sg * r}Q${x1},${y} ${x1},${y + r}V${y + 9 - r}Q${x1},${y + 9} ${x1 - sg * r},${y + 9}H${x0}Z`; };
          rows.forEach((o, n) => { const y = top + n * rh;
            svg.append('text').attr('class', 'pc-name').attr('x', cx).attr('y', y + 10).attr('text-anchor', 'middle').text(o.name);
            for (const [v, cnt, held, sg, what] of [[o.p, o.pn, o.ph, -1, g ? 'It buys from' : 'It pays'], [o.r, o.rn, o.rh, 1, g ? 'It sells to' : 'It is paid by']]) { if (!(v > 0)) continue; const x1 = cx + sg * len(v);
              svg.append('path').attr('class', 'pc-bar' + (held ? ' held' : '')).attr('d', bar(cx, x1, y + 15, sg)).append('title').text(`${what} ${o.name}: ${fmtMoney(v)} a month, ${count(cnt)}${held ? '. Hidden this month: held at an average' : ''}`);
              svg.append('text').attr('class', 'pc-val').attr('x', x1 + sg * 4).attr('y', y + 23).attr('text-anchor', sg < 0 ? 'end' : 'start').text(fmtMoney(v)); }
            svg.append('line').attr('class', 'pc-gap').attr('x1', cx).attr('x2', cx).attr('y1', y + 14).attr('y2', y + 25);       // a gap in the page colour parts the two bars at the centre
            svg.append('line').attr('class', 'pc-axis').attr('x1', cx).attr('x2', cx).attr('y1', y + 12).attr('y2', y + 14.5); svg.append('line').attr('class', 'pc-axis').attr('x1', cx).attr('x2', cx).attr('y1', y + 24.5).attr('y2', y + 27); });
          box.append(svg.node(), el('p', 'd-small', `£ a month, ${lowerFirst(periodText(m))}: the ${rows.length === 8 ? 'eight ' : ''}industries here ${g ? 'it trades most with' : 'with the largest payments to and from it'}${rows.some(o => o.rh || o.ph) ? '. Dashed bars are hidden by ONS this month and shown at an average' : ''}.`)); } } }

    // its place in the order over time
    const roll = S.inter ? d.level_all_rolling : d.level_rolling, lvl = L(d);
    box.append(el('h3', null, 'Place in the order, 12 months to each month'));
    const w = 290, h = 64, sx = d3.scaleLinear().domain([0, NM - 1]).range([2, w - 44]), sy = d3.scaleLinear().domain(lev).range(goods() ? [6, h - 6] : [h - 6, 6]).clamp(true);   // the same way up as the chart
    const svg = d3.create('svg').attr('id', 'levelspark').attr('viewBox', `0 0 ${w} ${h}`).attr('role', 'img').attr('aria-label', 'Its level in the supplier-to-buyer order over time');
    svg.append('line').attr('x1', 2).attr('x2', w - 44).attr('y1', sy(0)).attr('y2', sy(0)).attr('stroke', 'var(--muted)').attr('stroke-width', 1.25);
    svg.append('text').attr('x', w - 40).attr('y', sy(0) + 3).attr('font-size', 10).attr('fill', 'var(--muted)').text('average');
    svg.append('text').attr('x', w - 40).attr('y', 10).attr('font-size', 10).attr('fill', 'var(--muted)').text(goods() ? 'supplier' : 'buyer');
    svg.append('text').attr('x', w - 40).attr('y', h - 3).attr('font-size', 10).attr('fill', 'var(--muted)').text(goods() ? 'buyer' : 'supplier');
    svg.append('path').attr('fill', 'none').attr('stroke', 'var(--ink2)').attr('stroke-width', 2).attr('stroke-linejoin', 'round')
      .attr('d', d3.line().defined(v => v != null).x((_, j) => sx(j)).y(v => sy(v))(roll));
    if (roll[m] != null) svg.append('circle').attr('cx', sx(m)).attr('cy', sy(roll[m])).attr('r', 4).attr('fill', 'var(--ink)').attr('stroke', 'var(--surface)').attr('stroke-width', 2);
    box.append(svg.node(), el('p', 'd-small', `Over the whole period it sits ${Math.abs(lvl) < 0.05 ? 'at about the average' : `${Math.abs(lvl).toFixed(2)} levels to the ${lvl > 0 ? 'buyer' : 'supplier'} side of the average industry`}${d.small ? ', but it is too small for that to be reliable' : ''}.`));
    if (S.lens !== 'none') { const tt = ties(), r = tt.rank[i];
      box.append(el('p', 'd-small', `${fmtPct(tt.sd[i])} of its payments to and from other industries here, over the whole period, are with the highlighted industries${r < 0 ? '' : `: ${r + 1} of ${tt.n} counting from the least`}. `
        + `Of what it sells to other industries here, ${fmtPct(ties('sells').sd[i])} goes to the highlighted ones; of what it buys, ${fmtPct(ties('buys').sd[i])} comes from them.`)); }
    { const st = shifts(), r = st.rank[i];
      box.append(el('p', 'd-small', r < 0 ? `Its level moves about by ${st.sd[i].toFixed(3)} (standard deviation), but that is mostly noise in so small an industry.`
        : `Its place shifts ${r < st.n / 3 ? 'less than most' : r >= 2 * st.n / 3 ? 'more than most' : 'about as much as most'}: a standard deviation of ${st.sd[i].toFixed(3)} levels, ${r + 1} of ${st.n} counting from the steadiest.`)); }

    // chain or two-way: how its payments with other industries here split, and which way they run
    { const ch = chains(), pm = v => fmtMoney(v / NM), buyerEnd = !!ch.buyerEnd[i];
      const parts = [['s-inside', ch.twoWay[i], 'Two-way'], ['s-london', ch.passed[i], 'Passed along'], ['s-rest', ch.end[i], buyerEnd ? 'Buyer end' : 'Supplier end']];
      box.append(el('h3', null, 'Chain or two-way trade, whole period'));
      const b = el('div', 'bar3'); b.setAttribute('role', 'img'); b.setAttribute('aria-label', parts.map(e => `${e[2]} ${fmtPct(e[1])}`).join(', '));
      for (const [cls, v] of parts) { if (v <= 0) continue; const sgm = el('span', cls); sgm.style.flex = `${v} 1 0`; b.append(sgm); }
      const kr = el('div', 'bar3-key'); for (const [cls, v, tx] of parts) { const sp = el('span'), sw = el('i'); sw.style.background = `var(--seg-${cls.slice(2)})`; sp.append(sw, document.createTextNode(`${tx} ${fmtPct(v)}`)); kr.append(sp); }
      const gd = goods();                                   // the same figures, worded as goods and services when that is what the chart shows
      box.append(b, kr, el('p', 'd-small', gd
        ? `Of its trade with other industries here, by what was paid, ${fmtPct(ch.twoWay[i])} is two-way with the same partner and ${fmtPct(ch.passed[i])} is goods and services bought from one industry and sold on to another. `
          + `The other ${fmtPct(ch.end[i])} is where it ${buyerEnd ? 'buys more here than it sells here, so goods and services leave the picture here' : 'sells more here than it buys here, so goods and services enter the picture here'}.`
        : `Of its payments with other industries here, ${fmtPct(ch.twoWay[i])} are two-way with the same partner and ${fmtPct(ch.passed[i])} are money paid to it by one industry and paid on to another. `
          + `The other ${fmtPct(ch.end[i])} is where it ${buyerEnd ? 'pays out more than it is paid, so money enters the picture here' : 'is paid more than it pays out, so money leaves the picture here'}.`));
      box.append(el('p', 'd-small', d.small ? `It is too small for its place in the order to say which way its ${gd ? 'trade runs' : 'payments run'}.`
        : `With one net link a pair, ${fmtPct(ch.netDown[i])} of its ${gd ? 'trade runs down the page' : 'payments run down the order'}; with both directions counted, ${fmtPct(ch.grossDown[i])}. For the middle industry here those are ${fmtPct(ch.midNetDown)} and ${fmtPct(ch.midGrossDown)}.`));
      if (S.trace) { const tr = traceOf(i), g = goods();
        box.append(el('h3', null, 'Its chain, whole period'));
        const verb = up => up ? (g ? 'sells' : 'is paid') : (g ? 'buys' : 'pays');
        const tree = (items, up, parent) => { const ul = el('ul', 'd-chain'); for (const it of items) { const li = el('li'), name = nodes[it.i].short;
            li.append(el('span', 'n', name), document.createTextNode(` ${pm(it.v)} a month net, ${fmtPct(it.share)} of what ${parent || 'it'} ${verb(up)} net`));
            if (it.next.length) { li.append(el('div', 'd-small', up ? (g ? 'which sells to' : 'which is paid by') : (g ? 'which buys from' : 'which pays'))); li.append(tree(it.next, up, name)); } ul.append(li); } return ul; };
        for (const [title, items, up] of [[g ? 'It sells to' : 'It is paid by', tr.up, true], [g ? 'It buys from' : 'It pays', tr.down, false]]) {
          box.append(el('p', 'd-chainhead', title)); if (items.length) box.append(tree(items, up, '')); else box.append(el('p', 'd-small', 'No single industry makes up a tenth of it.')); }
        box.append(el('p', 'd-small', g ? 'Each step follows net sales worth a tenth or more of what the industry sells net (downwards on the chart) or buys net (upwards), two steps each way. A trail stops where an industry’s customers aren’t in the data, as with public services and shops.'
          : 'Each step follows net payments worth a tenth or more of what the industry is paid net (upwards on the chart) or pays net (downwards), two steps each way. A trail stops where an industry’s payers aren’t in the data, as with public services and shops.')); } }

    // what is hidden
    const never = S.inter ? d.never_all : d.never_core, cells = S.inter ? d.cells_all : d.cells_core;
    let held = 0, all = 0; for (let k = 0; k < NL; k++) if (lact[k] && (sup[k] === i || buy[k] === i)) { all += curV[k]; if (basis[k][m] === 'h') held += curV[k]; }
    box.append(el('h3', null, 'What is hidden'), el('p', 'd-small', `${never} of its ${cells} links with other industries here were never published, so they are missing. ${S.pubonly ? 'Hidden months are switched off.' : `${fmtPct(all ? held / all : 0)} of the value shown for it this month is held at an average.`}`));
  }

  // ---- table view
  function tableRows() {
    const m = Math.round(S.t), tot = monthTotals('value', S.smooth, S.pubonly).byMonth[m] || 1;
    return drawn.slice().sort((i, j) => curV[j] - curV[i]).map(k => ({ payer: nodes[buy[k]].short, payee: nodes[sup[k]].short, paid: curV[k], share: curV[k] / tot, payments: curN[k],
      back: rev[k] >= 0 ? curV[rev[k]] : 0, status: basis[k][m] === 'p' ? 'Published' : basis[k][m] === 'h' ? (S.pubonly ? 'Hidden' : 'Held at an average') : 'No payments recorded' }));
  }
  function renderTable() {
    const m = Math.round(S.t), rows = tableRows(), head = $('linktable').tHead, body = $('linktable').tBodies[0];
    $('tablecap').textContent = `${rows.length} links, ${lowerFirst(periodText(m))}, a month. Amounts are what the payer paid; the other direction is in its own column.${goods() ? ' The chart draws each payment the other way round, as goods and services.' : ''}${S.dir === 'both' ? '' : ` Only links running ${S.dir} the order are listed.`}`;
    const cols = [['Payer', ''], ['Paid to', ''], ['Amount', 'num'], ['Share of the month', 'num'], ['Payments', 'num'], ['Average payment', 'num'], ['Paid the other way', 'num'], ['Data', '']];
    const hr = el('tr'); for (const [c, cls] of cols) hr.append(el('th', cls, c)); head.replaceChildren(hr);
    const frag = document.createDocumentFragment();
    for (const r of rows) { const tr = el('tr');
      tr.append(el('td', '', r.payer), el('td', '', r.payee), el('td', 'num', fmtMoney(r.paid)), el('td', 'num', fmtPct(r.share)), el('td', 'num', r.payments >= 50 ? d3.format(',')(Math.round(r.payments / 100) * 100) : 'under 50'),
        el('td', 'num', fmtAvg(r.paid, r.payments)), el('td', 'num', fmtMoney(r.back)), el('td', '', r.status)); frag.append(tr); }
    body.replaceChildren(frag);
  }
  function downloadCsv() {
    const m = Math.round(S.t), esc = v => '"' + String(v).replace(/"/g, '""') + '"';
    const lines = [['payer', 'paid_to', 'amount_gbp', 'share_of_month', 'payments', 'paid_the_other_way_gbp', 'data', 'period'].join(',')];
    for (const r of tableRows()) lines.push([esc(r.payer), esc(r.payee), Math.round(r.paid * 1000), r.share.toFixed(5), Math.round(r.payments), Math.round(r.back * 1000), esc(r.status), esc(periodText(m))].join(','));
    const a = el('a'); a.href = URL.createObjectURL(new Blob([lines.join('\n') + '\n'], { type: 'text/csv' })); a.download = `ynh_payment_links_${D.months[m]}.csv`; a.click(); setTimeout(() => URL.revokeObjectURL(a.href), 2000);
  }

  // ---------------------------------------------------------------- pointer: hover for figures, click to pin
  const tip = $('tip');
  function hit(x, y) {                                     // nearest industry, else nearest drawn link
    let bn = -1, bd = 1e9;
    for (const d of nodes) { if (!act[d.i]) continue; const dd = Math.hypot(pos[d.i].x - x, pos[d.i].y - y), reach = Math.max(12, radNow[d.i] + 4); if (dd < reach && dd - radNow[d.i] < bd) { bd = dd - radNow[d.i]; bn = d.i; } }
    if (bn >= 0) return { node: bn, link: -1 };
    let bl = -1; bd = 1e9;
    for (const k of drawn) { const o = k * (NSEG + 1) * 4, reach = Math.max(4, widthNow[k] / 2 + 2);
      for (let i = 0; i < NSEG; i++) { const p = o + i * 4, x1 = geo[p], y1 = geo[p + 1], dx = geo[p + 4] - x1, dy = geo[p + 5] - y1, l2 = dx * dx + dy * dy || 1;
        const u = Math.max(0, Math.min(1, ((x - x1) * dx + (y - y1) * dy) / l2)), dd = Math.hypot(x - x1 - u * dx, y - y1 - u * dy);
        if (dd < reach && dd < bd) { bd = dd; bl = k; } } }
    return { node: -1, link: bl };
  }
  function showTip(x, y) {
    tip.replaceChildren();
    const m = Math.round(S.t), tot = monthTotals('value', S.smooth, S.pubonly).byMonth[m] || 1;
    if (hoverNode >= 0) { const d = nodes[hoverNode];
      tip.append(el('div', 't-title', d.short), el('div', 't-sub', `SIC ${d.id}, ${d.group}`));
      const r1 = el('div'); r1.append(document.createTextNode(goods() ? 'Buys from other industries here ' : 'Pays other industries here '), el('span', 't-val', fmtMoney(paidV[d.i]))); const r2 = el('div'); r2.append(document.createTextNode(goods() ? 'Sells to them ' : 'Is paid by them '), el('span', 't-val', fmtMoney(recvV[d.i])));
      tip.append(r1, r2, el('div', 't-note', d.small ? 'Too small to place reliably. Click to pin.' : 'Click to pin and see more.'));
    } else if (hoverLink >= 0) { const k = hoverLink, r = rev[k], payer = nodes[buy[k]].short, payee = nodes[sup[k]].short;
      const said = (x, y) => goods() ? `${y} supplies ${x}` : `${x} pays ${y}`;       // x pays y, or the same thing turned round
      tip.append(el('div', 't-title', said(payer, payee)));
      const r1 = el('div'); r1.append(el('span', 't-val', fmtMoney(curV[k])), document.createTextNode(` a month${goods() ? ' paid for it' : ''}, ${fmtPct(curV[k] / tot)} of the month`)); tip.append(r1);
      tip.append(el('div', null, `${fmtCount(curN[k])} payments${curN[k] >= 50 ? ', averaging ' + fmtAvg(curV[k], curN[k]) : ''}`));
      if (r >= 0 && lact[r] && curV[r] > 0) { tip.append(el('div', 't-sub', `${said(payee, payer)} ${fmtMoney(curV[r])} (${fmtCount(curN[r])} payments)`));
        if (S.net === 'net') tip.append(el('div', 't-sub', `The link shows the difference: ${fmtMoney(curV[k] - curV[r])}`)); }
      const np = (basis[k].match(/p/g) || []).length;
      tip.append(el('div', 't-note', basis[k][m] === 'h' && !S.pubonly ? `Hidden this month: shown at the average of its published months. It was published in ${np} of ${NM}.` : basis[k][m] === 'p' ? `Published this month, and in ${np} of ${NM} months in all.` : ''));
    } else { tip.style.display = 'none'; return; }
    tip.style.display = 'block';
    const tw = tip.offsetWidth, th = tip.offsetHeight;
    tip.style.left = Math.max(4, Math.min(W - tw - 4, x + 14)) + 'px'; tip.style.top = Math.max(4, Math.min(H - th - 4, y + 16)) + 'px';
  }
  function pointer(e) {
    if (!pos || tween) return;
    const b = canvas.getBoundingClientRect(), x = e.clientX - b.left, y = e.clientY - b.top, h = hit(x, y);
    if (h.node !== hoverNode || h.link !== hoverLink) { hoverNode = h.node; hoverLink = h.link; canvas.style.cursor = h.node >= 0 ? 'pointer' : 'default'; requestDraw(); }
    showTip(x, y);
  }
  canvas.addEventListener('pointermove', pointer);
  canvas.addEventListener('pointerleave', () => { hoverNode = hoverLink = -1; tip.style.display = 'none'; requestDraw(); });
  canvas.addEventListener('click', e => { const b = canvas.getBoundingClientRect(), h = hit(e.clientX - b.left, e.clientY - b.top); select(h.node >= 0 ? (h.node === S.sel ? -1 : h.node) : h.link >= 0 ? S.sel : -1); });
  function select(i) { S.sel = i >= 0 && act[i] ? i : -1; pickSel.value = S.sel >= 0 ? nodes[S.sel].id : ''; renderDetail(); requestDraw(); syncUrl(); }

  // ---------------------------------------------------------------- controls
  const lens = $('lens'), pickSel = $('pick');
  const optgroup = (label, opts) => { const g = el('optgroup'); g.label = label; for (const [v, t] of opts) { const o = el('option', null, t); o.value = v; g.append(o); } return g; };
  function buildOptions() {                               // the highlight sets and the list of industries, which differ by region
    const none = el('option', null, 'None'); none.value = 'none';
    lens.replaceChildren(none, optgroup('Sector group', GROUPS.map(g => ['group:' + g, g])));
    if (D.pay_groups.length) lens.append(optgroup('Payment group', D.pay_groups.map(g => ['pay:' + g, g])));
    if (D.is8_names.length) lens.append(optgroup('Industrial strategy', [['is8:any', 'Any industrial strategy sector'], ...D.is8_names.map(g => ['is8:' + g, g])]));   // none for Northern Ireland: no jobs data
    if (nodes.some(d => d.fe)) lens.append(optgroup('Foundational economy', [['fe:both', 'Material and providential'], ['fe:material', 'Material: utilities, food, transport'],
      ['fe:providential', 'Providential: health, education, care'], ['fe:overlooked', 'Overlooked: occasional everyday buys']]));
    if (![...lens.options].some(o => o.value === S.lens)) S.lens = 'none';
    const no = el('option', null, 'None'); no.value = ''; pickSel.replaceChildren(no);
    for (const d of nodes.slice().sort((a, b) => a.short.localeCompare(b.short))) { const o = el('option', null, d.short); o.value = d.id; pickSel.append(o); }
  }
  pickSel.addEventListener('change', e => select(nodes.findIndex(n => n.id === e.target.value)));
  const selects = ['cols', 'lens', 'show', 'measure', 'scale', 'net', 'cover', 'speed'], checks = ['smooth', 'dots', 'inter', 'halo', 'pubonly', 'trace'];
  function wording() {                                     // the fixed text that depends on money or goods
    const g = goods();
    $('l-inside').textContent = g ? 'Bought and sold inside the region' : 'Paid inside the region';
    $('l-out').textContent = g ? 'Bought from the rest of the UK' : 'Paid to the rest of the UK';
    $('l-in').textContent = g ? 'Sold to the rest of the UK' : 'Received from the rest of the UK';
    $('lede-order').textContent = g ? 'Here each payment is turned round and drawn as the goods or services it paid for: suppliers sit higher and buyers lower, so these mostly run down the page.'
      : 'Industries that mostly buy sit higher and those that mostly supply sit lower, so money mostly runs down the page.';
  }
  function reflect() {                                     // put the state into the controls
    for (const k of selects) $(k).value = S[k];
    for (const k of checks) $(k).checked = S[k];
    document.querySelectorAll('input[name=layout]').forEach(r => { r.checked = r.value === S.layout; });
    document.querySelectorAll('input[name=dir]').forEach(r => { r.checked = r.value === S.dir; });
    $('cols').disabled = S.layout !== 'order'; $('halo').disabled = S.measure !== 'value';
    for (const o of pickSel.options) { const n = nodes.find(d => d.id === o.value); o.hidden = o.disabled = !!n && !act[n.i]; }
    pickSel.value = S.sel >= 0 ? nodes[S.sel].id : '';
    $('play').textContent = playing ? 'Pause' : 'Play';
  }
  function changed(k) {                                    // what each control has to refresh
    if (!D) return;
    if (k === 'inter') { setActive(); for (const key in totalsCache) delete totalsCache[key]; updateStripMax(); buildSpark(); moveTo(false); }
    if (k === 'layout' || k === 'cols' || (k === 'lens' && isTies(S.cols))) moveTo(false);
    if (k === 'show') { geoDirty = true; moveTo(false); wording(); }
    if (['measure', 'scale', 'net', 'halo', 'inter', 'smooth', 'pubonly', 'show', 'layout', 'cols', 'lens', 'trace'].includes(k)) buildKey();
    if (k === 'inter') fillChainFacts();
    if (k === 'smooth') updateStripMax();
    reflect(); refresh(); syncUrl();
  }
  for (const k of selects) $(k).addEventListener('change', e => { S[k] = e.target.value; changed(k); });
  for (const k of checks) $(k).addEventListener('change', e => { S[k] = e.target.checked; changed(k); });
  document.querySelectorAll('input[name=layout]').forEach(r => r.addEventListener('change', () => { if (r.checked) { S.layout = r.value; changed('layout'); } }));
  document.querySelectorAll('input[name=dir]').forEach(r => r.addEventListener('change', () => { if (r.checked) { S.dir = r.value; changed('dir'); } }));

  // ---- time: step, scrub, play
  const slider = $('month');
  function setMonth(m) { S.t = Math.max(0, Math.min(NM - 1, m)); refresh(); syncUrl(); }
  function setPlaying(on) { playing = on; if (on && S.t >= NM - 1) S.t = 0; $('play').textContent = on ? 'Pause' : 'Play'; if (on) schedule(); else { S.t = Math.round(S.t); refresh(); syncUrl(); } }
  slider.addEventListener('input', e => { if (playing) setPlaying(false); setMonth(+e.target.value); });
  $('prev').addEventListener('click', () => { if (playing) setPlaying(false); setMonth(Math.round(S.t) - 1); });
  $('next').addEventListener('click', () => { if (playing) setPlaying(false); setMonth(Math.round(S.t) + 1); });
  $('play').addEventListener('click', () => setPlaying(!playing));
  document.addEventListener('keydown', e => {
    const tag = (e.target.tagName || '').toLowerCase(); if (e.metaKey || e.ctrlKey || e.altKey || tag === 'select' || tag === 'textarea' || (tag === 'input' && e.target.type !== 'range' && e.target.type !== 'radio' && e.target.type !== 'checkbox')) return;
    if (e.key === ' ' && tag !== 'button' && tag !== 'summary' && tag !== 'input') { e.preventDefault(); setPlaying(!playing); }
    else if ((e.key === 'ArrowLeft' || e.key === 'ArrowRight') && tag !== 'input') { e.preventDefault(); if (playing) setPlaying(false); setMonth(Math.round(S.t) + (e.key === 'ArrowLeft' ? -1 : 1)); }
    else if (e.key === 'Escape' && S.sel >= 0) select(-1);
  });
  $('tablebox').addEventListener('toggle', () => { if ($('tablebox').open) renderTable(); });
  $('csv').addEventListener('click', downloadCsv);
  const openNote = () => { if (location.hash === '#order-note') $('order-note').open = true; };
  window.addEventListener('hashchange', openNote); openNote();
  $('order-link').addEventListener('click', () => { $('order-note').open = true; });

  // ---- keep the address bar in step, so a view can be shared
  let urlTimer = 0;
  function syncUrl() { clearTimeout(urlTimer); urlTimer = setTimeout(() => {
    if (!D) return;
    const p = new URLSearchParams();
    for (const k of ['region', ...selects, 'layout', 'dir']) if (S[k] !== DEFAULTS[k]) p.set(k, S[k]);
    for (const k of checks) if (S[k] !== DEFAULTS[k]) p.set(k, S[k] ? '1' : '0');
    if (Math.round(S.t) !== NM - 1) p.set('m', D.months[Math.round(S.t)]);
    if (S.sel >= 0) p.set('sel', nodes[S.sel].id);
    try { history.replaceState(null, '', p.toString() ? '?' + p : location.pathname); } catch (err) { /* opened from disk in a browser that forbids it */ }
  }, 250); }

  // ---------------------------------------------------------------- start
  function relayout() { if (!D) return; resize(); for (const k in layouts) delete layouts[k]; pos = null; moveTo(true); buildSpark(); buildKey(); refresh(); }

  // ---------------------------------------------------------------- regions
  // Text that names the region or quotes one of its figures: every element with data-f takes that entry of the region's
  // facts, worked out by the build. data-if and data-unless show or hide an element on a true-or-false entry.
  function fillChainFacts() { const all = chains().all; $('chain-passed').textContent = fmtPct(all.passed); $('chain-twoway').textContent = fmtPct(all.twoWay); }
  function fillText() {
    const F = D.facts, R = regionOf(S.region), Q = R.quality;
    document.querySelectorAll('[data-f]').forEach(e => { const v = F[e.dataset.f]; e.textContent = v == null ? '' : v; });
    document.querySelectorAll('[data-if]').forEach(e => { e.hidden = !F[e.dataset.if]; });
    document.querySelectorAll('[data-unless]').forEach(e => { e.hidden = !!F[e.dataset.unless]; });
    $('h-title').textContent = `How industries in ${D.the_name} pay each other`; document.title = `${D.name} payment flows`;
    $('built').textContent = `Data built ${D.built}.`; fillChainFacts();
    const io = IDX.regions.map(r => r.quality.io_cor); $('io-range').textContent = `${d3.min(io).toFixed(2)} to ${d3.max(io).toFixed(2)}`;
    const qEl = $('quality'); qEl.classList.toggle('thin', R.thin);
    qEl.replaceChildren(el('strong', null, R.thin ? `Thin data for ${D.the_name}. ` : 'Data here. '),
      document.createTextNode(`${fmtPct(Q.hidden)} of monthly figures are hidden and ${fmtPct(Q.never)} sit in links never published at all. Held averages add ${fmtPct(Q.held_adds)} to what is published. ` +
        `${Q.small} of ${Q.industries} industries are too small to place. ${fmtPct(Q.no_count)} of the value is in links with under 50 payments a month, which ONS records as none, so those get no payment dots.` +
        (R.thin ? ' Much of what is drawn here is an estimate, and many links are missing: read it with care.' : '')));
    // the region picker and "most like"
    const sel = $('region'); if (!sel.options.length) for (const r of IDX.regions) { const o = el('option', null, r.name + (r.built ? '' : ' (not built yet)')); o.value = r.code; o.disabled = !r.built; sel.append(o); }
    sel.value = S.region;
    const alike = $('alike'); alike.replaceChildren(document.createTextNode('Most like it: '));
    R.distances.slice(0, 2).forEach((d, j) => { if (j) alike.append(document.createTextNode(', ')); alike.append(regionLink(d.code)); });
    alike.append(document.createTextNode('. '), Object.assign(el('a', null, 'How regions compare'), { href: '#compare' }));
  }
  function regionLink(code) {                              // a region's name: a button if its data are built, plain text if not
    const r = regionOf(code); if (!r.built) return el('span', null, r.name);
    const b = el('button', 'linklike', r.name); b.type = 'button'; b.addEventListener('click', () => loadRegion(code)); return b;
  }
  function loadRegion(code) {
    const keepSel = D && S.sel >= 0 ? nodes[S.sel].id : wantSel, keepMonth = D ? D.months[Math.round(S.t)] : wantMonth;
    const arrive = () => {
      if (playing) { playing = false; $('play').textContent = 'Play'; }
      setData(window.FLOWS_DATA[code]); S.region = code;
      S.t = monthFromParam(keepMonth); S.sel = nodes.findIndex(n => n.id === keepSel);
      readTheme(); setActive(); updateStripMax(); fillText(); reflect(); wording(); relayout(); renderCompare();
      wrap.classList.remove('loading'); syncUrl();
    };
    if (window.FLOWS_DATA && window.FLOWS_DATA[code]) { arrive(); return; }
    wrap.classList.add('loading');                         // the old picture stays, dimmed, until the new region arrives
    const sc = document.createElement('script'); sc.src = `data/flows_${code}.js`;
    sc.onload = arrive;
    sc.onerror = () => { wrap.classList.remove('loading'); $('region').value = S.region; $('quality').textContent = `The data for ${regionOf(code).name} could not be loaded.`; };
    document.head.appendChild(sc);
  }
  $('region').addEventListener('change', e => loadRegion(e.target.value));

  // ---- how this region compares with the others (plan section 10.3; figures from data/regions.js)
  function renderCompare() {
    const R = regionOf(S.region), box = $('compare-body'); box.replaceChildren();
    const svgNS = 'http://www.w3.org/2000/svg';
    // A. overall similarity
    box.append(el('h3', null, 'Which regions are most alike'));
    const near = R.distances.slice(0, 2), far = R.distances[R.distances.length - 1], p1 = el('p');
    const The = D.the_name[0].toUpperCase() + D.the_name.slice(1);
    p1.append(document.createTextNode(`${The} is most like `)); near.forEach((d, j) => { if (j) p1.append(document.createTextNode(' and ')); p1.append(regionLink(d.code), document.createTextNode(` (${d.d.toFixed(2)})`)); });
    p1.append(document.createTextNode(`, and least like ${regionOf(far.code).name} (${far.d.toFixed(2)}). The number is the share of one region's payments that would have to move to a different pair of paying and paid industry to match the other: 0 is identical, 1 is nothing in common. It takes in which industries a region has as well as who pays whom, and mostly reflects the first.`));
    box.append(p1);
    const list = el('div', 'cmp-bars'); list.setAttribute('role', 'list');
    for (const d of R.distances) { const row = el('div', 'cmp-row'); row.setAttribute('role', 'listitem');
      const name = el('span', 'cmp-name'); name.append(regionLink(d.code)); const track = el('span', 'cmp-track'), bar = el('span', 'cmp-bar'); bar.style.width = (100 * d.d / 0.7).toFixed(1) + '%'; track.append(bar);
      row.append(name, track, el('span', 'cmp-val', d.d.toFixed(2))); list.append(row); }
    box.append(list, el('p', 'd-small', `Are these differences real? Regions are ${IDX.distance.lo.toFixed(2)} to ${IDX.distance.hi.toFixed(2)} apart. On the same measure a region is only ${IDX.distance.own_lo.toFixed(2)} to ${IDX.distance.own_hi.toFixed(2)} from its own earlier self (2019 to 2022 against 2023 on): ${D.the_name}, ${R.own_shift.toFixed(2)}. So the differences between regions are several times larger than a region's own change over time. Even the closest pair has a third of its payments in different places, so "most alike" is relative.`));

    // B. chain or web
    box.append(el('h3', null, 'Chain or web'));
    box.append(el('p', null, 'Two numbers for how chain-like a region’s payments are. The first is the share of money running down the order: higher means more one-way. The second is how many supplier industries a typical pound’s buyer in effect spreads its spending across: lower means narrower. Each dot is the whole period; the line through it is the range of the seven full years, 2019 to 2025. Where two regions’ lines overlap, they cannot be told apart.'));
    const order = IDX.regions.slice().sort((a, b) => b.chain.down.v - a.chain.down.v);
    const rowH = 20, top = 36, w = 720, h = top + rowH * order.length + 6, x0 = 150, pw = 230, gap = 70;
    const svg = document.createElementNS(svgNS, 'svg'); svg.setAttribute('viewBox', `0 0 ${w} ${h}`); svg.setAttribute('class', 'cmp-chart'); svg.setAttribute('role', 'img');
    svg.setAttribute('aria-label', 'The twelve regions on two measures of how chain-like their payments are. The values are printed beside each row.');
    const add = (tag, attrs, text) => { const e = document.createElementNS(svgNS, tag); for (const a in attrs) e.setAttribute(a, attrs[a]); if (text != null) e.textContent = text; svg.appendChild(e); return e; };
    const panels = [{ key: 'down', title: 'Money running down the order', x: x0, fmt: v => (v * 100).toFixed(0) + '%', note: 'further right: more chain-like' },
                    { key: 'spread', title: 'Supplier industries per buyer, in effect', x: x0 + pw + gap, fmt: v => v.toFixed(1), note: 'further left: more chain-like' }];
    for (const pn of panels) {
      const lo = d3.min(IDX.regions, r => r.chain[pn.key].lo), hi = d3.max(IDX.regions, r => r.chain[pn.key].hi), sx = d3.scaleLinear().domain([lo, hi]).nice().range([pn.x, pn.x + pw]);
      add('text', { x: pn.x, y: 10, class: 'cmp-title' }, pn.title); add('text', { x: pn.x, y: 24, class: 'cmp-tick' }, pn.note);
      for (const t of sx.ticks(4)) { add('line', { x1: sx(t), x2: sx(t), y1: top - 4, y2: h - 4, class: 'cmp-grid' }); add('text', { x: sx(t), y: h + 8, class: 'cmp-tick', 'text-anchor': 'middle' }, pn.fmt(t)); }
      order.forEach((r, i) => { const c = r.chain[pn.key], y = top + rowH * i + rowH / 2, me = r.code === S.region;
        add('line', { x1: sx(c.lo), x2: sx(c.hi), y1: y, y2: y, class: 'cmp-range' + (me ? ' me' : '') }); add('circle', { cx: sx(c.v), cy: y, r: me ? 5 : 4, class: 'cmp-dot' + (me ? ' me' : '') });
        add('text', { x: pn.x + pw + 8, y: y + 3.5, class: 'cmp-num' + (me ? ' me' : '') }, pn.fmt(c.v)); });
    }
    order.forEach((r, i) => add('text', { x: x0 - 10, y: top + rowH * i + rowH / 2 + 3.5, 'text-anchor': 'end', class: 'cmp-label' + (r.code === S.region ? ' me' : '') }, r.short + (r.thin ? ' *' : '')));
    svg.setAttribute('viewBox', `0 0 ${w} ${h + 14}`); box.append(svg);
    // what can and cannot be said about this region
    const overlap = key => IDX.regions.filter(r => r.code !== R.code && r.chain[key].lo <= R.chain[key].hi && r.chain[key].hi >= R.chain[key].lo).length;
    const rank = order.findIndex(r => r.code === R.code) + 1, o1 = overlap('down'), o2 = overlap('spread');
    const where = rank <= 3 ? 'near the chain end' : rank >= 10 ? 'near the web end' : 'in the middle';
    box.append(el('p', null, `${The} is ${where}: ${ordinal(rank)} of 12 on the first number. Its range overlaps ${o1} other region${o1 === 1 ? '' : 's'} on the first number and ${o2} on the second, ` +
      (Math.min(o1, o2) >= 4 ? 'so it cannot be told apart from most of them.' : Math.max(o1, o2) <= 2 ? 'so it stands clearly apart.' : 'so it is distinct from some and not from others.') +
      (R.thin ? ' Its data are also among the thinnest, which by itself pushes a region towards the chain end (see the caution below).' : '')));
    box.append(el('p', 'd-small', `* Thin data. A caution on this whole comparison: across the twelve regions the first number goes up with the share of figures hidden (a rank correlation of ${IDX.chain.down_cor_hidden.toFixed(2)}) and down with the size of the region (${IDX.chain.down_cor_size.toFixed(2)}). Small regions with more hidden look more chain-like. That may be true of smaller economies, or it may be that the thin two-way links that make a web are the ones ONS hides. These data cannot separate the two.`));
  }
  const ordinal = n => n + (['th', 'st', 'nd', 'rd'][n % 10 > 3 || Math.floor(n / 10) === 1 ? 0 : n % 10]);
  $('compare-link') && $('compare-link').addEventListener('click', () => { $('compare').open = true; });
  window.addEventListener('hashchange', () => { if (location.hash === '#compare') $('compare').open = true; }); if (location.hash === '#compare') $('compare').open = true;
  $('alike').addEventListener('click', e => { if (e.target.tagName === 'A') $('compare').open = true; });

  // ---------------------------------------------------------------- start
  readTheme(); loadRegion(S.region);
  let lastW = wrap.clientWidth;
  new ResizeObserver(() => { if (Math.abs(wrap.clientWidth - lastW) > 1) { lastW = wrap.clientWidth; relayout(); } }).observe(wrap);
  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => { readTheme(); if (D) requestDraw(); });
  // for the checks in tests/: set state from outside, and read how long a draw takes
  window.ynhFlows = { state: S, load: code => new Promise(res => { loadRegion(code); (function wait() { if (S.region === code && D && D.region === code && !wrap.classList.contains('loading')) res(); else setTimeout(wait, 30); })(); }),
    set(o) { const inter = 'inter' in o && o.inter !== S.inter; Object.assign(S, o); if (inter) { setActive(); for (const key in totalsCache) delete totalsCache[key]; updateStripMax(); buildSpark(); }
      geoDirty = true; pos = null; moveTo(true); buildKey(); reflect(); wording(); refresh(); }, scriptMs: () => lastScriptMs, drawnLinks: () => drawn.length, play: setPlaying };
})();
