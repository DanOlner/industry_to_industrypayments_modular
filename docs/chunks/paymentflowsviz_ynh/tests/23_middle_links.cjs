// 23_middle_links.cjs
// Two things, both on payments between industries inside a region (intermediaries left out, as the page opens).
//
// 1. A check on something seen on the page. Other manufacturing (SIC 32) in Yorkshire and the Humber sits in the middle
//    of the supplier-to-buyer order with money coming down to it from industries above and going on down to industries
//    below. Is it really a middle link in a chain, or does the default view (one net link a pair) flatter it? Prints its
//    balance, who pays it and whom it pays, how much of its value runs down the order net and gross, how much is two-way
//    trade with the same partner, the same for the view the page opens on, and how it looks in the other regions.
//
// 2. Scoping a "chain measure" for every industry. Everything an industry pays to and is paid by other industries in
//    the region splits exactly three ways, with no use of the levels:
//      two-way        paid to a partner that pays the same amount back: twice the smaller of the two directions, each pair
//      passed along   what is left after that is net: some partners pay it (net buyers), it pays others (net suppliers).
//                     Twice the smaller of net-in and net-out is money that comes in from one industry and goes on to another
//      end of line    the rest: net-in and net-out don't match. More paid out than in: a buyer end, money enters the picture
//                     here (from households, public money, payers of unknown industry, other regions). More in than out: a
//                     supplier end, money leaves the picture here (wages, taxes, profit, imports, other regions)
//    Prints who has the most passed along, how far that is a property of the industry (does it hold from region to
//    region?), how it sits against the level, and each region's total. 23b_middle_links_io.R does the same split on the
//    UK input-output table and sets the two side by side.
//    Then lists three-step chains (net buyer -> middle link -> net supplier), sized by sharing what the middle link passes
//    along in proportion, and traces a chain two steps up and down from one industry.
//
// Run from the repo root: node docs/chunks/paymentflowsviz_ynh/tests/23_middle_links.cjs [sic2] [region]   (default: 32 TLE)
// The jobs breakdown of division 32 quoted in the plan (section 13) is from the build's jobs cache, in R:
//   j <- readRDS('local/paymentflowsviz_ynh/bres_itl1_sic5.rds'); j[itl1 == 'TLE' & substr(sic5, 1, 2) == '32', .(jobs = sum(jobs)), by = substr(sic5, 1, 3)]
const fs = require('fs'), path = require('path');
const dir = path.join(__dirname, '..', 'data'), outDir = path.join(__dirname, 'outputs');
const SIC = process.argv[2] || '32', HOME = process.argv[3] || 'TLE';
const REGIONS = ['TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN'];
const sum = a => a.reduce((x, y) => x + y, 0), pct = v => Math.round(100 * v) + '%', f2 = v => v.toFixed(2), signed = v => (v > 0 ? '+' : '') + v.toFixed(2);
const quant = (v, p) => { const s = [...v].sort((a, b) => a - b); return s[Math.min(s.length - 1, Math.floor(p * s.length))]; };
const median = v => { const s = [...v].sort((a, b) => a - b), n = s.length; return n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2; };
const ranks = v => { const o = v.map((x, i) => [x, i]).sort((a, b) => a[0] - b[0]), r = new Array(v.length);
  for (let i = 0; i < o.length;) { let j = i; while (j + 1 < o.length && o[j + 1][0] === o[i][0]) j++; for (let k = i; k <= j; k++) r[o[k][1]] = (i + j) / 2; i = j + 1; } return r; };
const pearson = (a, b) => { const n = a.length, ma = sum(a) / n, mb = sum(b) / n; let sab = 0, saa = 0, sbb = 0;
  for (let i = 0; i < n; i++) { sab += (a[i] - ma) * (b[i] - mb); saa += (a[i] - ma) ** 2; sbb += (b[i] - mb) ** 2; } return sab / Math.sqrt(saa * sbb); };
const rho = (a, b) => pearson(ranks(a), ranks(b));
const cache = {}, load = code => { if (!cache[code]) { global.window = {}; require(path.join(dir, `flows_${code}.js`)); cache[code] = window.FLOWS_DATA[code]; } return cache[code]; };

// Every placed industry in a region. months: which months to add up (default all); values come out in GBP millions a month.
function measures(code, months) {
  const D = load(code), n = D.nodes, L = D.links, ms = months || D.months.map((_, m) => m), core = n.map(d => d.role === 'industry'), lev = n.map(d => d.level), W = new Map();
  L.supplier.forEach((s, k) => { const b = L.buyer[k]; if (!core[s] || !core[b]) return; W.set(s + '|' + b, sum(ms.map(m => L.value[k][m])) / 1e3 / ms.length); });   // what b pays s
  const g = (s, b) => W.get(s + '|' + b) || 0, out = [];
  n.forEach((d, i) => { if (!core[i] || d.small) return;
    let paid = 0, recv = 0, twoWay = 0, netIn = 0, netOut = 0, grossDown = 0, netDown = 0; const partners = [];
    n.forEach((e, j) => { if (j === i || !core[j]) return; const r = g(i, j), p = g(j, i); if (!(r + p)) return;     // r: j pays i (j buys from i); p: i pays j
      paid += p; recv += r; twoWay += 2 * Math.min(r, p);
      if (lev[j] > lev[i]) grossDown += r; if (lev[j] < lev[i]) grossDown += p;                                       // money runs down when the payer is the higher of the two
      const net = r - p; if (net > 0) { netIn += net; if (lev[j] > lev[i]) netDown += net; } else { netOut -= net; if (lev[j] < lev[i]) netDown -= net; }
      partners.push({ id: e.id, name: e.short, lev: lev[j], r, p, net }); });
    const gross = paid + recv; if (!gross) return;
    out.push({ code, id: d.id, name: d.short, level: lev[i], paid, recv, gross, balance: (paid - recv) / gross, twoWay: twoWay / gross, passed: 2 * Math.min(netIn, netOut) / gross,
               end: Math.abs(netIn - netOut) / gross, endKind: netOut > netIn ? 'buyer' : 'supplier', netIn, netOut, grossDown: grossDown / gross, netDown: netIn + netOut ? netDown / (netIn + netOut) : 0,
               partners, never: d.never_core, cells: d.cells_core, rolling: d.level_rolling }); });
  return out;
}
const placeFrom = (all, x, f, top) => top ? all.filter(d => f(d) > f(x)).length + 1 : all.filter(d => f(d) < f(x)).length + 1;
const money = v => '£' + (v >= 100 ? v.toFixed(0) : v.toFixed(1)) + 'm';

// ---- 1. The industry asked about
const E = measures(HOME), x = E.find(d => d.id === SIC), N = E.length;
if (!x) { console.log(`SIC ${SIC} is not a placed industry in ${HOME}`); process.exit(1); }
console.log(`== 1. ${x.name} (SIC ${SIC}) in ${load(HOME).name}, whole period ==`);
console.log(`level ${x.level}: ${placeFrom(E, x, d => d.level)} of ${N} counting from the supplier end. 12 months to each month it runs from ${f2(Math.min(...x.rolling.filter(v => v != null)))} to ${f2(Math.max(...x.rolling.filter(v => v != null)))}`);
console.log(`pays other industries here ${money(x.paid)} a month and is paid ${money(x.recv)}: ${placeFrom(E, x, d => d.gross, true)} of ${N} by size. ${x.never} of its ${x.cells} links were never published`);
const line = (label, f, top) => console.log(`${label}: ${pct(f(x))} (${placeFrom(E, x, f, top)} of ${N} from the ${top ? 'highest' : 'lowest'}; middle industry ${pct(median(E.map(f)))})`);
line('value running down the order, one net link a pair', d => d.netDown, true); line('value running down the order, both directions counted', d => d.grossDown, true);
line('two-way with the same partner', d => d.twoWay, false);
const top = (f, k = 8) => x.partners.slice().sort((a, b) => f(b) - f(a)).slice(0, k);
console.log('paid most by (partner, levels above it; pays it / is paid by it, a month):\n  ' + top(p => p.r).map(p => `${p.name} (${signed(p.lev - x.level)}) ${money(p.r)} / ${money(p.p)}`).join('\n  '));
console.log('pays most to:\n  ' + top(p => p.p).map(p => `${p.name} (${signed(p.lev - x.level)}) ${money(p.p)} / ${money(p.r)}`).join('\n  '));
{ const NM = load(HOME).months.length, Lm = measures(HOME, [NM - 3, NM - 2, NM - 1]), y = Lm.find(d => d.id === SIC);
  console.log(`three months to ${load(HOME).months[NM - 1]} (the view the page opens on): pays ${money(y.paid)}, is paid ${money(y.recv)}; net value running down ${pct(y.netDown)}, both directions ${pct(y.grossDown)}`);
  console.log('  largest net links: ' + y.partners.slice().sort((a, b) => Math.abs(b.net) - Math.abs(a.net)).slice(0, 8).map(p => `${p.name} ${p.net > 0 ? 'pays it' : 'is paid'} ${money(Math.abs(p.net))} net${(p.net > 0) === (p.lev > x.level) ? '' : ' (UP)'}`).join('; ')); }
console.log(`${x.name} in each region:`);
const all = {}; for (const code of REGIONS) all[code] = measures(code);
for (const code of REGIONS) { const R = all[code], y = R.find(d => d.id === SIC); if (!y) { console.log(`  ${code}: not placed`); continue; }
  console.log(`  ${code}: level ${signed(y.level)}, pays ${money(y.paid)} is paid ${money(y.recv)}, net down ${pct(y.netDown)}, gross down ${pct(y.grossDown)}, two-way ${pct(y.twoWay)}, passed along ${pct(y.passed)}; paid most by ${y.partners.slice().sort((a, b) => b.r - a.r).slice(0, 3).map(p => p.name).join(', ')}; pays most to ${y.partners.slice().sort((a, b) => b.p - a.p).slice(0, 3).map(p => p.name).join(', ')}`); }

// ---- 2. The three-way split for every industry
console.log(`\n== 2. Two-way, passed along, end of line ==`);
const show = d => `${d.name}: two-way ${pct(d.twoWay)}, passed along ${pct(d.passed)}, ${d.endKind} end ${pct(d.end)}`;
console.log(`${load(HOME).name}, most passed along:\n  ` + E.slice().sort((a, b) => b.passed - a.passed).slice(0, 12).map(show).join('\n  '));
console.log('least passed along:\n  ' + E.slice().sort((a, b) => a.passed - b.passed).slice(0, 8).map(show).join('\n  '));
console.log(`${x.name}: ${show(x)}; ${placeFrom(E, x, d => d.passed, true)} of ${N} by share passed along`);
const rows = [['region', 'sic2', 'industry', 'level', 'paid_gbp_m_month', 'received_gbp_m_month', 'two_way', 'passed_along', 'end_of_line', 'end_kind', 'net_down_the_order', 'gross_down_the_order']];
const regionRows = [['region', 'industries', 'two_way', 'passed_along', 'end_of_line', 'p10_passed', 'median_passed', 'p90_passed', 'rank_corr_passed_with_distance_from_average_level']];
for (const code of REGIONS) { const R = all[code], G = sum(R.map(d => d.gross)), w = f => sum(R.map(d => f(d) * d.gross)) / G, v = R.map(d => d.passed);
  for (const d of R) rows.push([code, d.id, '"' + d.name + '"', d.level, d.paid.toFixed(2), d.recv.toFixed(2), d.twoWay.toFixed(3), d.passed.toFixed(3), d.end.toFixed(3), d.endKind, d.netDown.toFixed(3), d.grossDown.toFixed(3)]);
  const rl = rho(v, R.map(d => -Math.abs(d.level)));
  regionRows.push([code, R.length, w(d => d.twoWay).toFixed(3), w(d => d.passed).toFixed(3), w(d => d.end).toFixed(3), quant(v, 0.1).toFixed(3), median(v).toFixed(3), quant(v, 0.9).toFixed(3), rl.toFixed(2)]);
  console.log(`${code}: of all payments between industries, two-way ${pct(w(d => d.twoWay))}, passed along ${pct(w(d => d.passed))}, end of line ${pct(w(d => d.end))} | by industry, passed along: a tenth below ${pct(quant(v, 0.1))}, middle ${pct(median(v))}, a tenth above ${pct(quant(v, 0.9))} | with closeness to the average level ${f2(rl)}`); }
// does it belong to the industry? rank correlation between each pair of regions, over industries placed in both
const between = f => { const cs = []; for (let a = 0; a < REGIONS.length; a++) for (let b = a + 1; b < REGIONS.length; b++) { const A = new Map(all[REGIONS[a]].map(d => [d.id, f(d)])), both = all[REGIONS[b]].filter(d => A.has(d.id));
    cs.push(rho(both.map(d => A.get(d.id)), both.map(f))); } return cs; };
for (const [label, f] of [['passed along', d => d.passed], ['two-way', d => d.twoWay], ['balance (paid less received, over both)', d => d.balance], ['level', d => d.level]]) { const cs = between(f);
  console.log(`same industry, two regions, rank correlation of ${label}: middle ${f2(median(cs))}, from ${f2(Math.min(...cs))} to ${f2(Math.max(...cs))}`); }
fs.writeFileSync(path.join(outDir, 'middle_links_by_industry.csv'), rows.map(r => r.join(',')).join('\n') + '\n');
fs.writeFileSync(path.join(outDir, 'middle_links_regions.csv'), regionRows.map(r => r.join(',')).join('\n') + '\n');

// ---- 3. Chains: net buyer -> middle link -> net supplier. What the middle link passes along is shared among its net
// buyers and net suppliers in proportion, so a chain carries (net from buyer) x (net to supplier) / (the larger of net-in and net-out).
console.log(`\n== 3. Three-step chains ==`);
const chainRows = [['region', 'rank', 'buyer', 'middle_link', 'supplier', 'gbp_m_month', 'share_of_all_passed_along', 'runs_down_the_order']];
const chains = code => { const out = []; for (const d of all[code]) { const big = Math.max(d.netIn, d.netOut); if (!big) continue;
    for (const b of d.partners) if (b.net > 0) for (const s of d.partners) if (s.net < 0) out.push({ b, d, s, v: b.net * -s.net / big, down: b.lev > d.level && d.level > s.lev }); }
  return out.sort((p, q) => q.v - p.v); };
for (const code of REGIONS) { const C = chains(code), tot = sum(C.map(c => c.v));
  C.slice(0, 40).forEach((c, k) => chainRows.push([code, k + 1, '"' + c.b.name + '"', '"' + c.d.name + '"', '"' + c.s.name + '"', c.v.toFixed(2), (c.v / tot).toFixed(4), c.down]));
  console.log(`${code}: ${C.length} chains carry ${money(tot)} a month; the top 10 carry ${pct(sum(C.slice(0, 10).map(c => c.v)) / tot)}, the top 40 ${pct(sum(C.slice(0, 40).map(c => c.v)) / tot)}; ${pct(sum(C.filter(c => c.down).map(c => c.v)) / tot)} of it runs down the order at both steps`);
  if (code === HOME) { console.log('  ' + C.slice(0, 15).map(c => `${c.b.name} -> ${c.d.name} -> ${c.s.name}: ${money(c.v)}${c.down ? '' : ' (not down the order)'}`).join('\n  '));
    console.log(`  through ${x.name}:\n    ` + C.filter(c => c.d.id === SIC).slice(0, 8).map(c => `${c.b.name} -> ${c.d.name} -> ${c.s.name}: ${money(c.v)}`).join('\n    ')); } }
fs.writeFileSync(path.join(outDir, 'middle_links_chains.csv'), chainRows.map(r => r.join(',')).join('\n') + '\n');
// Tracing a chain from one industry: up through net buyers that give a tenth or more of its net receipts, down through net
// suppliers that take a tenth or more of its net payments, two steps each way.
{ const byId = new Map(E.map(d => [d.id, d])), seen = new Set([SIC]);
  const step = (d, up, depth, indent) => { for (const p of d.partners.filter(p => up ? p.net > 0 : p.net < 0).sort((a, b) => Math.abs(b.net) - Math.abs(a.net))) { const share = Math.abs(p.net) / (up ? d.netIn : d.netOut); if (share < 0.1) break;
      console.log(`${indent}${up ? 'paid by' : 'pays'} ${p.name}: ${money(Math.abs(p.net))} net, ${pct(share)} of its net ${up ? 'receipts' : 'payments'}${seen.has(p.id) ? ' (already in the chain)' : ''}`);
      if (depth > 1 && byId.has(p.id) && !seen.has(p.id)) { seen.add(p.id); step(byId.get(p.id), up, depth - 1, indent + '    '); } } };
  console.log(`\nTracing from ${x.name} in ${load(HOME).name}, links of a tenth or more:\n  upwards`); step(x, true, 2, '    '); console.log('  downwards'); step(x, false, 2, '    '); }
console.log('\nWrote middle_links_by_industry.csv, middle_links_regions.csv and middle_links_chains.csv in', outDir);
