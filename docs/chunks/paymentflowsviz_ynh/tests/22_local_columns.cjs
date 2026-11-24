// 22_local_columns.cjs
// Is "how much of its trade is inside the region" worth ordering columns by, and does it go with the supplier-to-buyer
// order? For each industry in a region, over the whole period: of everything it pays to and is paid by industries
// anywhere in the UK, the share that is with industries in its own region. Three versions, as for the ties columns:
// both ways, only what it is paid ("sells"), only what it pays ("buys"). Payments within the industry inside the region
// count as inside, because the outside figures include payments to the same industry elsewhere. Only payments where both
// sides have a known industry and a known region are in the data files, so payers of unknown industry, households and
// public administration (which has no region) are not in either side of the share.
// This is the same sum the page does (locals() in flows.js), run on the built data files. It prints, per region:
//   - the spread of each measure (does it spread industries out or bunch them?);
//   - its rank correlation with the level in the supplier-to-buyer order (buyers have high levels);
//   - how much the ranking changes if payments within the industry are left out of "inside";
//   - how alike the sells and buys rankings are;
//   - the rank correlation of the level with "buys inside" less "sells inside". Levels are worked out on payments inside
//     the region alone, so an industry that buys here but sells elsewhere is a net buyer in the region and sits high. If
//     that is what links the level to these measures, this difference should track the level better than either does alone.
// Run from the repo root: node docs/chunks/paymentflowsviz_ynh/tests/22_local_columns.cjs [TLE,TLI,...]   (default: all twelve)
const fs = require('fs'), path = require('path');
const dir = path.join(__dirname, '..', 'data'), outDir = path.join(__dirname, 'outputs');
const sum = a => a.reduce((x, y) => x + y, 0);
const quantile = (v, p) => { const s = [...v].sort((a, b) => a - b); return s[Math.min(s.length - 1, Math.floor(p * s.length))]; };
const pct = v => Math.round(100 * v) + '%';
const ranks = v => { const o = v.map((x, i) => [x, i]).sort((a, b) => a[0] - b[0]), r = new Array(v.length);
  for (let i = 0; i < o.length;) { let j = i; while (j + 1 < o.length && o[j + 1][0] === o[i][0]) j++; for (let k = i; k <= j; k++) r[o[k][1]] = (i + j) / 2; i = j + 1; } return r; };
const pearson = (a, b) => { const n = a.length, ma = sum(a) / n, mb = sum(b) / n; let sab = 0, saa = 0, sbb = 0;
  for (let i = 0; i < n; i++) { sab += (a[i] - ma) * (b[i] - mb); saa += (a[i] - ma) ** 2; sbb += (b[i] - mb) ** 2; } return sab / Math.sqrt(saa * sbb); };
const rho = (a, b) => pearson(ranks(a), ranks(b));
const regions = (process.argv[2] || 'TLC,TLD,TLE,TLF,TLG,TLH,TLI,TLJ,TLK,TLL,TLM,TLN').split(',');
const spread = [['region', 'measure', 'industries', 'p10', 'median', 'p90', 'rank_corr_with_level', 'rank_corr_without_own_industry']];
const gaps = [['region', 'rank_corr_sells_with_buys', 'rank_corr_level_with_buys_less_sells']];
const byInd = [['region', 'sic2', 'industry', 'level', 'inside_both', 'inside_sells', 'inside_buys', 'paid_inside_gbp_k', 'received_inside_gbp_k', 'within_industry_gbp_k', 'paid_outside_gbp_k', 'received_outside_gbp_k']];
for (const code of regions) {
  global.window = {}; require(path.join(dir, `flows_${code}.js`)); const D = window.FLOWS_DATA[code];
  const n = D.nodes, L = D.links, core = n.map(d => d.role === 'industry');
  const paid = n.map(() => 0), recv = n.map(() => 0);                          // inside the region, between different industries (intermediaries left out, as the page opens)
  L.supplier.forEach((s, k) => { const b = L.buyer[k]; if (!core[s] || !core[b]) return; const t = sum(L.value[k]); recv[s] += t; paid[b] += t; });
  const self = n.map(d => sum(d.self)), out = n.map(d => sum(d.ext.buy_core)), inn = n.map(d => sum(d.ext.sell_core));
  const idx = n.map((d, i) => i).filter(i => core[i] && !n[i].small), level = idx.map(i => n[i].level);
  const share = (kind, withSelf) => idx.map(i => { const s = withSelf ? self[i] : 0;
    const ins = (kind !== 'buys' ? recv[i] + s : 0) + (kind !== 'sells' ? paid[i] + s : 0), o = (kind !== 'buys' ? inn[i] : 0) + (kind !== 'sells' ? out[i] : 0);
    return ins + o > 0 ? ins / (ins + o) : 0; });
  const m = { both: share('both', true), sells: share('sells', true), buys: share('buys', true) };
  for (const kind of ['both', 'sells', 'buys']) { const v = m[kind], rl = rho(v, level), rs = rho(v, share(kind, false));
    spread.push([code, kind, idx.length, quantile(v, 0.1).toFixed(3), quantile(v, 0.5).toFixed(3), quantile(v, 0.9).toFixed(3), rl.toFixed(2), rs.toFixed(2)]);
    console.log(`${code} ${kind.padEnd(6)} ${idx.length} industries: a tenth below ${pct(quantile(v, 0.1))}, middle ${pct(quantile(v, 0.5))}, a tenth above ${pct(quantile(v, 0.9))} | with level ${rl.toFixed(2)} | with the version leaving out the industry's own payments ${rs.toFixed(2)}`); }
  const gap = m.buys.map((v, j) => v - m.sells[j]), rg = rho(gap, level); gaps.push([code, rho(m.sells, m.buys).toFixed(2), rg.toFixed(2)]);
  console.log(`${code} sells against buys: ${rho(m.sells, m.buys).toFixed(2)} | level against (buys inside less sells inside): ${rg.toFixed(2)}`);
  idx.forEach((i, j) => byInd.push([code, n[i].id, '"' + n[i].short + '"', n[i].level, m.both[j].toFixed(3), m.sells[j].toFixed(3), m.buys[j].toFixed(3), paid[i], recv[i], self[i], out[i], inn[i]]));
  if (code === 'TLE') for (const kind of ['both', 'sells', 'buys']) {
    const ord = idx.map((i, j) => [`${n[i].short} ${pct(m[kind][j])}`, m[kind][j]]).sort((a, b) => b[1] - a[1]).map(x => x[0]);
    console.log(`   ${kind}, most inside: ${ord.slice(0, 9).join('; ')}\n   ${kind}, least inside: ${ord.slice(-8).join('; ')}`); }
}
fs.writeFileSync(path.join(outDir, 'local_spread.csv'), spread.map(r => r.join(',')).join('\n') + '\n');
fs.writeFileSync(path.join(outDir, 'local_by_industry.csv'), byInd.map(r => r.join(',')).join('\n') + '\n');
fs.writeFileSync(path.join(outDir, 'local_level_gap.csv'), gaps.map(r => r.join(',')).join('\n') + '\n');
const col = (kind, j) => spread.slice(1).filter(r => r[1] === kind).map(r => +r[j]).sort((a, b) => a - b), mid = v => (v.length % 2 ? v[(v.length - 1) / 2] : (v[v.length / 2 - 1] + v[v.length / 2]) / 2).toFixed(2);
for (const kind of ['both', 'sells', 'buys']) { const v = col(kind, 6); console.log(`Across regions, ${kind} with level: ${v[0]} to ${v[v.length - 1]}, middle ${mid(v)}, above zero in ${v.filter(x => x > 0).length} of ${v.length}`); }
{ const v = gaps.slice(1).map(r => +r[2]).sort((a, b) => a - b); console.log(`Across regions, level with (buys inside less sells inside): ${v[0]} to ${v[v.length - 1]}, middle ${mid(v)}`); }
console.log('Wrote local_spread.csv, local_by_industry.csv and local_level_gap.csv in', outDir);
