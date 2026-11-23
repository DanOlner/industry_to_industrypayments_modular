// 21_ties_columns.cjs
// Is "ties to what is highlighted" worth ordering columns by? For each industry in a region: of everything it pays to
// and is paid by other industries there, over the whole period, the share that is with a highlighted set. A partner
// counts by its share in the set. This is the same sum the page does (ties() in flows.js), run on the built data files
// for the four foundational economy highlights, to see whether the measure spreads industries out or bunches them.
// Run from the repo root: node docs/chunks/paymentflowsviz_ynh/tests/21_ties_columns.cjs [TLE,TLI,...]   (default: all twelve)
const fs = require('fs'), path = require('path');
const dir = path.join(__dirname, '..', 'data'), out = path.join(__dirname, 'outputs', 'ties_spread.csv');
const sets = { both: f => Math.min(1, f[0] + f[1]), material: f => f[0], providential: f => f[1], overlooked: f => f[2] };
const quantile = (v, p) => { const s = [...v].sort((a, b) => a - b); return s[Math.min(s.length - 1, Math.floor(p * s.length))]; };
const pct = v => Math.round(100 * v) + '%';
const regions = (process.argv[2] || 'TLC,TLD,TLE,TLF,TLG,TLH,TLI,TLJ,TLK,TLL,TLM,TLN').split(',');
const rows = [['region', 'highlight', 'industries', 'p10', 'median', 'p90']];
for (const code of regions) {
  global.window = {}; require(path.join(dir, `flows_${code}.js`)); const D = window.FLOWS_DATA[code];
  const n = D.nodes, L = D.links, total = L.value.map(a => a.reduce((x, y) => x + y, 0)), core = n.map(d => d.role === 'industry');
  for (const [name, f] of Object.entries(sets)) {
    const sh = n.map(d => f(d.fe || [0, 0, 0])), num = n.map(() => 0), den = n.map(() => 0);
    L.supplier.forEach((s, k) => { const b = L.buyer[k]; if (!core[s] || !core[b]) return;            // intermediaries left out, as the page opens
      num[s] += total[k] * sh[b]; den[s] += total[k]; num[b] += total[k] * sh[s]; den[b] += total[k]; });
    const idx = n.map((d, i) => i).filter(i => core[i] && !n[i].small && den[i] > 0), t = idx.map(i => num[i] / den[i]);
    rows.push([code, name, idx.length, quantile(t, 0.1).toFixed(3), quantile(t, 0.5).toFixed(3), quantile(t, 0.9).toFixed(3)]);
    console.log(`${code} ${name.padEnd(12)} ${idx.length} industries: a tenth below ${pct(quantile(t, 0.1))}, middle ${pct(quantile(t, 0.5))}, a tenth above ${pct(quantile(t, 0.9))}`);
    if (code === 'TLE' && (name === 'both' || name === 'providential')) {
      const ord = idx.map((i, j) => `${n[i].short} ${pct(t[j])}${sh[i] >= 0.5 ? '*' : ''}`).map((s, j) => [s, t[j]]).sort((a, b) => b[1] - a[1]).map(x => x[0]);
      console.log('   most tied (* = itself mostly in the set): ' + ord.slice(0, 9).join('; '));
      console.log('   least tied: ' + ord.slice(-7).join('; '));
      const where = id => { const i = n.findIndex(d => d.id === id), j = idx.indexOf(i); return `${n[i].short} ${pct(t[j])}, ${t.filter(v => v < t[j]).length + 1} of ${idx.length} from the least`; };
      console.log('   ' + where('85') + '; ' + where('86'));
    }
  }
}
fs.writeFileSync(out, rows.map(r => r.join(',')).join('\n') + '\n');
console.log('Wrote', out);
