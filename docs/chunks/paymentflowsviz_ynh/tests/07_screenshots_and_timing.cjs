// 07_screenshots_and_timing.cjs
// Drive the prototype in headless Chrome: save a screenshot of each layout and lens, and time the
// drawing (canvas against SVG, and canvas at several link counts).
// Needs puppeteer-core (npm install puppeteer-core somewhere, then point NODE_PATH at its node_modules)
// and Google Chrome. Run from this folder:
//   NODE_PATH=/path/to/node_modules node 07_screenshots_and_timing.cjs
const puppeteer = require('puppeteer-core');
const path = require('path'), fs = require('fs');
const page_url = 'file://' + path.resolve(__dirname, 'prototype/prototype.html');
const out = path.resolve(__dirname, 'outputs/screens'); fs.mkdirSync(out, { recursive: true });

const shots = [
  ['01_order_free', { layout: 'order', cols: 'free', top: 500 }],
  ['02_order_groups', { layout: 'order', cols: 'group', top: 500 }],
  ['03_hybrid', { layout: 'hybrid', cols: 'free', top: 300 }],
  ['04_connections', { layout: 'force', cols: 'free', top: 300 }],
  ['05_order_groups_lens_manufacturing', { layout: 'order', cols: 'group', top: 800, lens: 'group:Manufacturing' }],
  ['06_order_groups_lens_is8_any', { layout: 'order', cols: 'group', top: 800, lens: 'is8:any' }],
  ['07_hybrid_lens_advanced_manufacturing', { layout: 'hybrid', cols: 'free', top: 800, lens: 'is8:Advanced manufacturing' }],
  ['08_order_groups_gross_all_links', { layout: 'order', cols: 'group', top: 3210, net: 'gross' }],
  ['09a_order_groups_ring', { layout: 'order', cols: 'group', top: 500, ext: 'ring' }],
  ['09b_order_groups_halo', { layout: 'order', cols: 'group', top: 500, ext: 'halo' }],
  ['09c_order_groups_halo_top150', { layout: 'order', cols: 'group', top: 150, ext: 'halo' }],
  ['10_order_groups_count', { layout: 'order', cols: 'group', top: 500, measure: 'count' }],
  ['11_order_groups_2019_01', { layout: 'order', cols: 'group', top: 500, t: 2 }],
  ['12_hybrid_groups', { layout: 'hybrid', cols: 'group', top: 500 }],
  ['13_dots_payments_top430', { layout: 'order', cols: 'group', top: 430, dots: true }],
  ['14_dots_payments_hybrid_top150', { layout: 'hybrid', cols: 'free', top: 150, dots: true }],
  ['15_intermediaries_in', { layout: 'order', cols: 'group', top: 500, inter: true }],
  ['16_payment_group_columns', { layout: 'order', cols: 'paygroup', top: 430 }],
  ['17_hybrid_payment_group_outlines', { layout: 'hybrid', cols: 'free', top: 300, hulls: true }],
  ['18_connections_payment_group_outlines', { layout: 'force', cols: 'free', top: 300, hulls: true }],
];

(async () => {
  const browser = await puppeteer.launch({ executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: 'new',
    args: ['--allow-file-access-from-files', '--enable-gpu', '--use-angle=metal'] });
  const page = await browser.newPage();
  page.on('pageerror', e => console.log('PAGE ERROR', e.message));
  page.on('console', m => { if (m.type() === 'error') console.log('CONSOLE', m.text()); });
  await page.setViewport({ width: 1400, height: 1080, deviceScaleFactor: 1 });
  await page.goto(page_url, { waitUntil: 'load' });
  await page.waitForFunction(() => typeof window.setState === 'function');
  const base = { layout: 'order', cols: 'free', measure: 'value', net: 'net', top: 500, lens: 'none', ext: 'off', smooth: true, share: false, dots: false, t: 92, mult: 1, renderer: 'canvas', batch: false, inter: false, hulls: false };
  for (const [name, st] of (process.argv.includes('--timing-only') ? [] : shots.filter(x => { const o = process.argv.find(a => a.startsWith('--only=')); return !o || o.slice(7).split(',').some(p => x[0].startsWith(p)); }))) {
    await page.evaluate(s => window.setState(s), { ...base, ...st });
    await (await page.$('#wrap')).screenshot({ path: path.join(out, name + '.png') });
    console.log('saved', name);
  }

  // ---- Timing: a fresh page for each case, played for five seconds ----
  // script_ms is the time the page's own drawing code takes per frame; frames_per_sec is what the
  // browser actually put on screen while playing (60 is the ceiling), so it includes painting.
  const gpu = await page.evaluate(() => { const g = document.createElement('canvas').getContext('webgl'); const e = g && g.getExtension('WEBGL_debug_renderer_info'); return g ? g.getParameter(e ? e.UNMASKED_RENDERER_WEBGL : g.RENDERER) : 'no webgl'; });
  console.log('\nGraphics backend:', gpu);
  const cases = [
    ['canvas, one fill per link', 'top 500 net', { top: 500 }],
    ['canvas, one path per colour and opacity step', 'top 500 net', { top: 500, batch: true }],
    ['svg paths', 'top 500 net', { top: 500, renderer: 'svg' }],
    ['canvas, one fill per link', 'all links, both directions', { top: 3210, net: 'gross' }],
    ['canvas, one path per colour and opacity step', 'all links, both directions', { top: 3210, net: 'gross', batch: true }],
    ['svg paths', 'all links, both directions', { top: 3210, net: 'gross', renderer: 'svg' }],
    ['canvas, one fill per link', 'all links, both directions, moving dots', { top: 3210, net: 'gross', dots: true }],
    ['canvas, one fill per link', 'x5 (about 16,000 links)', { top: 3210, net: 'gross', mult: 5 }],
    ['canvas, one fill per link', 'x15 (about 47,000 links)', { top: 3210, net: 'gross', mult: 15 }],
    ['canvas, one fill per link', 'x35 (about 110,000 links)', { top: 3210, net: 'gross', mult: 35 }],
  ];
  const rows = [];
  for (const [renderer, load, st] of (process.argv.includes('--shots-only') ? [] : cases)) {
    const pg = await browser.newPage(); await pg.setViewport({ width: 1400, height: 1080, deviceScaleFactor: 2 });   // 2 = a high-density laptop screen
    await pg.goto(page_url, { waitUntil: 'load' }); await pg.waitForFunction(() => typeof window.setState === 'function');
    await pg.evaluate(s => window.setState(s), { ...base, layout: 'order', cols: 'group', ...st, t: 0 });
    const r = await pg.evaluate(() => new Promise(res => { let n = 0; const ms = []; const t0 = performance.now(); document.getElementById('play').click();
      (function f() { n++; const m = parseFloat(document.getElementById('perf').textContent.split(' in ')[1]); if (m) ms.push(m);
        if (performance.now() - t0 < 5000) requestAnimationFrame(f); else { ms.sort((a, b) => a - b); res({ fps: n / 5, script: ms[Math.floor(ms.length / 2)], links: +document.getElementById('perf').textContent.split(' ')[0] }); } })(); }));
    rows.push({ drawing: renderer, load, links: r.links, script_ms: +r.script.toFixed(1), frames_per_sec: Math.round(r.fps) });
    await pg.close();
  }
  if (rows.length) console.table(rows);
  if (rows.length) fs.writeFileSync(path.resolve(__dirname, 'outputs/drawing_timing.csv'), 'drawing,load,links,script_ms,frames_per_sec\n' + rows.map(r => Object.values(r).map(v => typeof v === 'string' ? '"' + v + '"' : v).join(',')).join('\n') + '\n');
  await browser.close();
})();
