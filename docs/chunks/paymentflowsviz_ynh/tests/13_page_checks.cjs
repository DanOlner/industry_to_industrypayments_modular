// 13_page_checks.cjs
// Checks on the finished page (../index.html) in headless Chrome: loads without errors, every control
// works, screenshots in light, dark and narrow layouts, and how fast it plays.
// Needs puppeteer-core and Google Chrome, as 07 does:
//   NODE_PATH=/path/to/node_modules node 13_page_checks.cjs            (add --shots-only to skip the timing)
const puppeteer = require('puppeteer-core');
const path = require('path'), fs = require('fs');
const url = 'file://' + path.resolve(__dirname, '../index.html');
const out = path.resolve(__dirname, 'outputs/page'); fs.mkdirSync(out, { recursive: true });
const problems = [];

(async () => {
  const browser = await puppeteer.launch({ executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: 'new',
    args: ['--allow-file-access-from-files', '--enable-gpu', '--use-angle=metal'] });
  const open = async (w, h, scheme, query = '', dsf = 1) => {
    const page = await browser.newPage();
    page.on('pageerror', e => problems.push('page error: ' + e.message));
    page.on('console', m => { if (m.type() === 'error') problems.push('console error: ' + m.text()); });
    await page.emulateMediaFeatures([{ name: 'prefers-color-scheme', value: scheme }]);
    await page.setViewport({ width: w, height: h, deviceScaleFactor: dsf });
    await page.goto(url + query, { waitUntil: 'load' });
    await page.waitForFunction(() => window.ynhFlows && window.ynhFlows.drawnLinks() > 0, { timeout: 15000 });
    await new Promise(r => setTimeout(r, 400));
    return page;
  };
  const shot = async (page, name, sel) => { const t = sel ? await page.$(sel) : page; await t.screenshot({ path: path.join(out, name + '.png'), ...(sel ? {} : { fullPage: true }) }); console.log('saved', name); };
  const set = (page, o) => page.evaluate(s => window.ynhFlows.set(s), o);

  // ---- 1. the opening view, light and dark, wide and narrow
  let page = await open(1440, 1000, 'light');
  await shot(page, '01_open_light_full');
  await shot(page, '02_open_light_stage', '.stage');
  await set(page, { sel: 39 }); await shot(page, '03_pinned_industry', '.stage');
  await set(page, { sel: -1, cols: 'paygroup' }); await shot(page, '04_payment_group_columns', '#chartwrap');
  await set(page, { cols: 'group', lens: 'is8:any' }); await shot(page, '05_highlight_industrial_strategy', '#chartwrap');
  await set(page, { lens: 'pay:Construction' }); await shot(page, '06_highlight_payment_group', '#chartwrap');
  await set(page, { lens: 'none', layout: 'hybrid' }); await shot(page, '07_hybrid', '#chartwrap');
  await set(page, { layout: 'force' }); await shot(page, '08_connections', '#chartwrap');
  await set(page, { layout: 'order', inter: true }); await shot(page, '09_intermediaries_in', '#chartwrap');
  await set(page, { inter: false, halo: true, cover: '0.5' }); await shot(page, '10_halo_fewer_links', '.stage');
  await set(page, { halo: false, cover: '0.8', scale: 'amount', t: 3 }); await shot(page, '11_amounts_2019', '.stage');
  await set(page, { scale: 'share', t: 92, pubonly: true }); await shot(page, '12_published_only', '#chartwrap');
  await set(page, { pubonly: false, measure: 'count' }); await shot(page, '13_number_of_payments', '.stage');
  await set(page, { measure: 'value' });
  await page.evaluate(() => { document.getElementById('tablebox').open = true; }); await new Promise(r => setTimeout(r, 200));
  await shot(page, '14_table', '#tablebox');
  // hover a link and a node, for the tooltips
  const box = await (await page.$('#chart')).boundingBox();
  await page.mouse.move(box.x + box.width * 0.46, box.y + box.height * 0.55); await new Promise(r => setTimeout(r, 200)); await shot(page, '15_tooltip_a', '#chartwrap');
  await page.close();

  page = await open(1440, 1000, 'dark'); await shot(page, '16_open_dark_full'); await set(page, { sel: 39, lens: 'group:Manufacturing' }); await shot(page, '17_dark_pinned_highlight', '.stage'); await page.close();
  page = await open(390, 844, 'light', '', 2); await shot(page, '18_phone_full'); await shot(page, '18b_phone_chart', '#chartwrap'); await page.close();
  page = await open(900, 1100, 'light'); await shot(page, '19_tablet_full'); await page.close();

  // ---- 2. every control changes something and nothing throws
  page = await open(1280, 900, 'light');
  const results = [];
  const ids = await page.evaluate(() => [...document.querySelectorAll('.controls select, .timebar select')].map(s => s.id));
  for (const id of ids) { const n = await page.evaluate(i => document.getElementById(i).options.length, id);
    for (let j = 0; j < n; j++) { await page.evaluate((i, jj) => { const s = document.getElementById(i); s.selectedIndex = jj; s.dispatchEvent(new Event('change')); }, id, j); await new Promise(r => setTimeout(r, 30)); }
    await page.evaluate(i => { const s = document.getElementById(i); s.selectedIndex = 0; s.dispatchEvent(new Event('change')); }, id); results.push(`${id}: ${n} options`); }
  for (const id of ['smooth', 'dots', 'inter', 'halo', 'pubonly']) { for (let j = 0; j < 2; j++) { await page.click('#' + id); await new Promise(r => setTimeout(r, 60)); } results.push(id + ': toggled'); }
  for (const v of ['hybrid', 'force', 'order']) { await page.evaluate(val => { const r = document.querySelector(`input[name=layout][value=${val}]`); r.checked = true; r.dispatchEvent(new Event('change')); }, v); await new Promise(r => setTimeout(r, 800)); }
  await page.click('#prev'); await page.click('#prev'); await page.click('#next');
  await page.keyboard.press('ArrowLeft'); await page.keyboard.press('Escape');
  const label = await page.evaluate(() => document.getElementById('monthLabel').textContent);
  console.log('controls exercised:', results.join('; '), '| month label now:', label, '| url:', await page.evaluate(() => location.search));
  await page.close();

  // ---- 3. playing
  if (!process.argv.includes('--shots-only')) {
    for (const [name, q, dsf] of [['opening view', '', 2], ['every link, both directions, intermediaries in', '?cover=1&net=gross&inter=1', 2]]) {
      page = await open(1440, 1000, 'light', q, dsf);
      const r = await page.evaluate(() => new Promise(res => { let n = 0; const ms = [], t0 = performance.now(); window.ynhFlows.state.t = 0; window.ynhFlows.play(true);
        (function f() { n++; ms.push(window.ynhFlows.scriptMs()); if (performance.now() - t0 < 5000) requestAnimationFrame(f); else { ms.sort((a, b) => a - b); res({ fps: n / 5, ms: ms[Math.floor(ms.length / 2)], links: window.ynhFlows.drawnLinks() }); } })(); }));
      console.log(`playing, ${name}: ${r.links} links, ${r.ms.toFixed(1)} ms of script a frame, ${Math.round(r.fps)} frames a second`);
      await page.close();
    }
  }
  await browser.close();
  console.log(problems.length ? 'PROBLEMS:\n  ' + [...new Set(problems)].join('\n  ') : 'no page or console errors');
})().catch(e => { console.error('FAILED', e); process.exit(1); });
