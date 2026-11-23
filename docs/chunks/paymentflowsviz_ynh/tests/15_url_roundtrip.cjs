// 15_url_roundtrip.cjs
// Scoping for "every option has its own URL": does each control already write itself to the address bar, and does
// loading that address bring the same view back? Changes nothing on the page; it only reports.
//   NODE_PATH=/path/to/node_modules node 15_url_roundtrip.cjs
const puppeteer = require('puppeteer-core');
const path = require('path'), fs = require('fs');
const url = 'file://' + path.resolve(__dirname, '../index.html');
const wait = ms => new Promise(r => setTimeout(r, ms));

(async () => {
  const browser = await puppeteer.launch({ executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: 'new', args: ['--allow-file-access-from-files'] });
  const open = async q => { const p = await browser.newPage(); await p.setViewport({ width: 1300, height: 900 }); await p.goto(url + q, { waitUntil: 'load' });
    await p.waitForFunction(() => window.ynhFlows && window.ynhFlows.drawnLinks() >= 0); await wait(250); return p; };
  const state = p => p.evaluate(() => { const s = window.ynhFlows.state; return { ...s, t: Math.round(s.t) }; });
  const rows = [];

  // every select, radio group and checkbox on the page: set each non-default value through the control itself
  let page = await open('');
  const base = await state(page);
  const controls = await page.evaluate(() => {
    const out = [];
    document.querySelectorAll('select').forEach(s => out.push({ kind: 'select', id: s.id, values: [...s.options].filter(o => !o.disabled).map(o => o.value) }));
    const groups = {}; document.querySelectorAll('input[type=radio]').forEach(r => { (groups[r.name] = groups[r.name] || []).push(r.value); });
    for (const name in groups) out.push({ kind: 'radio', id: name, values: groups[name] });
    document.querySelectorAll('input[type=checkbox]').forEach(c => out.push({ kind: 'check', id: c.id, values: [true, false] }));
    return out;
  });
  await page.close();
  for (const c of controls) {
    let ok = 0, tried = 0; const bad = [];
    for (const v of c.values) {
      page = await open(c.id === 'cols' ? '' : '');
      await page.evaluate((c, v) => {
        if (c.kind === 'select') { const s = document.getElementById(c.id); s.value = v; s.dispatchEvent(new Event('change')); }
        else if (c.kind === 'radio') { const r = document.querySelector(`input[name=${c.id}][value="${v}"]`); r.checked = true; r.dispatchEvent(new Event('change')); }
        else { const b = document.getElementById(c.id); if (b.checked !== v) { b.checked = v; b.dispatchEvent(new Event('change')); } }
      }, c, v);
      await wait(450);
      const q = await page.evaluate(() => location.search), before = await state(page); await page.close();
      const fresh = await open(q), after = await state(fresh); await fresh.close();
      const key = c.id === 'pick' ? 'sel' : c.id; tried++;
      if (JSON.stringify(before[key]) === JSON.stringify(after[key])) ok++; else bad.push(`${v} (address ${q || 'empty'}: ${before[key]} became ${after[key]})`);
    }
    rows.push({ control: c.id, kind: c.kind, values: tried, come_back: ok, problems: bad.slice(0, 3).join('; ') });
  }

  // the month slider and the step buttons
  page = await open(''); await page.click('#prev'); await page.click('#prev'); await wait(450);
  let q = await page.evaluate(() => location.search), b = await state(page); await page.close();
  let f = await open(q), a = await state(f); await f.close();
  rows.push({ control: 'month (step buttons)', kind: 'buttons', values: 1, come_back: +(a.t === b.t), problems: a.t === b.t ? '' : `${q}: ${b.t} became ${a.t}` });

  // pinning by clicking a circle is covered by 'pick' above. Things that are not in the address:
  page = await open('');
  await page.evaluate(() => { document.getElementById('tablebox').open = true; document.getElementById('order-note').open = true; }); await wait(400);
  const extras = { table_or_note_open_in_address: (await page.evaluate(() => location.search + location.hash)) !== '' };
  await page.click('#play'); await wait(600); extras.playing_in_address = /play/.test(await page.evaluate(() => location.search)); await page.click('#play'); await page.close();

  // everything at once, then a mangled address
  const all = '?layout=hybrid&cols=shift&lens=is8%3AAdvanced+manufacturing&show=goods&dir=up&measure=count&scale=amount&net=gross&cover=0.5&speed=6&smooth=0&dots=0&inter=1&halo=1&pubonly=1&m=2021-06&sel=64';
  page = await open(all); const sAll = await state(page); await wait(400); const qAll = await page.evaluate(() => location.search); await page.close();
  page = await open('?layout=sideways&cols=nonsense&lens=is8:Nothing&m=1999-01&sel=zz&cover=7&smooth=maybe'); const sBad = await state(page); await page.close();
  await browser.close();

  console.table(rows);
  console.log('Not carried in the address:', JSON.stringify(extras));
  console.log('All options at once: layout', sAll.layout, '| cols', sAll.cols, '| lens', sAll.lens, '| show', sAll.show, '| dir', sAll.dir, '| measure', sAll.measure, '| scale', sAll.scale, '| net', sAll.net,
    '| cover', sAll.cover, '| speed', sAll.speed, '| smooth', sAll.smooth, '| dots', sAll.dots, '| inter', sAll.inter, '| halo', sAll.halo, '| pubonly', sAll.pubonly, '| month', sAll.t, '| pinned', sAll.sel);
  console.log('  address after loading it:', qAll, `(${qAll.length} characters)`);
  console.log('Mangled address falls back to: layout', sBad.layout, '| cols', sBad.cols, '| lens', sBad.lens, '| month', sBad.t, '| pinned', sBad.sel, '| cover', sBad.cover, '| smooth', sBad.smooth);
  fs.writeFileSync(path.resolve(__dirname, 'outputs/url_roundtrip.csv'), 'control,kind,values_tried,came_back,problems\n' + rows.map(r => [r.control, r.kind, r.values, r.come_back, '"' + r.problems.replace(/"/g, "'") + '"'].join(',')).join('\n') + '\n');
})().catch(e => { console.error('FAILED', e); process.exit(1); });
