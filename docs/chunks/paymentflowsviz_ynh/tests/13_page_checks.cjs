// 13_page_checks.cjs
// Checks on the finished page (../index.html) in headless Chrome: loads without errors, every control
// works, screenshots in light, dark and narrow layouts, and how fast it plays.
// Needs puppeteer-core and Google Chrome, as 07 does:
//   NODE_PATH=/path/to/node_modules node 13_page_checks.cjs            (add --shots-only to skip the timing)
// To check a copy of the page somewhere else (for example one holding staged data that is not on the site yet):
//   PAGE_DIR=/path/to/copy OUT_DIR=/path/for/screenshots node 13_page_checks.cjs --regions-only
const puppeteer = require('puppeteer-core');
const path = require('path'), fs = require('fs');
const url = 'file://' + path.resolve(process.env.PAGE_DIR || path.join(__dirname, '..'), 'index.html');
const out = path.resolve(process.env.OUT_DIR || path.join(__dirname, 'outputs/page')); fs.mkdirSync(out, { recursive: true });
const regionsOnly = process.argv.includes('--regions-only');
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

  let page;
  if (!regionsOnly) {
  // ---- 1. the opening view, light and dark, wide and narrow
  page = await open(1440, 1000, 'light');
  await shot(page, '01_open_light_full');
  await shot(page, '02_open_light_stage', '.stage');
  await set(page, { sel: 39 }); await shot(page, '03_pinned_industry', '.stage');
  await set(page, { sel: -1, cols: 'paygroup' }); await shot(page, '04_payment_group_columns', '#chartwrap');
  await set(page, { cols: 'shift' }); await shot(page, '04b_columns_by_shift', '.stage'); await set(page, { cols: 'shift', sel: 39 }); await shot(page, '04c_columns_by_shift_pinned', '.stage'); await set(page, { sel: -1 });
  await set(page, { cols: 'group', lens: 'is8:any' }); await shot(page, '05_highlight_industrial_strategy', '#chartwrap');
  await set(page, { lens: 'pay:Construction' }); await shot(page, '06_highlight_payment_group', '#chartwrap');
  await set(page, { lens: 'fe:both' }); await shot(page, '06b_highlight_foundational', '#chartwrap');
  await set(page, { lens: 'fe:providential', cols: 'ties' }); await shot(page, '06c_columns_by_ties_providential', '.stage');
  await set(page, { lens: 'fe:both', cols: 'ties' }); await shot(page, '06d_columns_by_ties_foundational', '#chartwrap');
  await set(page, { lens: 'none', cols: 'ties' }); await shot(page, '06e_columns_by_ties_no_highlight', '#chartwrap');
  await set(page, { lens: 'fe:providential', cols: 'sells' }); await shot(page, '06f_columns_by_sales_to_providential', '#chartwrap');
  await set(page, { lens: 'fe:both', cols: 'buys' }); await shot(page, '06g_columns_by_purchases_from_foundational', '#chartwrap');
  await set(page, { lens: 'none', cols: 'inside' }); await shot(page, '06h_columns_by_trade_inside', '#chartwrap');
  await set(page, { cols: 'sellsinside', sel: 39 }); await shot(page, '06i_columns_by_sales_inside_pinned', '.stage'); await set(page, { sel: -1 });
  await set(page, { cols: 'buysinside', lens: 'fe:both' }); await shot(page, '06j_columns_by_purchases_inside_foundational', '#chartwrap');
  await set(page, { lens: 'none', cols: 'passed' }); await shot(page, '06k_columns_by_passed_along', '#chartwrap');
  await set(page, { cols: 'inside', sel: 26 }); await shot(page, '06l_pinned_other_manufacturing', '.stage');              // 26: Other manufacturing in Yorkshire and the Humber
  await set(page, { trace: true }); await shot(page, '06m_trace_chain_other_manufacturing', '.stage');
  await set(page, { cols: 'group', sel: 33 }); await shot(page, '06n_trace_chain_building_construction', '#chartwrap'); await set(page, { trace: false, sel: -1 });
  await set(page, { cols: 'passed', show: 'goods', sel: 37 }); await shot(page, '06o_passed_along_goods_view_wholesale', '.stage'); await set(page, { trace: true, sel: 26 }); await shot(page, '06p_trace_goods_view', '.stage'); await set(page, { show: 'money', trace: false, sel: -1 });      // 37: Wholesale
  await set(page, { cols: 'group', show: 'goods', sel: 26 }); await shot(page, '06q_side_panel_partner_chart_goods', '#detail'); await set(page, { show: 'money' }); await shot(page, '06r_side_panel_partner_chart_money', '#detail');
  await page.click('#detail .d-toggle'); await new Promise(r => setTimeout(r, 200)); await shot(page, '06s_side_panel_partner_table', '#detail'); await page.click('#detail .d-toggle'); await set(page, { sel: -1 });
  await set(page, { lens: 'none', cols: 'group' });
  await set(page, { lens: 'none', layout: 'hybrid' }); await shot(page, '07_hybrid', '#chartwrap');
  await set(page, { layout: 'force' }); await shot(page, '08_connections', '#chartwrap');
  await set(page, { layout: 'order', inter: true }); await shot(page, '09_intermediaries_in', '#chartwrap');
  await set(page, { inter: false, halo: true, cover: '0.5' }); await shot(page, '10_halo_fewer_links', '.stage');
  await set(page, { show: 'goods' }); await shot(page, '10b_goods_halo', '.stage');
  await set(page, { halo: false, cover: '0.8' }); await shot(page, '10c_goods_full');
  await set(page, { layout: 'hybrid', sel: 39 }); await shot(page, '10d_goods_hybrid_pinned', '.stage');
  await set(page, { layout: 'order', sel: -1, show: 'money', halo: true, cover: '0.5' });
  await set(page, { halo: false, cover: '0.8', scale: 'amount', t: 3 }); await shot(page, '11_amounts_2019', '.stage');
  await set(page, { scale: 'share', t: 92, pubonly: true }); await shot(page, '12_published_only', '#chartwrap');
  await set(page, { pubonly: false, measure: 'count' }); await shot(page, '13_number_of_payments', '.stage');
  await set(page, { measure: 'value', dir: 'up' }); await shot(page, '13b_up_the_order_only', '#chartwrap');
  await set(page, { dir: 'down' }); await shot(page, '13c_down_the_order_only', '#chartwrap'); await set(page, { dir: 'both' });
  await page.evaluate(() => { document.getElementById('order-note').open = true; }); await shot(page, '13d_order_note', '#order-note'); await page.evaluate(() => { document.getElementById('order-note').open = false; });
  await set(page, { measure: 'value' });
  await page.evaluate(() => { document.getElementById('tablebox').open = true; }); await new Promise(r => setTimeout(r, 200));
  await shot(page, '14_table', '#tablebox');
  // hover a link and a node, for the tooltips
  const box = await (await page.$('#chart')).boundingBox();
  await page.mouse.move(box.x + box.width * 0.46, box.y + box.height * 0.55); await new Promise(r => setTimeout(r, 200)); await shot(page, '15_tooltip_a', '#chartwrap');
  await page.close();

  page = await open(1440, 1000, 'dark'); await shot(page, '16_open_dark_full'); await set(page, { halo: true, cover: '0.5', show: 'goods' }); await shot(page, '16b_dark_goods_halo', '#chartwrap'); await set(page, { halo: false, cover: '0.8', sel: 26 }); await shot(page, '16c_dark_side_panel_partner_chart', '#detail'); await set(page, { sel: -1, show: 'money' }); await set(page, { sel: 39, lens: 'group:Manufacturing' }); await shot(page, '17_dark_pinned_highlight', '.stage'); await page.close();
  page = await open(390, 844, 'light', '', 2); await shot(page, '18_phone_full'); await shot(page, '18b_phone_chart', '#chartwrap'); await page.close();
  page = await open(900, 1100, 'light'); await shot(page, '19_tablet_full'); await page.close();

  // beyond the payments: the section under the chart, the side panel block, and the dotted line for a hidden link that
  // company accounts point to (food manufacturing and retail in Yorkshire and the Humber)
  page = await open(1440, 1000, 'light', '?sel=10');
  await shot(page, '24_hidden_link_dotted_food_manufacturing', '#chartwrap'); await shot(page, '24b_side_panel_beyond_the_payments', '#detail');
  await page.evaluate(() => { document.getElementById('outside').open = true; document.querySelectorAll('#outside details.out-co')[1].open = true; }); await new Promise(r => setTimeout(r, 300));
  await shot(page, '24c_what_the_payments_leave_out', '#outside');
  await set(page, { vouch: false }); await shot(page, '24d_hidden_link_switched_off', '#chartwrap'); await page.close();
  page = await open(1440, 1000, 'dark', '?sel=47&show=goods'); await shot(page, '24e_dark_hidden_link_retail_goods', '#chartwrap'); await page.close();
  // the dotted lines with a real pointer (the hook above never has the pointer over a circle, which once hid them):
  // all show with nothing pinned; a click that leaves the pointer on the circle shows that industry's; hovering a line
  // gives a tooltip and clicking it pins the supplying industry; the switch removes them; a region with none greys the box
  page = await open(1440, 1000, 'dark');
  { const want = (got, exp, what) => { if (got !== exp) problems.push(`dotted lines, ${what}: expected ${exp}, got ${got}`); };
    const dotsNow = () => page.evaluate(() => window.ynhFlows.dottedLinks()), wait = ms => new Promise(r => setTimeout(r, ms));
    await page.$eval('#chart', e => e.scrollIntoView({ block: 'center' })); await wait(300);
    let b; const box = async () => { b = await (await page.$('#chart')).boundingBox(); };          // asked for again after each screenshot, which can scroll the page
    want(await dotsNow(), 2, 'nothing pinned'); want(await page.$eval('#vouch-n', e => e.textContent.trim()), '(2 in this region)', 'count on the tick box');
    await shot(page, '24h_dark_hidden_links_nothing_pinned', '#chartwrap');
    const f = await page.evaluate(() => window.ynhFlows.at('10')); await box();
    await page.mouse.move(b.x + f.x, b.y + f.y); await page.mouse.click(b.x + f.x, b.y + f.y); await wait(300);
    want(await page.$eval('#pick', e => e.value), '10', 'click pins food manufacturing'); want(await dotsNow(), 1, 'pinned by a click, pointer still on the circle');
    await shot(page, '24i_dark_pinned_by_click_pointer_still_on', '#chartwrap');
    await box(); await page.mouse.click(b.x + f.x, b.y + f.y); await page.mouse.move(b.x + 4, b.y + 4); await wait(200);        // unpin, and take the pointer away
    want(await dotsNow(), 2, 'unpinned by a click'); const q = await page.evaluate(() => window.ynhFlows.dottedMid(0));    // food manufacturing to retail: its middle is in the open
    await page.mouse.move(b.x + q.x, b.y + q.y); await wait(200);
    want(await page.$eval('#tip', e => e.style.display !== 'none' && /can’t show/.test(e.textContent)), true, 'tooltip on a dotted line');
    await shot(page, '24j_dark_tooltip_on_dotted_line', '#chartwrap');
    await box(); await page.mouse.click(b.x + q.x, b.y + q.y); await wait(300); want(await page.$eval('#pick', e => e.value), q.supplier, 'clicking a dotted line pins its supplier');
    await page.mouse.move(b.x + 4, b.y + 4); await set(page, { sel: -1, vouch: false }); await wait(200); want(await dotsNow(), 0, 'switched off');
    await set(page, { vouch: true }); await page.evaluate(() => window.ynhFlows.load('TLC')); await wait(400);
    want(await dotsNow(), 0, 'North East, which has none'); want(await page.$eval('#vouch', e => e.disabled), true, 'tick box greyed out where there are none');
    want(await page.$eval('#vouch-n', e => e.textContent.trim()), '(none found in this region)', 'words on the tick box where there are none'); }
  await page.close();
  // positions that follow the year shown (plan section 17). Switched on: height is the year's level, no circles overlap,
  // the other years are worked out behind, and a step across a January slides the circles without working anything out
  // again. Switched off again: every circle is back exactly where it was.
  page = await open(1440, 1000, 'light', '?sel=70&m=2019-06');
  { const want = (got, exp, what) => { if (got !== exp) problems.push(`positions by year, ${what}: expected ${exp}, got ${got}`); };
    const wait = ms => new Promise(r => setTimeout(r, ms)), F = fn => page.evaluate(fn);
    const places = () => F(() => JSON.stringify([...document.querySelectorAll('#pick option')].map(e => e.value).filter(Boolean).map(id => window.ynhFlows.at(id))));
    const off = await places();
    await page.select('#pos', 'year'); await wait(1800);                                              // the slide, then the other years worked out behind it
    want(await F(() => window.ynhFlows.period()), '2019', 'the months in hand'); want(await F(() => document.getElementById('pos-now').textContent), '2019', 'year named beside the switch');
    want(await F(() => document.getElementById('pos-note').hidden), false, 'note under the controls'); want(/pos=year/.test(await F(() => location.search)), true, 'in the address');
    want(await F(() => window.ynhFlows.heightGap() <= 14.5), true, 'height is the year\'s level, within the slack'); want(await F(() => window.ynhFlows.overlaps()), 0, 'overlapping circles');
    want(await places() !== off, true, 'circles moved'); await shot(page, '25_positions_2019_head_offices', '.stage');
    const views = await F(() => window.ynhFlows.viewsWorkedOut()); want(views >= 9, true, `all eight periods worked out ahead (${views} views held)`);
    await set(page, { t: 11 }); await wait(200); const dec = await places();
    await page.click('#next'); await wait(150); want(await F(() => window.ynhFlows.moving()), true, 'stepping into January slides the circles'); await wait(900);
    want(await F(() => window.ynhFlows.period()), '2020', 'the months in hand after the step'); want(await places() !== dec, true, 'circles moved with the year');
    want(await F(() => window.ynhFlows.viewsWorkedOut()), views, 'views held after the step (nothing worked out again)');
    await set(page, { t: 77 }); await wait(300); await shot(page, '25b_positions_2025_head_offices', '.stage');
    await set(page, { t: 92 }); await wait(200); want(await F(() => document.getElementById('pos-now').textContent), '12 months to Sep 2026', 'the part-year named');
    for (const [name, o, pic] of [['hybrid, 2019', { layout: 'hybrid', t: 5, sel: -1 }, '25c_positions_hybrid_2019'], ['hybrid, 2025', { t: 77 }, '25d_positions_hybrid_2025'], ['connections, 2019', { layout: 'force', t: 5 }, '25e_positions_connections_2019'],
      ['connections, 2025', { t: 77 }, '25f_positions_connections_2025'], ['by share passed along, 2019', { layout: 'order', cols: 'passed', t: 5 }, '25g_positions_passed_along_2019'], ['by share passed along, 2025', { t: 77 }, '25h_positions_passed_along_2025'],
      ['free columns', { cols: 'free' }], ['payment groups', { cols: 'paygroup' }], ['intermediaries in', { cols: 'group', inter: true }], ['goods and services', { inter: false, show: 'goods' }]]) {
      await set(page, o); await wait(250); want(await F(() => window.ynhFlows.overlaps()), 0, `overlapping circles, ${name}`);
      if (await F(() => window.ynhFlows.state.layout === 'order')) want(await F(() => window.ynhFlows.heightGap() <= 14.5), true, `height is the year's level, ${name}`);
      if (pic) await shot(page, pic, '#chartwrap'); }
    await F(() => window.ynhFlows.load('TLD')); await wait(700); want(await F(() => window.ynhFlows.period()), '2025', 'kept on switching region'); want(await F(() => window.ynhFlows.heightGap() <= 14.5), true, 'height in another region');
    await F(() => window.ynhFlows.load('TLE')); await wait(500); await set(page, { show: 'money', sel: 59, t: 5 }); await page.select('#pos', 'all'); await wait(1000);
    want(await F(() => document.getElementById('pos-note').hidden), true, 'note gone when switched off'); want(await places(), off, 'switched off, every circle back where it was'); }
  await page.close();
  page = await open(1440, 1000, 'dark', '?pos=year&m=2021-06&sel=24&cols=inside'); await new Promise(r => setTimeout(r, 300)); await shot(page, '25i_dark_positions_2021_trade_inside_pinned', '.stage'); await page.close();
  page = await open(390, 844, 'light', '?pos=year&m=2019-06', 2); await shot(page, '25j_phone_positions_2019'); await page.close();
  page = await open(390, 844, 'light', '?sel=10', 2); await page.evaluate(() => { document.getElementById('outside').open = true; }); await new Promise(r => setTimeout(r, 300)); await shot(page, '24f_phone_what_the_payments_leave_out', '#outside'); await page.close();
  page = await open(1440, 1000, 'light', '?region=TLL&sel=10'); await page.evaluate(() => { document.getElementById('outside').open = true; }); await new Promise(r => setTimeout(r, 300)); await shot(page, '24g_wales_what_the_payments_leave_out', '#outside'); await page.close();

  }
  // ---- 1b. every region that has been built: loads from its own address, with its own text; and switching keeps the view
  page = await open(1440, 1000, 'light');
  const built = await page.evaluate(() => window.FLOWS_REGIONS.regions.filter(r => r.built).map(r => ({ code: r.code, name: r.name, thin: r.thin })));
  await page.close();
  for (const r of built) {
    page = await open(1440, 1000, 'light', '?region=' + r.code);
    const info = await page.evaluate(() => ({ title: document.getElementById('h-title').textContent, links: window.ynhFlows.drawnLinks(), industries: document.getElementById('pick').options.length - 1,
      highlights: document.getElementById('lens').options.length, unfilled: [...document.querySelectorAll('[data-f]')].filter(e => !e.textContent && e.dataset.f !== 'small_example').map(e => e.dataset.f) }));
    console.log(`${r.code}${r.thin ? ' (thin)' : ''}: "${info.title}", ${info.links} links drawn, ${info.industries} industries, ${info.highlights} highlight sets` + (info.unfilled.length ? `, UNFILLED TEXT: ${info.unfilled.join(' ')}` : ''));
    if (info.unfilled.length) problems.push(`${r.code}: text with no figure: ${info.unfilled.join(' ')}`);
    // full set of pictures for the three regions the build was developed on; one picture of the chart for the rest, to keep the folder small
    if (['TLC', 'TLD', 'TLE'].includes(r.code)) {
      await shot(page, `20_${r.code}_full`);
      await page.evaluate(() => { document.getElementById('compare').open = true; document.getElementById('order-note').open = true; }); await new Promise(res => setTimeout(res, 200));
      await shot(page, `21_${r.code}_compare`, '#compare'); await shot(page, `22_${r.code}_order_note`, '#order-note');
      await set(page, { cols: 'paygroup' }); await shot(page, `23_${r.code}_payment_groups`, '#chartwrap');
    } else await shot(page, `20_${r.code}_chart`, '#chartwrap');
    await page.close();
  }
  page = await open(1440, 1000, 'light', '?sel=46&cols=shift&m=2022-06&dir=down');
  for (const r of [...built, built[0]]) { await page.evaluate(c => window.ynhFlows.load(c), r.code); await new Promise(res => setTimeout(res, 500));
    const st = await page.evaluate(() => { const s = window.ynhFlows.state; return `${s.region}: pinned ${document.getElementById('pick').value}, month ${Math.round(s.t)}, cols ${s.cols}, dir ${s.dir}, address ${location.search}`; });
    console.log('switched to', st); }
  await page.close();

  if (!regionsOnly) {
  // ---- 2. every control changes something and nothing throws
  page = await open(1280, 900, 'light');
  const results = [];
  const ids = await page.evaluate(() => [...document.querySelectorAll('.controls select, .timebar select')].map(s => s.id));
  for (const id of ids) { const n = await page.evaluate(i => document.getElementById(i).options.length, id);
    for (let j = 0; j < n; j++) { await page.evaluate((i, jj) => { const s = document.getElementById(i); s.selectedIndex = jj; s.dispatchEvent(new Event('change')); }, id, j); await new Promise(r => setTimeout(r, 30)); }
    await page.evaluate(i => { const s = document.getElementById(i); s.selectedIndex = 0; s.dispatchEvent(new Event('change')); }, id); results.push(`${id}: ${n} options`); }
  for (const id of ['smooth', 'dots', 'inter', 'halo', 'pubonly']) { for (let j = 0; j < 2; j++) { await page.click('#' + id); await new Promise(r => setTimeout(r, 60)); } results.push(id + ': toggled'); }
  for (const v of ['down', 'up', 'both']) { await page.evaluate(val => { const r = document.querySelector(`input[name=dir][value=${val}]`); r.checked = true; r.dispatchEvent(new Event('change')); }, v); await new Promise(r => setTimeout(r, 60));
    results.push(`dir ${v}: ${await page.evaluate(() => window.ynhFlows.drawnLinks())} links`); }
  for (const v of ['hybrid', 'force', 'order']) { await page.evaluate(val => { const r = document.querySelector(`input[name=layout][value=${val}]`); r.checked = true; r.dispatchEvent(new Event('change')); }, v); await new Promise(r => setTimeout(r, 800)); }
  await page.click('#prev'); await page.click('#prev'); await page.click('#next');
  await page.keyboard.press('ArrowLeft'); await page.keyboard.press('Escape');
  const label = await page.evaluate(() => document.getElementById('monthLabel').textContent);
  console.log('controls exercised:', results.join('; '), '| month label now:', label, '| url:', await page.evaluate(() => location.search));
  await page.close();

  }
  // ---- 3. playing
  if (!regionsOnly && !process.argv.includes('--shots-only')) {
    for (const [name, q, dsf] of [['opening view', '', 2], ['every link, both directions, intermediaries in', '?cover=1&net=gross&inter=1', 2], ['positions following the year, through a January', '?pos=year', 2]]) {
      page = await open(1440, 1000, 'light', q, dsf); if (/pos=year/.test(q)) await new Promise(r => setTimeout(r, 1500));      // let the other years be worked out behind, as they would be before anyone presses play
      const r = await page.evaluate(() => new Promise(res => { let n = 0, last = 0, gap = 0; const ms = [], t0 = performance.now(); window.ynhFlows.state.t = 0; window.ynhFlows.play(true);
        (function f(now) { n++; if (last && n > 3) gap = Math.max(gap, now - last); last = now; ms.push(window.ynhFlows.scriptMs()); if (performance.now() - t0 < 5000) requestAnimationFrame(f); else { ms.sort((a, b) => a - b); res({ fps: n / 5, ms: ms[Math.floor(ms.length / 2)], gap, links: window.ynhFlows.drawnLinks(), period: window.ynhFlows.period() }); } })(); }));
      console.log(`playing, ${name}: ${r.links} links, ${r.ms.toFixed(1)} ms of script a frame, ${Math.round(r.fps)} frames a second, longest gap between frames ${Math.round(r.gap)} ms` + (r.period === 'all' ? '' : `, ended in ${r.period}`));
      await page.close();
    }
  }
  await browser.close();
  console.log(problems.length ? 'PROBLEMS:\n  ' + [...new Set(problems)].join('\n  ') : 'no page or console errors');
})().catch(e => { console.error('FAILED', e); process.exit(1); });
