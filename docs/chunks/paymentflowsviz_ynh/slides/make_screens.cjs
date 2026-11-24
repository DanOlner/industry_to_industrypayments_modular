// make_screens.cjs
// Takes the screenshots the deck uses (chains_deck.qmd) from the page itself, at fixed settings, into img/.
// All in the goods view and the light theme. Re-run after the page changes, then re-render the deck.
// Needs puppeteer-core and Google Chrome, as tests/13_page_checks.cjs does:
//   NODE_PATH=/path/to/node_modules node docs/chunks/paymentflowsviz_ynh/slides/make_screens.cjs
const puppeteer = require('puppeteer-core'), path = require('path'), fs = require('fs');
const url = 'file://' + path.resolve(__dirname, '..', 'index.html'), out = path.join(__dirname, 'img'); fs.mkdirSync(out, { recursive: true });
const shots = [                                   // name, address: the same addresses the deck links to
  ['overview', '?show=goods'],
  ['other_manufacturing_trace', '?show=goods&sel=32&trace=1'],
  ['education_trace', '?show=goods&sel=85&trace=1'],
  ['wholesale_pinned', '?show=goods&sel=46'],
];
(async () => {
  const browser = await puppeteer.launch({ executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: 'new', args: ['--allow-file-access-from-files'] });
  const problems = [];
  for (const [name, q] of shots) { const page = await browser.newPage();
    page.on('pageerror', e => problems.push(name + ': ' + e.message));
    await page.emulateMediaFeatures([{ name: 'prefers-color-scheme', value: 'light' }]); await page.setViewport({ width: 1440, height: 1000, deviceScaleFactor: 2 });
    await page.goto(url + q, { waitUntil: 'load' }); await page.waitForFunction(() => window.ynhFlows && window.ynhFlows.drawnLinks() > 0, { timeout: 20000 });
    await new Promise(r => setTimeout(r, 1500));                                                 // let the layout settle
    await (await page.$('#chartwrap')).screenshot({ path: path.join(out, name + '.png') }); console.log('saved', name); await page.close(); }
  console.log(problems.length ? problems.join('\n') : 'no page errors'); await browser.close();
})();
