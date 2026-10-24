// Browser tests for --css, an instance's own stylesheet (see the manual's
// "Customising the look"): it applies to the pages without breaking their
// Content-Security-Policy, can rearrange the journal page's parts with the
// order property, and can give the entries icons by their account. This spec
// starts its own hledger-web, with --css.
const { test, expect } = require('@playwright/test');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { startServer } = require('./server');
const { watchViolations, watchPageErrors, expectNoViolations } = require('./helpers');

let server, URL, tmpdir;

test.beforeAll(async () => {
  // starting the server can take longer than the 30s hook timeout, eg on a cold stack
  test.setTimeout(120000);
  tmpdir = fs.mkdtempSync(path.join(os.tmpdir(), 'hledger-web-css-'));
  const journal = path.join(tmpdir, 'css.journal');
  fs.copyFileSync(path.join(__dirname, 'fixture.journal'), journal);
  // the manual's example, which moves the years and the page links below
  // the entries, and a rule to see the stylesheet apply
  fs.writeFileSync(path.join(tmpdir, 'custom.css'), [
    '.journal-entries { order: 1; }',
    '.journal-years, .journal-paging-top { order: 2; }',
    '.journal-paging-bottom { display: none; }',
    '.journal-heading { letter-spacing: 3px; }',
    // the manual's icons, by account and by kind
    '[data-account^="expenses:food" i] .entry-icon::before { content: "F"; }',
    '[data-kind="moving"] .entry-icon::before { content: "T"; }',
  ].join('\n'));
  ({ child: server, url: URL } = await startServer(
    ['-f', journal, '--serve', '--host', '127.0.0.1', '--port', '0', '--all-time', '--css', path.join(tmpdir, 'custom.css')]));
});

test.afterAll(() => {
  try { if (server) process.kill(server.pid); } catch (e) { /* already gone */ }
  try { fs.rmSync(tmpdir, { recursive: true, force: true }); } catch (e) { /* fine */ }
});

test('an instance stylesheet applies, within the policy, and can rearrange the journal', async ({ page }) => {
  const violations = await watchViolations(page);
  const errors = watchPageErrors(page);
  await page.goto(URL + '/journal');
  await expect(page.locator('#main-content h2')).toHaveCSS('letter-spacing', '3px');
  // the years row (the fixture spans one year, so it is empty) and the
  // paging come after the entries now; the add button stays above them
  const top = sel => page.locator(sel).evaluate(e => e.getBoundingClientRect().top);
  expect(await top('.journal-years')).toBeGreaterThan(await top('.journal-entries'));
  expect(await top('.journal-paging-top')).toBeGreaterThan(await top('.journal-entries'));
  expect(await top('#addformlink')).toBeLessThan(await top('.journal-entries'));
  await expect(page.locator('.journal-paging-bottom')).toBeHidden();
  await expectNoViolations(page, violations);
  expect(errors).toEqual([]);
});

test('the rows name their main account and kind, for a stylesheet to give them icons', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(URL + '/journal');
  // every entry names an account
  const accounts = await page.locator('#main-content tr.title').evaluateAll(rows => rows.map(r => r.dataset.account));
  expect(accounts.length).toBeGreaterThan(0);
  for (const a of accounts) expect(a).toMatch(/\S/);
  const meal = page.locator('#main-content tr.title', { hasText: 'Cafe Luna' });
  await expect(meal).toHaveAttribute('data-account', 'expenses:food:dining');
  await expect(meal).toHaveAttribute('data-kind', 'spending');
  const pay = page.locator('#main-content tr.title', { hasText: 'payroll' });
  await expect(pay).toHaveAttribute('data-account', 'income:salary');
  await expect(pay).toHaveAttribute('data-kind', 'earning');
  const opening = page.locator('#main-content tr.title', { hasText: 'opening balances' });
  await expect(opening).toHaveAttribute('data-account', 'assets:bank:checking');
  await expect(opening).toHaveAttribute('data-kind', 'moving');
  // the stylesheet's icons, before what each entry was for
  const icon = row => row.locator('.entry-icon').evaluate(e => getComputedStyle(e, '::before').content);
  expect(await icon(meal)).toBe('"F"');
  expect(await icon(opening)).toBe('"T"');
  expect(await icon(pay)).toBe('none');
  const x = async (row, sel) => (await row.locator(sel).boundingBox()).x;
  expect(await x(meal, '.entry-what')).toBeGreaterThan(await x(meal, 'td.entry-card') + 30);
  expect(Math.round(await x(pay, '.entry-what'))).toEqual(Math.round(await x(pay, 'td.entry-card')));

  // in an account's register, the account is the one other than the register's
  await page.goto(URL + '/register?q=' + encodeURIComponent('inacct:assets:bank:checking'));
  const row = page.locator('#main-content .registerreport tbody tr', { hasText: 'Cafe Luna' });
  await expect(row).toHaveAttribute('data-account', 'expenses:food:dining');
  await expect(row).toHaveAttribute('data-kind', 'spending');
  await expect(page.locator('#main-content .registerreport tbody tr', { hasText: 'opening balances' }))
    .toHaveAttribute('data-account', 'equity:opening');
});
