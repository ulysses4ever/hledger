// Browser tests for --css, an instance's own stylesheet (see the manual's
// "Customising the look"): it applies to the pages without breaking their
// Content-Security-Policy, and can rearrange the journal page's parts with
// the order property. This spec starts its own hledger-web, with --css.
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
  ].join('\n'));
  ({ child: server, url: URL } = await startServer(
    ['-f', journal, '--serve', '--host', '127.0.0.1', '--port', '0', '--css', path.join(tmpdir, 'custom.css')]));
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
