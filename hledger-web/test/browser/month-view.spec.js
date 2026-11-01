// Browser tests for the month view: the journal and an account's register
// open on today's month, with a band naming it between links to the months
// before and after, and on the journal, how much its entries spent and
// earned. This spec starts its own hledger-web, without --all-time and with
// --today, so that "this month" is February 2025 in the fixture.
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
  tmpdir = fs.mkdtempSync(path.join(os.tmpdir(), 'hledger-web-month-'));
  const journal = path.join(tmpdir, 'month.journal');
  fs.copyFileSync(path.join(__dirname, 'fixture.journal'), journal);
  ({ child: server, url: URL } = await startServer(
    ['-f', journal, '--serve', '--host', '127.0.0.1', '--port', '0', '--today', '2025-02-14']));
});

test.afterAll(() => {
  try { if (server) process.kill(server.pid); } catch (e) { /* already gone */ }
  try { fs.rmSync(tmpdir, { recursive: true, force: true }); } catch (e) { /* fine */ }
});

const entries = page => page.locator('.journal-entries tr.title');

test('the journal opens on this month, and its arrows step a month at a time', async ({ page }) => {
  const violations = await watchViolations(page);
  const errors = watchPageErrors(page);
  await page.goto(URL + '/journal');
  const band = page.locator('.period-band');
  await expect(band.locator('.period-name')).toHaveText('February 2025');
  // payroll, a weekly shop, Cafe Luna
  await expect(entries(page)).toHaveCount(3);
  await expect(band.locator('a.period-prev')).toHaveAccessibleName('Show the period before');
  await expect(band.locator('a.period-next')).toHaveAccessibleName('Show the period after');

  await band.locator('a.period-prev').click();
  await expect(page).toHaveURL(/period=2025-01/);
  await expect(band.locator('.period-name')).toHaveText('January 2025');
  await expect(entries(page)).toHaveCount(3);

  await band.locator('a.period-next').click();
  await band.locator('a.period-next').click();
  await expect(band.locator('.period-name')).toHaveText('March 2025');
  await expect(entries(page)).toHaveCount(1);
  expect(errors).toEqual([]);
  await expectNoViolations(page, violations);
});

test('the band says what the month spent and earned, in red and green', async ({ page }) => {
  await page.goto(URL + '/journal');
  const totals = page.locator('.period-band .period-totals');
  // groceries 48.75 and dining 18.00; the payroll's 3200
  await expect(totals).toHaveText(/spent\s*66\.75\s*·\s*earned\s*3200\.00/);
  const colour = name => page.evaluate(n => {
    const probe = document.createElement('span');
    probe.style.color = `var(${n})`;
    document.body.append(probe);
    const c = getComputedStyle(probe).color;
    probe.remove();
    return c;
  }, name);
  await expect(totals.locator('.spent')).toHaveCSS('color', await colour('--negative'));
  await expect(totals.locator('.earned')).toHaveCSS('color', await colour('--income'));
  // and January's: the groceries and transit, and no income
  await page.goto(URL + '/journal?period=2025-01');
  await expect(totals).toHaveText(/spent\s*73\.60\s*·\s*earned\s*0/);
});

test('a date in the search replaces the month, and All shows every date', async ({ page }) => {
  await page.goto(URL + '/journal?q=' + encodeURIComponent('date:2025-01'));
  await expect(page.locator('.period-name')).toHaveText('January 2025');
  await expect(entries(page)).toHaveCount(3);
  await page.goto(URL + '/journal');
  await page.locator('.period-band .period-length').selectOption({ label: 'All' });
  await expect(page).toHaveURL(/period=all/);
  await expect(page.locator('.period-name')).toHaveText('All dates');
  await expect(page.locator('.period-band a')).toHaveCount(0);
  await expect(entries(page)).toHaveCount(7);
  // a search shows all its matches, whatever the month
  await page.goto(URL + '/journal?q=' + encodeURIComponent('desc:grocer'));
  await expect(page.locator('.period-name')).toHaveText('All dates');
  await expect(entries(page)).toHaveCount(2);
});

test("an account's register opens on this month too, and keeps the account as it steps", async ({ page }) => {
  await page.goto(URL + '/register?q=' + encodeURIComponent('inacct:assets:bank:checking'));
  await expect(page.locator('.period-name')).toHaveText('February 2025');
  await expect(page.locator('.registerreport tbody tr:not(.day-heading)')).toHaveCount(3);
  await page.locator('.period-band a.period-prev').click();
  await expect(page).toHaveURL(/q=inacct%3Aassets%3Abank%3Achecking&period=2025-01/);
  await expect(page.locator('.period-name')).toHaveText('January 2025');
});

test('the length of the period switches between week, month, quarter and year, keeping the account', async ({ page }) => {
  await page.goto(URL + '/register?q=' + encodeURIComponent('inacct:assets:bank:checking'));
  const length = page.locator('.period-band .period-length');
  await expect(length).toHaveValue(/period=2025-02$/);
  await length.selectOption({ label: 'Year' });
  await expect(page).toHaveURL(/q=inacct%3Aassets%3Abank%3Achecking&period=2025$/);
  await expect(page.locator('.period-name')).toHaveText('2025');
  await page.locator('.period-band .period-length').selectOption({ label: 'Week' });
  await expect(page).toHaveURL(/period=2025-02-10\.\.2025-02-17/);
  await expect(page.locator('.period-name')).toHaveText('10 Feb – 16 Feb 2025');
  await page.locator('.period-band a.period-next').click();
  await expect(page.locator('.period-name')).toHaveText('17 Feb – 23 Feb 2025');
});

for (const width of [390, 320]) {
  test(`the band fits a phone ${width}px wide`, async ({ page }) => {
    await page.setViewportSize({ width, height: 800 });
    for (const p of ['/journal', '/register?q=inacct:assets:bank:checking']) {
      await page.goto(URL + p);
      const over = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
      expect(over, p).toBeLessThanOrEqual(0);
      const band = await page.locator('.period-band').boundingBox();
      expect(band.x + band.width, p).toBeLessThanOrEqual(width);
    }
  });
}
