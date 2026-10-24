// Browser tests for an account's page, the register of one account, as
// phone money apps show an account (section 17 of hledger.css): its balance
// at the end of the period, the balance before it and what was paid in and
// out, and on a phone, the rows grouped under a heading per day with the
// day's total, each with the balance after it.
const { test, expect } = require('@playwright/test');
const { watchPageErrors } = require('./helpers');

const phone = { width: 390, height: 844 };
// February 2025 in the fixture's checking account: payroll in, a weekly
// shop and Cafe Luna out, after January's opening balance, shop and transit.
const FEB = '/register?q=' + encodeURIComponent('inacct:assets:bank:checking') + '&period=2025-02';

const colour = (page, name) => page.evaluate(n => {
  const probe = document.createElement('span');
  probe.style.color = `var(${n})`;
  document.body.append(probe);
  const c = getComputedStyle(probe).color;
  probe.remove();
  return c;
}, name);

for (const width of [phone.width, 1280]) {
  test(`the page shows the balance, and the period's summary, at ${width}px`, async ({ page }) => {
    const errors = watchPageErrors(page);
    await page.setViewportSize({ width, height: 844 });
    await page.goto(FEB);
    // 1000.00 - 52.10 - 21.50 before February; + 3200.00 - 48.75 - 18.00 in it
    await expect(page.locator('.account-balance')).toHaveText('4059.65');
    await expect(page.locator('.account-balance')).not.toHaveClass(/negative/);
    const summary = page.locator('.account-summary');
    await expect(summary.locator('dt')).toHaveText(['Previous balance', 'Paid in', 'Spent']);
    await expect(summary.locator('dd')).toHaveText(['926.40', '3200.00', '-66.75']);
    await expect(summary.locator('dd.earning')).toHaveCSS('color', await colour(page, '--income'));
    await expect(summary.locator('dd.spending')).toHaveCSS('color', await colour(page, '--negative'));
    // no net line
    await expect(summary.locator('dt')).toHaveCount(3);
    expect(errors).toEqual([]);
  });
}

test('a negative balance is red', async ({ page }) => {
  await page.goto('/register?q=' + encodeURIComponent('inacct:income:salary') + '&period=2025-02');
  await expect(page.locator('.account-balance')).toHaveText('-3200.00');
  await expect(page.locator('.account-balance')).toHaveCSS('color', await colour(page, '--negative'));
});

test('on a phone, the rows come under a heading per day, with its total', async ({ page }) => {
  await page.setViewportSize(phone);
  await page.goto(FEB);
  const headings = page.locator('.registerreport tr.day-heading');
  await expect(headings.locator('.day-date')).toHaveText(['2025-02-14 (Fri)', '2025-02-03 (Mon)', '2025-02-01 (Sat)']);
  await expect(headings.locator('.day-total')).toHaveText(['-18.00', '-48.75', '3200.00']);
  await expect(headings.first()).toBeVisible();
  // each heading is followed by its day's rows
  const after = headings.nth(1).locator('xpath=following-sibling::tr[1]');
  await expect(after).toContainText('Grocer Green');
});

test("on a phone, each row says what it was for, its amount, its payee and note, and the balance after it", async ({ page }) => {
  await page.setViewportSize(phone);
  await page.goto(FEB);
  const row = name => page.locator('.registerreport tbody tr', { hasText: name }).locator('td.entry-card');
  const shop = row('weekly shop');
  await expect(shop).toBeVisible();
  await expect(shop.locator('.entry-what')).toHaveText(/^\s*expenses:food:groceries\s*$/);
  await expect(shop.locator('.entry-amount')).toHaveText('-48.75');
  await expect(shop.locator('.entry-amount')).toHaveCSS('color', await colour(page, '--negative'));
  await expect(shop.locator('.entry-memo')).toHaveText('Grocer Green · weekly shop');
  // the running balance, from before the period: small, secondary text
  await expect(shop.locator('.entry-balance')).toHaveText(/»\s*4077\.65/);
  await expect(shop.locator('.entry-balance')).toBeVisible();
  const size = await shop.locator('.entry-balance').evaluate(e => parseFloat(getComputedStyle(e).fontSize));
  expect(size).toBeLessThan(16);
  await expect(row('payroll').locator('.entry-amount')).toHaveCSS('color', await colour(page, '--income'));
  await expect(row('payroll').locator('.entry-balance')).toHaveText(/»\s*4126\.40/);
  // the newest row's balance is the one at the top
  await expect(row('Cafe Luna').locator('.entry-balance')).toHaveText(/»\s*4059\.65/);
  // the same in the historical mode
  await page.goto(FEB + '&accum=historical');
  await expect(row('weekly shop').locator('.entry-balance')).toHaveText(/»\s*4077\.65/);
  await expect(page.locator('.account-summary dd').first()).toHaveText('926.40');
});

test('on a wide screen, the table is as before, without the headings and blocks', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.goto(FEB);
  await expect(page.locator('.registerreport tr.day-heading').first()).toBeHidden();
  await expect(page.locator('.registerreport td.entry-card').first()).toBeHidden();
  await expect(page.locator('.registerreport tbody tr', { hasText: 'Cafe Luna' }).locator('td.balance')).toBeVisible();
});
