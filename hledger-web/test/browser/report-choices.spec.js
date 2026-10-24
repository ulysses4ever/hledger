// Browser tests for a report's choices on a phone: the rows of links above
// a report (Report, Section, Show, Interval) give way to drop-down lists,
// side by side, and picking a choice goes to it (see linksRow in
// Widget/Common.hs, section 10 of hledger.css, and hledger.js). Run with:
// npx playwright test report-choices  (see README.md)
const { test, expect } = require('@playwright/test');
const { watchPageErrors } = require('./helpers');

const lists = page => page.locator('#main-content .report-choice select');
const picked = (page, n) => lists(page).nth(n).locator('option:checked');

test.describe('on a phone', () => {
  test.use({ viewport: { width: 390, height: 844 } });

  test('a report\'s rows of links are drop-down lists, side by side, showing the page\'s choices', async ({ page }) => {
    const errors = watchPageErrors(page);
    await page.goto('/balancesheet');
    for (const row of await page.locator('#main-content .report-choices .report-links').all())
      await expect(row).toBeHidden();
    await expect(lists(page)).toHaveCount(3);
    const [a, b] = [await lists(page).nth(0).boundingBox(), await lists(page).nth(1).boundingBox()];
    expect(Math.abs(a.y - b.y)).toBeLessThan(1);
    await expect(page.locator('#main-content .report-choice-name')).toHaveText(['Report', 'Section', 'Interval']);
    await expect(picked(page, 0)).toHaveText('Balance sheet');
    await expect(picked(page, 1)).toHaveText('Assets');
    await expect(picked(page, 2)).toHaveText('None');
    expect(errors).toEqual([]);
  });

  test('picking a report or an interval goes to it', async ({ page }) => {
    const errors = watchPageErrors(page);
    await page.goto('/balancesheet');
    await lists(page).nth(0).selectOption({ label: 'Income statement' });
    await expect(page).toHaveURL(/\/incomestatement$/);
    await lists(page).nth(2).selectOption({ label: 'Monthly' });
    await expect(page).toHaveURL(/\/incomestatement\?period=monthly$/);
    await expect(picked(page, 2)).toHaveText('Monthly');
    expect(errors).toEqual([]);
  });

  test('a statement shows one section at a time, untitled, and all of them when asked', async ({ page }) => {
    const errors = watchPageErrors(page);
    const rows = page.locator('#main-content tbody td.account a');
    await page.goto('/balancesheet');
    await expect(page.locator('#main-content tr.section')).toHaveCount(0);
    await expect(rows.filter({ hasText: 'assets:bank:checking' })).toHaveCount(1);
    await expect(rows.filter({ hasText: 'equity:' })).toHaveCount(0);
    await page.goto('/balancesheetequity');
    await lists(page).nth(1).selectOption({ label: 'Equity' });
    await expect(page).toHaveURL(/\/balancesheetequity\?section=equity$/);
    await expect(rows.filter({ hasText: 'equity:opening' })).toHaveCount(1);
    await expect(rows.filter({ hasText: 'assets:' })).toHaveCount(0);
    await lists(page).nth(1).selectOption({ label: 'All' });
    await expect(page).toHaveURL(/\/balancesheetequity\?section=all$/);
    await expect(page.locator('#main-content tr.section')).toHaveCount(3);
    await expect(picked(page, 1)).toHaveText('All');
    // the income statement opens on its expenses
    await page.goto('/incomestatement');
    await expect(picked(page, 1)).toHaveText('Expenses');
    await expect(rows.filter({ hasText: 'income:salary' })).toHaveCount(0);
    expect(errors).toEqual([]);
  });

  test('the balance report\'s modes are a list too', async ({ page }) => {
    await page.goto('/balance');
    await expect(page.locator('#main-content .report-choice-name')).toHaveText(['Report', 'Show', 'Interval']);
    await lists(page).nth(1).selectOption({ label: 'Ending balances' });
    await expect(page).toHaveURL(/\/balance\?accum=historical$/);
    await expect(picked(page, 1)).toHaveText('Ending balances');
  });

  test('going back shows the earlier page with its own choice', async ({ page }) => {
    await page.goto('/balancesheet');
    await lists(page).nth(0).selectOption({ label: 'Income statement' });
    await expect(page).toHaveURL(/\/incomestatement$/);
    await page.goBack();
    await expect(page).toHaveURL(/\/balancesheet$/);
    await expect(picked(page, 0)).toHaveText('Balance sheet');
  });
});

test.describe('on a phone without javascript', () => {
  test.use({ viewport: { width: 390, height: 844 }, javaScriptEnabled: false });

  test('the rows of links stay, as nothing would make a list go anywhere', async ({ page }) => {
    await page.goto('/balancesheet');
    await expect(page.locator('#main-content .report-links').first()).toBeVisible();
    await expect(page.locator('#main-content .report-choice').first()).toBeHidden();
  });
});

test('on a wider screen, the rows of links show and the lists do not', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.goto('/balancesheet');
  await expect(page.locator('#main-content .report-links').first()).toBeVisible();
  await expect(page.locator('#main-content .report-choice').first()).toBeHidden();
});
