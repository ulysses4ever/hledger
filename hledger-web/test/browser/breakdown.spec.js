// Browser tests for the breakdown page: the period's money as a ring and
// a list, drilled into from the kinds to the categories to the accounts,
// and from an account with nothing under it to its register.
const { test, expect } = require('@playwright/test');
const { watchPageErrors } = require('./helpers');

const names = page => page.locator('.breakdown-list .breakdown-name').allTextContents();

test('the breakdown drills from the kinds to the categories to the register', async ({ page }) => {
  const errors = watchPageErrors(page);
  await page.goto('/breakdown');
  await expect(page.locator('.period-band .period-length')).toBeVisible();
  await expect(page.locator('svg.donut circle.donut-slice')).toHaveCount(3);
  expect(await names(page)).toEqual(['Income', 'Transfer', 'Expense']);
  await page.locator('.breakdown-list a', { hasText: 'Expense' }).click();
  await expect(page).toHaveURL(/\/breakdown\?kind=expenses/);
  expect((await names(page)).map(n => n.replace(/\s+/g, ''))).toEqual(['expenses:food', 'expenses:transport']);
  await expect(page.locator('.breakdown-path .current')).toHaveText('Expense');
  await page.locator('.breakdown-list a').first().click();
  await expect(page).toHaveURL(/kind=expenses&acct=expenses%3Afood/);
  await expect(page.locator('.donut-centre')).toHaveText('food');
  const parts = (await names(page)).map(n => n.replace(/\s+/g, ''));
  expect(parts[0]).toBe('expenses:food:groceries');
  // an account with nothing under it opens its register for the period
  await page.locator('.breakdown-list a').first().click();
  await expect(page).toHaveURL(/\/register\?q=inacct%3Aexpenses%3Afood%3Agroceries&period=all/);
  expect(errors).toEqual([]);
});

test('the way back up is a path of links, and the period band steps the breakdown', async ({ page }) => {
  await page.goto('/breakdown?kind=expenses&acct=expenses%3Afood&period=2025-02');
  await expect(page.locator('.period-name')).toHaveText('February 2025');
  expect(await page.locator('.breakdown-path a').allTextContents()).toEqual(['Reports', 'Expense']);
  await page.locator('.period-band a.period-prev').click();
  await expect(page).toHaveURL(/kind=expenses&acct=expenses%3Afood/);
  await expect(page).toHaveURL(/period=2025-01/);
  await page.locator('.breakdown-path a', { hasText: 'Reports' }).click();
  await expect(page).toHaveURL(/\/breakdown\?period=2025-01/);
});
