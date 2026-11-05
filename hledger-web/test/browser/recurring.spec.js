// Browser tests for the recurring rules: the page lists them, a rule is
// added with the add form and a schedule, then deleted.
const { test, expect } = require('@playwright/test');
const fs = require('fs');
const { watchPageErrors } = require('./helpers');

test('a rule is added, listed with its next date, and deleted', async ({ page }) => {
  const errors = watchPageErrors(page);
  await page.goto('/recurring');
  await page.locator('a', { hasText: 'Add a rule' }).click();
  await expect(page).toHaveURL(/\/recurring-new$/);
  await page.locator('#addform select[name=schedule]').selectOption({ label: 'Every month' });
  await page.locator('#addform input[name=date]').fill('2025-02-03');
  await page.locator('#addform input[name=description]').fill('E2eRule');
  await page.locator('#addform input[name=description]').press('Tab');
  await page.locator('#addform .category-select').selectOption('expenses:transport');
  await page.locator('#addform select[name=account]').nth(0).selectOption('expenses:transport:transit');
  await page.locator('#addform input[name=amount]').nth(0).fill('21.50');
  await page.locator('#addform select[name=account]').nth(1).selectOption('assets:bank:checking');
  await page.locator('#addform button[type=submit]').click();
  await expect(page).toHaveURL(/\/recurring$/);
  await expect(page.locator('#message')).toContainText('Rule saved.');
  const rule = page.locator('.recurring-list li', { hasText: 'E2eRule' });
  await expect(rule).toContainText('Every month');
  await expect(rule).toContainText('Next:');
  expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).toContain('~ monthly from 2025-02-03  E2eRule');
  await rule.locator('a').click();
  await expect(page.locator('#addform input[name=description]')).toHaveValue('E2eRule');
  page.once('dialog', d => d.accept());
  await page.locator('#deleteform button[type=submit]').click();
  await expect(page.locator('#message')).toContainText('Rule deleted.');
  expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).not.toContain('E2eRule');
  expect(errors).toEqual([]);
});
