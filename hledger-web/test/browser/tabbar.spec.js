// Browser tests for the tab bar along the foot of a phone's screen (section
// 16 of hledger.css): Journal, Accounts (the balance sheet), a round + to
// add a transaction, Reports (the income statement), and Search. It marks
// the tab of the page shown, stands in for the sidebar's toggle button and
// the journal's add button, never covers the page, and is hidden at 768px
// and wider.
const { test, expect } = require('@playwright/test');
const { watchViolations, watchPageErrors, expectNoViolations } = require('./helpers');

const phone = { width: 390, height: 844 };
const bar = page => page.locator('#tabbar');
const tab = (page, name) => bar(page).getByRole('link', { name, exact: true });

test('the tabs have names, and the one for the page shown is marked', async ({ page }) => {
  await page.setViewportSize(phone);
  await page.goto('/journal');
  await expect(bar(page)).toBeVisible();
  await expect(bar(page)).toHaveAccessibleName('Pages');
  const names = await bar(page).getByRole('link').evaluateAll(as => as.map(a => a.getAttribute('aria-label') || a.textContent.trim()));
  expect(names).toEqual(['Journal', 'Accounts', 'Add a transaction', 'Reports', 'Search']);
  await expect(tab(page, 'Journal')).toHaveAttribute('aria-current', 'page');
  await expect(bar(page).locator('[aria-current]')).toHaveCount(1);
  // an account's register is under Accounts
  await page.goto('/register?q=inacct:assets:bank:checking');
  await expect(tab(page, 'Accounts')).toHaveAttribute('aria-current', 'page');
});

test('the tabs lead to the journal, the balance sheet and the income statement', async ({ page }) => {
  const violations = await watchViolations(page);
  const errors = watchPageErrors(page);
  await page.setViewportSize(phone);
  await page.goto('/journal');
  await tab(page, 'Accounts').click();
  await expect(page).toHaveURL(/\/balancesheet$/);
  await expect(tab(page, 'Accounts')).toHaveAttribute('aria-current', 'page');
  await tab(page, 'Reports').click();
  await expect(page).toHaveURL(/\/incomestatement$/);
  await expect(tab(page, 'Reports')).toHaveAttribute('aria-current', 'page');
  await tab(page, 'Journal').click();
  await expect(page).toHaveURL(/\/journal$/);
  await expect(tab(page, 'Journal')).toHaveAttribute('aria-current', 'page');
  expect(errors).toEqual([]);
  await expectNoViolations(page, violations);
});

test('the + opens the add dialog, or on a page without it the add page', async ({ page }) => {
  const errors = watchPageErrors(page);
  await page.setViewportSize(phone);
  await page.goto('/journal');
  // it stands in for the journal's own button
  await expect(page.locator('#addformlink')).toBeHidden();
  const add = tab(page, 'Add a transaction');
  await expect(add).toBeVisible();
  await add.click();
  await expect(page.locator('#addmodal')).toBeVisible();
  await page.keyboard.press('Escape');
  await expect(page.locator('#addmodal')).toBeHidden();
  await page.goto('/balancesheet');
  await tab(page, 'Add a transaction').click();
  await expect(page).toHaveURL(/\/add$/);
  await expect(page.locator('#addform')).toBeVisible();
  await expect(tab(page, 'Add a transaction')).toHaveAttribute('aria-current', 'page');
  expect(errors).toEqual([]);
});

test('Search opens the search form and puts the cursor in it', async ({ page }) => {
  await page.setViewportSize(phone);
  await page.goto('/journal');
  const form = page.locator('#searchform');
  await expect(form).toBeHidden();
  await tab(page, 'Search').click();
  await expect(form).toBeVisible();
  await expect(form.locator('input[name=q]')).toBeFocused();
  // a search in force keeps it in sight, so that it can be seen and cleared
  await page.goto('/journal?q=' + encodeURIComponent('desc:grocer'));
  await expect(form).toBeVisible();
  // an account's register, browsed to, is not a search
  await page.goto('/register?q=inacct:assets:bank:checking');
  await expect(form).toBeHidden();
});

test('the sidebar toggle gives way to the bar, which never covers the page', async ({ page }) => {
  await page.setViewportSize(phone);
  await page.goto('/journal');
  await expect(page.locator('#spacer button[data-toggle="offcanvas"]')).toBeHidden();
  // scrolled to the end, the last entry is above the bar
  await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
  const last = await page.locator('#main-content tr.title').last().boundingBox();
  const barTop = (await bar(page).boundingBox()).y;
  expect(last.y + last.height).toBeLessThanOrEqual(barTop);
});

test('at 1280px there is no tab bar, and the sidebar, search form and add button are as before', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.goto('/journal');
  await expect(bar(page)).toBeHidden();
  await expect(page.locator('#searchform')).toBeVisible();
  await expect(page.locator('#addformlink')).toBeVisible();
  await expect(page.locator('#sidebar-menu')).toBeVisible();
});

test.describe('without javascript', () => {
  test.use({ javaScriptEnabled: false });
  test('Search still shows the search form', async ({ page }) => {
    await page.setViewportSize(phone);
    await page.goto('/journal');
    await expect(page.locator('#searchform')).toBeHidden();
    await tab(page, 'Search').click();
    await expect(page).toHaveURL(/#searchform$/);
    await expect(page.locator('#searchform')).toBeVisible();
  });
});
