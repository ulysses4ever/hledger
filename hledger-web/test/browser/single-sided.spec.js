// Browser tests for the journal's single-sided view.
//
// The journal shows each entry single-sided: the postings whose amounts are
// all negative, where the money came from, are named on a muted "from"
// line under the others, without their amounts. Ticking "Show both sides"
// shows every posting with its amount, as before; the choice is kept in a
// cookie, which the server reads, so that a page is drawn that way from the
// start (see section 12 of hledger.css, and hledger.js).
//
// Run with: npx playwright test single-sided  (see README.md)
const { test, expect } = require('@playwright/test');
const { watchPageErrors } = require('./helpers');

// The rows of the entry with this description that are in sight: whether
// each is a "from" line, the accounts it names, and its amount.
function entry(page, description) {
  return page.locator('#main-content tr.title', { hasText: description }).evaluate(title => {
    const shown = el => getComputedStyle(el).display !== 'none';
    const rows = [];
    // single-sided, the entry sits on the title row itself: its two
    // accounts (from, to) in cells of their own, or its main posting
    const mains = [...title.querySelectorAll('td.account.main-posting')].filter(shown);
    if (mains.length)
      rows.push({
        from: false,
        accounts: mains.flatMap(td => [...td.querySelectorAll('a[title]')].map(a => a.title)),
        amount: title.querySelector('td.amount.main-posting').textContent.trim(),
      });
    for (let r = title.nextElementSibling; r && !r.classList.contains('title'); r = r.nextElementSibling)
      if (shown(r))
        rows.push({
          from: r.classList.contains('from'),
          accounts: [...r.querySelectorAll('a[title]')].map(a => a.title),
          amount: r.querySelector('td.amount').textContent.trim(),
        });
    return rows;
  });
}

// Cafe Luna: 18.00 to expenses:food:dining, from assets:bank:checking:
// single-sided, one line, from → to, with the amount.
const single = [
  { from: false, accounts: ['assets:bank:checking', 'expenses:food:dining'], amount: '18.00' },
];
const both = [
  { from: false, accounts: ['expenses:food:dining'], amount: '18.00' },
  { from: false, accounts: ['assets:bank:checking'], amount: '-18.00' },
];

test('the journal is single-sided, and shows both sides when asked, remembering that', async ({ page }) => {
  const errors = watchPageErrors(page);
  await page.goto('/journal');
  const box = page.locator('#bothsides');
  await expect(box).toBeVisible();
  await expect(box).not.toBeChecked();
  expect(await entry(page, 'Cafe Luna')).toEqual(single);
  // the from line is muted
  const colors = await page.locator('#main-content tr.title', { hasText: 'Cafe Luna' }).evaluate(title => {
    const from = title.parentElement.querySelector(`tr.from`);
    return [getComputedStyle(from.querySelector('.from-label')).color, getComputedStyle(title.querySelector('td.description')).color];
  });
  expect(colors[0]).not.toEqual(colors[1]);

  await box.check();
  expect(await entry(page, 'Cafe Luna')).toEqual(both);
  // the choice is kept: the server draws the next page that way
  expect(await (await page.request.get('/journal')).text()).toMatch(/<table class="[^"]*\bboth-sides\b/);
  await page.reload();
  await expect(box).toBeChecked();
  expect(await entry(page, 'Cafe Luna')).toEqual(both);

  await box.uncheck();
  await page.reload();
  await expect(box).not.toBeChecked();
  expect(await entry(page, 'Cafe Luna')).toEqual(single);
  expect(errors).toEqual([]);
});

test.describe('without javascript', () => {
  test.use({ javaScriptEnabled: false });

  test('the journal is single-sided, and the box, which could not keep its choice, is not shown', async ({ page }) => {
    await page.goto('/journal');
    await expect(page.locator('#bothsides')).toBeHidden();
    expect(await entry(page, 'Cafe Luna')).toEqual(single);
  });
});
