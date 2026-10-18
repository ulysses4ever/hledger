// Browser tests at a phone's width (#340).
//
// Below 768px wide, where the sidebar goes off-canvas, the journal and the
// register stack each row, the register leaves out its running balance,
// the add form shares its rows out differently, and a report with one
// column of amounts lays out each row by itself (see section 10 of
// hledger.css). These tests check what a reader holding a phone would
// notice: a page wider than the screen, a table scrolling sideways inside
// it, an amount out of view, an account name's leaf cut off, a field too
// narrow for what it holds.
//
// Run with: npx playwright test phone  (see README.md)
const { test, expect } = require('@playwright/test');
const { watchPageErrors } = require('./helpers');

// Phones held upright: a common width (390px, eg iPhone 12 to 16), and the
// narrowest still in use (320px, eg iPhone SE).
const phones = [{ width: 390, height: 844 }, { width: 320, height: 568 }];

// The pages a phone shows in full, with nothing to scroll sideways.
const pages = {
  'journal': '/journal',
  'register': '/register?q=inacct:assets:bank:checking',
  'register of all accounts': '/register',
  'add': '/add',
  'balance report': '/balance',
  'balance sheet': '/balancesheet',
  'income statement': '/incomestatement',
};

// In the page: what would have a reader scroll sideways, or miss a figure.
// The page wider than the window; anything in the main pane that scrolls
// sideways by itself; an amount that ends past the window's edge.
function sideways() {
  const name = el => el.tagName.toLowerCase() + (el.id ? '#' + el.id : '') +
    [...el.classList].map(c => '.' + c).join('');
  const found = [];
  const width = document.documentElement.scrollWidth;
  if (width > window.innerWidth) found.push(`the page is ${width}px wide`);
  for (const el of document.querySelectorAll('#main-content, #main-content *')) {
    if (!el.getClientRects().length) continue;
    if (['auto', 'scroll'].includes(getComputedStyle(el).overflowX) && el.scrollWidth > el.clientWidth)
      found.push(`${name(el)} scrolls sideways`);
    if (el.classList.contains('amount') && el.textContent.trim() &&
        el.getBoundingClientRect().right > window.innerWidth)
      found.push(`${name(el)} "${el.textContent.trim()}" ends past the edge`);
  }
  return found;
}

for (const phone of phones) {
  test(`at ${phone.width}px wide, no page scrolls sideways or hides an amount`, async ({ page }) => {
    const errors = watchPageErrors(page);
    await page.setViewportSize(phone);
    const found = [];
    for (const [view, url] of Object.entries(pages)) {
      await page.goto(url);
      expect.soft(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth),
        `${view} is no wider than the window`).toBe(true);
      found.push(...(await page.evaluate(sideways)).map(f => `${view}: ${f}`));
    }
    // A report with a column per period has more columns than fit, and
    // scrolls sideways inside its table; the page itself does not.
    await page.goto('/balance?period=monthly');
    expect.soft(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth),
      'a monthly balance report is no wider than the window').toBe(true);
    expect(found).toEqual([]);
    expect(errors).toEqual([]);
  });
}

// The top edges of some cells of a row, to say which share a line.
async function tops(row, selectors) {
  const out = {};
  for (const s of selectors) out[s] = Math.round((await row.locator(s).first().boundingBox()).y);
  return out;
}

test('the journal and register stack their rows, and the running balance is left out', async ({ page }) => {
  await page.setViewportSize(phones[0]);

  // a journal entry: the date and description on a line, then a line per
  // posting, its account beside its amount
  await page.goto('/journal');
  await expect(page.locator('#main-content .transactionsreport thead')).toBeHidden();
  const title = await tops(page.locator('#main-content tr.title', { hasText: 'Cafe Luna' }), ['td.date', 'td.description']);
  expect(title['td.date']).toEqual(title['td.description']);
  const posting = page.locator('#main-content tr.title', { hasText: 'Cafe Luna' }).locator('xpath=following-sibling::tr[1]');
  const line = await tops(posting, ['td.account', 'td.amount']);
  expect(line['td.account']).toEqual(line['td.amount']);
  expect(line['td.account']).toBeGreaterThan(title['td.date']);

  // a register row: the date and description, then the other account and
  // the amount; no running balance, and no column headings
  await page.goto('/register?q=inacct:assets:bank:checking');
  await expect(page.locator('#main-content .registerreport thead')).toBeHidden();
  await expect(page.locator('#main-content .registerreport td.balance').first()).toBeHidden();
  const row = page.locator('#main-content .registerreport tbody tr', { hasText: 'Cafe Luna' });
  const cells = await tops(row, ['td.date', 'td.description', 'td.account', 'td.amount:not(.balance)']);
  expect(cells['td.date']).toEqual(cells['td.description']);
  expect(cells['td.account']).toEqual(cells['td.amount:not(.balance)']);
  expect(cells['td.account']).toBeGreaterThan(cells['td.date']);

  // on a wide screen it is a table again, with the running balance
  await page.setViewportSize({ width: 1280, height: 800 });
  await expect(page.locator('#main-content .registerreport td.balance').first()).toBeVisible();
  await expect(page.locator('#main-content .registerreport thead')).toBeVisible();
});

test('account names keep their leaf whole, and their full name as the title', async ({ page }) => {
  await page.setViewportSize(phones[1]);
  for (const url of ['/journal', '/register?q=inacct:assets:bank:checking']) {
    await page.goto(url);
    const links = await page.locator('#main-content td.account a').evaluateAll(as => as.map(a => {
      const leaf = a.querySelector('.leaf');
      return {
        title: a.title,
        leaf: leaf.textContent,
        cut: leaf.scrollWidth > leaf.clientWidth ||
          leaf.getBoundingClientRect().right > a.closest('td').getBoundingClientRect().right + 0.5,
      };
    }));
    expect(links.length).toBeGreaterThan(0);
    for (const link of links) {
      // the parents may be cut short, with an ellipsis, but not the leaf
      expect(link.cut, `${url}: ${link.title}`).toBe(false);
      expect(link.title.endsWith(link.leaf), `${url}: ${link.title} ends with ${link.leaf}`).toBe(true);
    }
  }
});

test("the add form's fields have room for what they hold", async ({ page }) => {
  for (const phone of phones) {
    await page.setViewportSize(phone);
    await page.goto('/journal');
    await page.locator('body').press('a');
    await expect(page.locator('#addmodal')).toBeVisible();
    const tooNarrow = await page.locator('#addform').evaluate(form => {
      // A field has room for a text if, typed in, it would not scroll.
      const fits = (input, text) => {
        const kept = input.value;
        input.value = text;
        const fit = input.scrollWidth <= input.clientWidth;
        input.value = kept;
        return fit;
      };
      const found = [];
      for (const input of form.querySelectorAll('input[name=amount]'))
        if (!fits(input, input.placeholder)) found.push(`"${input.placeholder}"`);
      if (!fits(form.querySelector('input[name=date]'), '2025-12-31')) found.push('a date');
      return found;
    });
    expect(tooNarrow, `at ${phone.width}px`).toEqual([]);
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  }
});

// flot draws the register chart to the width it has, so the chart is drawn
// again when that changes; otherwise a phone turned from landscape to
// upright would keep a chart wider than the page.
test('the register chart is drawn again to fit when a phone is turned upright', async ({ page }) => {
  const errors = watchPageErrors(page);
  await page.setViewportSize({ width: phones[0].height, height: phones[0].width });
  await page.goto('/register?q=inacct:assets:bank:checking');
  await expect(page.locator('#register-chart canvas').first()).toBeVisible();
  await page.setViewportSize(phones[0]);
  await expect.poll(() => page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  await expect(page.locator('#register-chart-label .legend-item')).toHaveCount(1);
  expect(errors).toEqual([]);
});

// Do two boxes overlap?
const overlap = (a, b) =>
  a.x < b.x + b.width && b.x < a.x + a.width && a.y < b.y + b.height && b.y < a.y + a.height;

test('on a phone, a round button in the corner adds a transaction', async ({ page }) => {
  const errors = watchPageErrors(page);
  await page.setViewportSize(phones[0]);
  await page.goto('/journal');
  const button = page.locator('#addbutton');
  await expect(button).toBeVisible();
  await expect(button).toHaveAccessibleName('Add a transaction');
  // it stands in for the journal's own button
  await expect(page.locator('#addformlink')).toBeHidden();
  // it opens the add dialog, as the a key still does
  await button.click();
  await expect(page.locator('#addmodal')).toBeVisible();
  await page.keyboard.press('Escape');
  await expect(page.locator('#addmodal')).toBeHidden();
  await page.locator('body').press('a');
  await expect(page.locator('#addmodal')).toBeVisible();
  await page.keyboard.press('Escape');
  // scrolled to the end, the page leaves room for it below the last amount
  await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
  const last = await page.locator('#main-content td.amount .amount').last().boundingBox();
  expect(overlap(last, await button.boundingBox())).toBe(false);
  // on a page without the dialog, it opens the add page
  await page.goto('/balancesheet');
  await page.locator('#addbutton').click();
  await expect(page).toHaveURL(/\/add$/);
  await expect(page.locator('#addform')).toBeVisible();
  await expect(page.locator('#addbutton')).toHaveCount(0);
  // on a wide screen, the journal has a button of its own instead
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.goto('/journal');
  await expect(page.locator('#addbutton')).toBeHidden();
  await expect(page.locator('#addformlink')).toBeVisible();
  expect(errors).toEqual([]);
});
