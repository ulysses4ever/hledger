// Browser tests for the journal on a phone, single-sided: each entry is one
// block, as a phone money app shows it (section 15 of hledger.css). Its
// first line says what it was for and how much, red when spent, green when
// earned; its second, its date and day, and the account the money came
// from or went to; its third, its payee and note. "Show both sides" and a
// wide screen show the entries as before.
const { test, expect } = require('@playwright/test');
const { watchPageErrors } = require('./helpers');

const phone = { width: 390, height: 844 };

// The entry whose description has this text.
const entry = (page, text) => page.locator('#main-content tr.title', { hasText: text });

// The value of one of hledger.css's color properties, as getComputedStyle
// gives colors.
const colour = (page, name) => page.evaluate(n => {
  const probe = document.createElement('span');
  probe.style.color = `var(${n})`;
  document.body.append(probe);
  const c = getComputedStyle(probe).color;
  probe.remove();
  return c;
}, name);

// The top edge of an element, to say which share a line.
const top = async loc => Math.round((await loc.boundingBox()).y);

for (const scheme of ['light', 'dark']) {
  test(`each entry is a block of three lines, its amount colored by what it did (${scheme})`, async ({ page }) => {
    const errors = watchPageErrors(page);
    await page.setViewportSize(phone);
    await page.emulateMedia({ colorScheme: scheme });
    await page.goto('/journal');

    // a payment: the expense's account, its amount spent, the account it
    // was paid from, and the description. (An account name's text is the
    // whole name; the top level and the colons are only out of sight.)
    const meal = entry(page, 'Cafe Luna');
    const card = meal.locator('td.entry-card');
    await expect(card).toBeVisible();
    await expect(meal.locator('td.description')).toBeHidden();
    await expect(card.locator('.entry-what')).toHaveText(/^\s*expenses:food:dining\s*$/);
    await expect(card.locator('.entry-what .leaf')).toHaveCSS('font-weight', '600');
    await expect(card.locator('.entry-amount')).toHaveText('-18.00');
    await expect(card.locator('.entry-amount')).toHaveCSS('color', await colour(page, '--negative'));
    await expect(card.locator('.entry-date')).toHaveText('2025-02-14 (Fri)');
    await expect(card.locator('.entry-accounts')).toHaveText(/^\s*assets:bank:checking\s*$/);
    await expect(card.locator('.entry-accounts')).toHaveCSS('color', await colour(page, '--entry-account'));
    await expect(card.locator('.entry-memo')).toHaveText('Cafe Luna');
    // line 1: what and how much; line 2: when and where; line 3: who and why
    const line1 = await top(card.locator('.entry-what')), line2 = await top(card.locator('.entry-date')),
      line3 = await top(card.locator('.entry-memo'));
    expect(await top(card.locator('.entry-amount'))).toEqual(line1);
    expect(await top(card.locator('.entry-accounts'))).toEqual(line2);
    expect(line2).toBeGreaterThan(line1);
    expect(line3).toBeGreaterThan(line2);
    // the amount and the account sit at the right
    const right = async loc => { const b = await loc.boundingBox(); return Math.round(b.x + b.width); };
    expect(await right(card.locator('.entry-amount'))).toEqual(await right(card.locator('.entry-accounts')));

    // income: its account, the amount earned, in green with a plus, and the
    // account it went to; payee and note between a dot
    const pay = entry(page, 'payroll').locator('td.entry-card');
    await expect(pay.locator('.entry-what')).toHaveText(/^\s*income:salary\s*$/);
    await expect(pay.locator('.entry-amount')).toHaveClass(/earning/);
    await expect(pay.locator('.entry-amount')).toHaveText('3200.00');
    expect(await pay.locator('.entry-amount').evaluate(e => getComputedStyle(e, '::before').content)).toBe('"+"');
    await expect(pay.locator('.entry-amount')).toHaveCSS('color', await colour(page, '--income'));
    await expect(pay.locator('.entry-memo')).toHaveText('Employer Inc · payroll');

    // a transfer: "Transfer", in the text's color, from one account to the other
    const opening = entry(page, 'opening balances').locator('td.entry-card');
    await expect(opening.locator('.entry-what')).toHaveText('Transfer');
    await expect(opening.locator('.entry-amount')).toHaveText('1000.00');
    await expect(opening.locator('.entry-amount')).toHaveCSS('color',
      await page.locator('#main-content').evaluate(e => getComputedStyle(e).color));
    await expect(opening.locator('.entry-accounts')).toHaveText(/^\s*from\s*equity:opening\s*→\s*to\s*assets:bank:checking\s*$/);

    // the accounts link to their registers, the date to its day
    await expect(card.locator('.entry-what a')).toHaveAttribute('href', /register\?q=inacct%3Aexpenses%3Afood%3Adining/);
    await expect(card.locator('.entry-date')).toHaveAttribute('href', /journal\?q=date%3A2025-02-14/);
    expect(errors).toEqual([]);
  });
}

test('"Show both sides" and a wide screen show the entries as before', async ({ page }) => {
  await page.setViewportSize(phone);
  await page.goto('/journal');
  const meal = entry(page, 'Cafe Luna');
  await page.locator('#bothsides').check();
  await expect(meal.locator('td.entry-card')).toBeHidden();
  await expect(meal.locator('td.description')).toBeVisible();
  await expect(page.locator('#main-content tr.posting td.account', { hasText: 'dining' }).first()).toBeVisible();
  await page.locator('#bothsides').uncheck();
  await expect(meal.locator('td.entry-card')).toBeVisible();
  await page.setViewportSize({ width: 1280, height: 800 });
  await expect(meal.locator('td.entry-card')).toBeHidden();
  await expect(meal.locator('td.description')).toBeVisible();
  await expect(page.locator('#main-content tr.posting td.account', { hasText: 'dining' }).first()).toBeVisible();
});

test('without an instance stylesheet, the icon slot takes no room', async ({ page }) => {
  await page.setViewportSize(phone);
  await page.goto('/journal');
  const card = entry(page, 'Cafe Luna').locator('td.entry-card');
  const icon = await card.locator('.entry-icon').boundingBox();
  expect(icon === null || icon.width === 0).toBe(true);
  const cardBox = await card.boundingBox(), what = await card.locator('.entry-what').boundingBox();
  expect(Math.round(what.x)).toEqual(Math.round(cardBox.x));
});
