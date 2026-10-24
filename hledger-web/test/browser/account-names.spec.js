// Browser tests for how the pages show account names.
//
// In the journal, the register and the statements, a name is shown
// without its top level when it has other parts (food › dining for
// expenses:food:dining), with › between the parts and the parents set back
// from the leaf; the top level and the colons stay in the text, out of
// sight. The balance report and the sidebar show every part (see
// accountNameAsHtml, and section 11 of hledger.css). Run with:
// npx playwright test account-names  (see README.md)
const { test, expect } = require('@playwright/test');
const { watchPageErrors } = require('./helpers');

// In the page: what each account link in sight in the main pane shows, its
// text, and its title. What it shows is the text of the parts in sight, with each
// separator as drawn.
function accountLinks(selector = '#main-content td.account a, #main-content td.from-accounts a') {
  const shown = el => {
    let s = '';
    for (const n of el.childNodes) {
      if (n.nodeType === Node.TEXT_NODE) { s += n.textContent; continue; }
      if (n.nodeType !== Node.ELEMENT_NODE) continue;
      const r = n.getBoundingClientRect();
      if (r.width <= 1 && r.height <= 1) continue;  // out of sight
      if (n.classList.contains('sep')) {
        s += getComputedStyle(n, '::before').content.replace(/^"(.*)"$/, '$1').replace(/\u00a0/g, ' ');  // a no-break space reads as a space
        continue;
      }
      s += shown(n);
    }
    return s;
  };
  return [...document.querySelectorAll(selector)]
    .filter(a => a.getClientRects().length).map(a => ({
    title: a.title, text: a.textContent.trim(), shown: shown(a.querySelector('.account-name')).trim(),
  }));
}

const expected = {
  'expenses:food:dining': 'food › dining',
  'expenses:food:groceries': 'food › groceries',
  'assets:bank:checking': 'bank › checking',
  'income:salary': 'salary',
  'equity:opening': 'opening',
};

for (const [page_, url] of [['journal', '/journal'], ['register', '/register?q=inacct:assets:bank:checking']]) {
  test(`the ${page_} shows account names without their top level, with › between the parts`, async ({ page }) => {
    const errors = watchPageErrors(page);
    await page.goto(url);
    const links = await page.evaluate(accountLinks);
    expect(links.length).toBeGreaterThan(0);
    for (const link of links) {
      // the full name is the title, and the text, for copying
      expect(link.text).toBe(link.title);
      // no colon in sight
      expect(link.shown, link.title).not.toContain(':');
      if (link.title in expected) expect(link.shown).toBe(expected[link.title]);
    }
    // the names the fixture gives each page
    const shown = links.map(l => l.shown);
    for (const name of page_ === 'journal'
      ? ['food › dining', 'bank › checking', 'opening', 'salary']
      : ['food › dining', 'food › groceries', 'opening', 'salary'])
      expect(shown).toContain(name);
    // the parents are set back from the leaf
    const colors = await page.locator('#main-content td.account .account-name').first().evaluate(n => {
      const leaf = n.querySelector('.leaf'), parent = n.querySelector('.parents, .sep');
      return [getComputedStyle(leaf).color, parent && getComputedStyle(parent).color];
    });
    if (colors[1]) expect(colors[0]).not.toEqual(colors[1]);
    expect(errors).toEqual([]);
  });
}

// In the page: what the account links a selector picks out show, as above.
async function shownNames(page, url, selector) {
  await page.goto(url);
  const links = await page.evaluate(accountLinks, selector);
  expect(links.length, url).toBeGreaterThan(0);
  for (const link of links) expect(link.shown, `${url}: ${link.text}`).not.toContain(':');
  return links.map(l => l.shown);
}

const reportRows = '#main-content tbody td.account a';

test('the statements show account names without the top level their sections name', async ({ page }) => {
  const errors = watchPageErrors(page);
  expect(await shownNames(page, '/incomestatement', reportRows))
    .toEqual(expect.arrayContaining(['salary', 'food › dining', 'food › groceries', 'transport › transit']));
  expect(await shownNames(page, '/balancesheet', reportRows)).toContain('bank › checking');
  expect(errors).toEqual([]);
});

test('the balance report and the sidebar show account names in full, with › between the parts', async ({ page }) => {
  const errors = watchPageErrors(page);
  expect(await shownNames(page, '/balance', reportRows))
    .toEqual(expect.arrayContaining(['assets › bank › checking', 'expenses › food › dining', 'income › salary']));
  // the sidebar is a tree: a row shows the account's own part, or a few
  // parts when its parents have nothing else in them
  expect(await shownNames(page, '/journal', '#sidebar-menu a.acct-name'))
    .toEqual(expect.arrayContaining(['assets › bank › checking', 'expenses', 'food', 'groceries', 'transport › transit']));
  expect(errors).toEqual([]);
});

test('on a phone, a report row wraps an account name that does not fit, after a › rather than before it', async ({ page }) => {
  const errors = watchPageErrors(page);
  await page.setViewportSize({ width: 320, height: 568 });
  await page.goto('/balance');
  const r = await page.evaluate(() => {
    const link = [...document.querySelectorAll('#main-content tbody td.account a')]
      .find(a => a.textContent === 'expenses:food:groceries');
    // a cell too narrow for the name on one line
    const cell = link.closest('td');
    cell.style.width = cell.style.maxWidth = '100px';
    const name = link.querySelector('.account-name'), leaf = name.querySelector('.leaf');
    const left = cell.getBoundingClientRect().left + parseFloat(getComputedStyle(cell).paddingLeft);
    return {
      lines: Math.round(name.getBoundingClientRect().height / leaf.getBoundingClientRect().height),
      sepsStartingLines: [...name.querySelectorAll('.sep')].filter(s => s.getBoundingClientRect().left < left + 1).length,
      past: name.getBoundingClientRect().right - cell.getBoundingClientRect().right,
    };
  });
  expect(r.lines).toBeGreaterThan(1);
  expect(r.sepsStartingLines).toBe(0);
  expect(r.past).toBeLessThanOrEqual(0);
  expect(errors).toEqual([]);
});
