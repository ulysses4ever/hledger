// Browser tests for how the journal and the register show account names.
//
// A name is shown without its top level when it has other parts
// (food › dining for expenses:food:dining), with › between the parts and
// the parents set back from the leaf; the full name is the link's title,
// and the top level and the colons stay in the text, out of sight (see
// accountNameAsHtml, and section 11 of hledger.css). Run with:
// npx playwright test account-names  (see README.md)
const { test, expect } = require('@playwright/test');
const { watchPageErrors } = require('./helpers');

// In the page: what each account link in the main pane shows, its text, and
// its title. What it shows is the text of the parts in sight, with each
// separator as drawn.
function accountLinks() {
  const shown = el => {
    let s = '';
    for (const n of el.childNodes) {
      if (n.nodeType === Node.TEXT_NODE) { s += n.textContent; continue; }
      if (n.nodeType !== Node.ELEMENT_NODE) continue;
      const r = n.getBoundingClientRect();
      if (r.width <= 1 && r.height <= 1) continue;  // out of sight
      if (n.classList.contains('sep')) {
        s += getComputedStyle(n, '::before').content.replace(/^"(.*)"$/, '$1');
        continue;
      }
      s += shown(n);
    }
    return s;
  };
  return [...document.querySelectorAll('#main-content td.account a')].map(a => ({
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
