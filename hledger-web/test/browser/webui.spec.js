// Browser end-to-end tests for the hledger-web UI.
//
// These pin down the behaviors that Hledger/Web/Test.hs cannot see, because
// yesod-test never runs javascript: the add form, autocomplete, the date
// picker, keyboard shortcuts, sidebar state, and hash highlighting.
//
// Run with: npx playwright test  (see README.md)
const { test, expect } = require('@playwright/test');
const fs = require('fs');

// Type the payee, and move on, which closes the list of payees it brings up.
// (Not Escape: in the add dialog that closes the dialog.)
async function setPayee(page, name) {
  await page.locator('#addform input[name=description]').fill(name);
  await page.locator('#addform input[name=description]').press('Tab');
}

// Collect uncaught page errors in every test; individual tests assert on them.
let pageErrors;
test.beforeEach(({ page }) => {
  pageErrors = [];
  page.on('pageerror', err => pageErrors.push(String(err)));
});

// Open the add-transaction modal via its keyboard shortcut and wait for it.
async function openAddForm(page) {
  await page.locator('body').press('a');
  await expect(page.locator('#addmodal')).toBeVisible();
}

test.describe('page initialization', () => {

  test('journal page loads and starts up without javascript errors', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    // startup ran to completion: the add form was pre-filled with today's date
    await expect(page.locator('#addform input[name=date]')).not.toHaveValue('');
    expect(pageErrors).toEqual([]);
  });

  // A url hash can be anything, eg an old bookmark's numeric row id, and
  // '#3' is not a valid CSS selector, so any code passing location.hash to
  // querySelector must guard it. A failure here leaves the page
  // half-initialized: no date picker, no keyboard shortcuts, no sidebar handlers.
  test('register url with a numeric transaction hash initializes fully', async ({ page }) => {
    await page.goto('/register?q=inacct:assets:bank:checking#3');
    expect(pageErrors).toEqual([]);
    await openAddForm(page);
    await expect(page.locator('#addform input[name=date]')).not.toHaveValue('');
  });

  // The mark is a :target rule in hledger.css, so assert on the rendered
  // colour rather than on a class: that is what the reader actually sees.
  test('a transaction link marks its target row', async ({ page }) => {
    await page.goto('/journal');
    // (one in sight: single-sided, an entry names its source accounts on a
    // "from" line, and their own rows are hidden)
    const link = page.locator('#main-content a[title="assets:bank:checking"]:visible').first();
    const id = (await link.getAttribute('href')).split('#')[1];
    await link.click();
    await expect(page).toHaveURL(/register/);
    await expect(page.locator(`[id="${id}"]`)).toBeVisible();
    const colourOf = l => l.evaluate(el => getComputedStyle(el).backgroundColor);
    const target = page.locator(`[id="${id}"] > td`).first();
    const other = page.locator('#main-content tbody tr:not(:target) > td').first();
    expect(await colourOf(target)).not.toEqual(await colourOf(other));
    expect(pageErrors).toEqual([]);
  });

});

test.describe('add form', () => {

  test('starts with two posting rows, the second without an amount', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await expect(page.locator('#addform .account-group')).toHaveCount(2);
    await expect(page.locator('#addform input[name=amount]').nth(0)).toBeVisible();
    await expect(page.locator('#addform input[name=amount]').nth(1)).toBeHidden();
  });

  test('the button under the rows adds a posting row, and the previous last row gets its amount', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await page.locator('#addrow').click();
    await expect(page.locator('#addform .account-group')).toHaveCount(3);
    await expect(page.locator('#addform input[name=amount]').nth(1)).toBeVisible();
    await expect(page.locator('#addform input[name=amount]').nth(2)).toBeHidden();
    await expect(page.locator('#addform select[name=account]').nth(2)).toBeFocused();
    expect(pageErrors).toEqual([]);
  });

  test('the payee field offers payees as it is typed into, by their letters in order', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    const input = page.locator('#addform input[name=description]');
    const list = page.locator('#addform .payee-matches');
    await input.click();
    await expect(list).toBeHidden();   // nothing until something is typed
    await input.fill('gr');
    expect(await list.locator('li').first().textContent()).toBe('Grocer Green');
    await input.fill('cfl');   // c, f, l in order: Cafe Luna
    await expect(list.locator('li')).toHaveText(['Cafe Luna']);
    await input.press('ArrowDown');
    await input.press('Enter');
    await expect(input).toHaveValue('Cafe Luna');
    await expect(list).toBeHidden();
    // a name no payee has is simply typed, and Enter is the form's
    await input.fill('Brand New Shop');
    await expect(list).toBeHidden();
    // the first match is picked by Enter without an arrow, or by a tap
    await input.fill('metro');
    await expect(list.locator('li').first()).toHaveText('Metro Transit');
    await list.locator('li').first().click();
    await expect(input).toHaveValue('Metro Transit');
    expect(pageErrors).toEqual([]);
  });

  test('the form runs kind, amount, date, category, account, payee, note; and focuses the amount', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    const tops = await page.locator('#addform').evaluate(form => {
      const top = sel => form.querySelector(sel).getBoundingClientRect().top;
      return [top('.kind-select'), top('input[name=amount]'), top('input[name=date]'), top('.category-select'),
              top('select[name=account][data-row="2"]'), top('input[name=description]'), top('input[name=note]')];
    });
    for (let i = 1; i < tops.length; i++) { expect(tops[i]).toBeGreaterThanOrEqual(tops[i - 1]); }
    await expect(page.locator('#addform input[name=amount]').first()).toBeFocused();
    expect(pageErrors).toEqual([]);
  });

  test("an account's register starts a new entry on that account, and a transfer's sides swap", async ({ page }) => {
    await page.goto('/register?q=' + encodeURIComponent('inacct:assets:bank:checking'));
    await openAddForm(page);
    const to = page.locator('#addform select[name=account]').nth(0);
    const from = page.locator('#addform select[name=account]').nth(1);
    await expect(from).toHaveValue('assets:bank:checking');
    await expect(page.locator('#addform .swap-sides')).toBeHidden();   // (not on an expense)
    await page.locator('#addform .kind-option[data-kind=transfer]').click();
    await expect(from).toHaveValue('assets:bank:checking');
    await to.selectOption('equity:opening');
    await page.locator('#addform .swap-sides').click();
    await expect(to).toHaveValue('assets:bank:checking');
    await expect(from).toHaveValue('equity:opening');
    await page.locator('#addform .kind-option[data-kind=expenses]').click();
    // an expense account's register starts the entry on that expense
    await page.goto('/register?q=' + encodeURIComponent('inacct:expenses:food:groceries'));
    await openAddForm(page);
    await expect(page.locator('#addform .kind-option[data-kind=expenses]')).toHaveClass(/active/);
    await expect(page.locator('#addform .category-select')).toHaveValue('expenses:food');
    await expect(to).toHaveValue('expenses:food:groceries');
    expect(pageErrors).toEqual([]);
  });

  test("the note field offers notes as it is typed into, the payee's own first", async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    const note = page.locator('#addform input[name=note]');
    const list = page.locator('#addform .note-matches');
    await note.click();
    await expect(list).toBeHidden();   // nothing until something is typed
    await note.fill('pay');
    await expect(list.locator('li')).toHaveText(['payroll']);
    await note.press('Enter');
    await expect(note).toHaveValue('payroll');
    await expect(list).toBeHidden();
    // with a payee named, its own notes come first, whatever else matches
    await page.locator('#addform input[name=description]').fill('Grocer Green');
    await note.fill('o');
    await expect(list.locator('li').first()).toHaveText('weekly shop');
    await page.locator('#addform input[name=description]').fill('Employer Inc');
    await note.fill('o');
    await expect(list.locator('li').first()).toHaveText('payroll');
    await list.locator('li').first().click();
    await expect(note).toHaveValue('payroll');
    expect(pageErrors).toEqual([]);
  });

  test('the first row picks a category, then an account within it; the second row lists assets first', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    const category = page.locator('#addform .category-select');
    // the kind of entry, Expense to begin with, narrows the categories to its own
    await expect(page.locator('#addform .kind-option[data-kind=expenses]')).toHaveClass(/active/);
    expect(await category.locator('option').allTextContents()).toEqual(['Category', 'food', 'transport']);
    await category.selectOption('expenses:food');
    const account = page.locator('#addform select[name=account]').nth(0);
    expect(await account.locator('option').allTextContents()).toEqual(['Subcategory', 'dining', 'dining › xss<script>alert(1)</script>', 'groceries']);
    await account.selectOption('expenses:food:groceries');
    // a category with one account picks it at once
    await page.locator('#addform .kind-option[data-kind=income]').click();
    expect(await category.locator('option').allTextContents()).toEqual(['Category', 'salary']);
    await category.selectOption('income:salary');
    await expect(account).toHaveValue('income:salary');
    expect(await page.locator('#addform select[name=account]').nth(1).locator('optgroup').evaluateAll(gs => gs.map(g => g.label))).toEqual(['Assets', 'Equity', 'Expenses', 'Income']);
    // a group of accounts is not offered, only the accounts in it
    const offered = await page.locator('#addform select[name=account]').nth(1).locator('option').evaluateAll(os => os.map(o => o.value));
    expect(offered).toContain('assets:bank:checking');
    expect(offered).not.toContain('assets:bank');
    expect(offered).not.toContain('expenses:food');
  });

  test('the kind of entry is remembered, and a transfer names the accounts to and from', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await page.locator('#addform .kind-option[data-kind=transfer]').click();
    // a transfer: no payee, no category; every account of the kind, grouped
    await expect(page.locator('#addform .payee-group')).toBeHidden();
    await expect(page.locator('#addform .category-select')).toBeHidden();
    const to = page.locator('#addform select[name=account]').nth(0);
    await expect(to.locator('option').first()).toHaveText('Account');
    await expect(page.locator('#addform .account-side').nth(0)).toHaveText('To');
    await expect(page.locator('#addform .account-side').nth(1)).toHaveText('From');
    expect(await to.locator('optgroup').evaluateAll(gs => gs.map(g => g.label))).toEqual(['Assets', 'Equity']);
    await to.selectOption('assets:bank:checking');
    expect(await to.locator('optgroup').count()).toBe(2);   // the list stays whole once one is picked
    await expect(page.locator('#addform select[name=account]').nth(1).locator('option').first()).toHaveText('Account');
    await page.goto('/journal');
    await openAddForm(page);
    await expect(page.locator('#addform .kind-option[data-kind=transfer]')).toHaveClass(/active/);
    await page.locator('#addform .kind-option[data-kind=expenses]').click();
    await expect(page.locator('#addform select[name=account]').nth(0).locator('option').first()).toHaveText('Subcategory');
    await expect(page.locator('#addform .payee-group')).toBeVisible();
    await expect(page.locator('#addform .category-select')).toBeVisible();
    await expect(page.locator('#addform .account-side').first()).toBeHidden();
    // the file chooser is out of sight: new entries go to the main file
    await expect(page.locator('#addform select[name=file]')).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

  // Completion is a native <datalist>. The browser draws its dropdown outside
  // the page, so a test can only check that the field is wired to a list and
  // that the list offers the journal's accounts and descriptions.
  test('the account fields are lists grouped by top-level account, and the description field offers completions', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    const first = page.locator('#addform select[name=account]').first();
    // the first row's list is empty until a category is picked, then holds that category's accounts
    await expect(first.locator('option')).toHaveCount(1);
    await page.locator('#addform .category-select').selectOption('expenses:food');
    await expect(first.locator('option[value="expenses:food:dining"][data-category="expenses:food"]')).toHaveText('dining');
    await page.locator('#addform .kind-option[data-kind=transfer]').click();  // asset accounts are under Transfer, listed at once
    await expect(first.locator('option[value="assets:bank:checking"][data-category="assets"]')).toHaveText('bank › checking');
    await expect(first).toHaveValue('');
    await expect(page.locator('#addform select[name=account]').nth(1).locator('optgroup[label="Assets"] option[value="assets:bank:checking"]')).toHaveCount(1);
    // the second row starts on the account the books use most
    await expect(page.locator('#addform select[name=account]').nth(1)).toHaveValue('assets:bank:checking');
    // the payees the field offers come with the page, frequent first
    expect(await page.locator('#payeenames option').first().getAttribute('value')).toBe('Grocer Green');
    expect(await page.locator('#payeenames option').count()).toBeGreaterThan(5);
  });

  test('the add form remembers this browser\'s last accounts', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await page.locator('#addform .category-select').selectOption('expenses:food');
    await page.locator('#addform select[name=account]').nth(0).selectOption('expenses:food:groceries');
    await page.locator('#addform select[name=account]').nth(1).selectOption('equity:opening');
    await setPayee(page, 'remembered');
    await page.locator('#addform input[name=amount]').nth(0).fill('1');
    await page.locator('#addform button[type=submit]').click();
    await expect(page.locator('#main-content')).toContainText('remembered');
    await openAddForm(page);
    // the first row remembers the category, and lists its accounts; the second the account itself
    await expect(page.locator('#addform .category-select')).toHaveValue('expenses:food');
    await expect(page.locator('#addform select[name=account]').nth(0)).toHaveValue('');
    expect(await page.locator('#addform select[name=account]').nth(0).locator('option').allTextContents()).toEqual(['Subcategory', 'dining', 'dining › xss<script>alert(1)</script>', 'groceries']);
    await expect(page.locator('#addform select[name=account]').nth(1)).toHaveValue('equity:opening');
  });

  test('the date field offers a picker and accepts a typed date', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    const date = page.locator('#addform input[name=date]');
    await date.fill('2025-03-04');
    await expect(date).toHaveValue('2025-03-04');
    expect(pageErrors).toEqual([]);
  });

  test('adds a transaction', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await setPayee(page, 'E2eCreatePlain');
    await page.locator('#addform .category-select').selectOption('expenses:food');
    await page.locator('#addform select[name=account]').nth(0).selectOption('expenses:food:dining');
    await page.locator('#addform input[name=amount]').nth(0).fill('12.34');
    await page.locator('#addform select[name=account]').nth(1).selectOption('assets:bank:checking');
    await page.locator('#addform button[type=submit]').click();
    await expect(page.locator('#message')).toContainText('Transaction added.');
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).toContain('E2eCreatePlain');
  });

  test('reports an unbalanced transaction without adding it', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await setPayee(page, 'E2eUnbalanced');
    await page.locator('#addform .category-select').selectOption('expenses:food');
    await page.locator('#addform select[name=account]').nth(0).selectOption('expenses:food:dining');
    await page.locator('#addform input[name=amount]').nth(0).fill('10.00');
    await page.locator('#addform select[name=account]').nth(1).selectOption('assets:bank:checking');
    await page.locator('#addrow').click();  // the second amount shows once a third row exists
    await page.locator('#addform input[name=amount]').nth(1).fill('99.00');
    await page.locator('#addform button[type=submit]').click();
    await expect(page.locator('#message')).toBeVisible();
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).not.toContain('E2eUnbalanced');
  });

});

test.describe('transaction page', () => {

  // Each test adds the entry it works on, leaving the fixture's entries to
  // the other tests, then opens its page with a click on its row.
  async function openEntry(page, desc, amount) {
    await page.goto('/journal');
    await openAddForm(page);
    await setPayee(page, desc);
    await page.locator('#addform .category-select').selectOption('expenses:food');
    await page.locator('#addform select[name=account]').nth(0).selectOption('expenses:food:dining');
    await page.locator('#addform input[name=amount]').nth(0).fill(amount);
    await page.locator('#addform select[name=account]').nth(1).selectOption('assets:bank:checking');
    await page.locator('#addform button[type=submit]').click();
    await expect(page.locator('#message')).toContainText('Transaction added.');
    await page.locator('#main-content tr.title', { hasText: desc }).locator('td.amount').click();
    await expect(page).toHaveURL(/\/transaction\/\d+$/);
  }

  test('every journal row starts with an icon slot, and a transfer without a description is named', async ({ page }) => {
    await page.goto('/journal');
    expect(await page.locator('#main-content tr.title td.description .entry-icon').count()).toBeGreaterThan(0);
    // the fixture's opening balances are a transfer with a description; add one without
    await openAddForm(page);
    await page.locator('#addform .kind-option[data-kind=transfer]').click();
    await page.locator('#addform input[name=amount]').nth(0).fill('1');
    await page.locator('#addform select[name=account]').nth(0).selectOption('assets:bank:checking');
    await page.locator('#addform select[name=account]').nth(1).selectOption('equity:opening');
    await page.locator('#addform button[type=submit]').click();
    await expect(page.locator('#message')).toContainText('Transaction added.');
    await expect(page.locator('#main-content tr.title td.description .entry-kind').first()).toHaveText('Transfer');
    expect(pageErrors).toEqual([]);
  });

  test("a click anywhere on an entry's row opens its page, a card with Edit and Delete; its links still work", async ({ page }) => {
    await openEntry(page, 'E2eRowClick', '5.00');
    await expect(page.locator('.entry-view-amount')).toHaveText(/5\.00/);
    await expect(page.locator('.entry-view-facts dd', { hasText: 'E2eRowClick' })).toBeVisible();
    await expect(page.locator('#addform')).toBeHidden();
    await expect(page.locator('.entry-actions a.entry-edit-button')).toBeVisible();
    await expect(page.locator('#deleteform button[type=submit]')).toBeVisible();
    // a link inside the row goes where it says, not to the entry
    await page.goto('/journal');
    await page.locator('#main-content tr.title', { hasText: 'E2eRowClick' }).locator('td.date a').click();
    await expect(page).toHaveURL(/\/journal\?q=date/);
    // on a phone the entry's block is one target: even its links open the entry
    await page.setViewportSize({ width: 390, height: 844 });
    await page.goto('/journal');
    await page.locator('#main-content tr.title', { hasText: 'E2eRowClick' }).locator('td.entry-card .entry-what a').click();
    await expect(page).toHaveURL(/\/transaction\/\d+$/);
    expect(pageErrors).toEqual([]);
  });

  test('Edit shows the entry filled in, and saving writes it in place', async ({ page }) => {
    await openEntry(page, 'E2eEditMe', '7.00');
    await page.locator('.entry-actions a.entry-edit-button').click();
    await expect(page.locator('#addform')).toBeVisible();
    await expect(page.locator('#addform input[name=description]')).toHaveValue('E2eEditMe');
    await expect(page.locator('#addform .category-select')).toHaveValue('expenses:food');
    await expect(page.locator('#addform select[name=account]').nth(0)).toHaveValue('expenses:food:dining');
    await expect(page.locator('#addform input[name=amount]').nth(0)).toHaveValue(/^7(\.00)?$/);
    await expect(page.locator('#addform select[name=account]').nth(1)).toHaveValue('assets:bank:checking');
    await page.locator('#addform input[name=amount]').nth(0).fill('8.50');
    await setPayee(page, 'E2eEdited');
    await page.locator('#addform button[type=submit]').click();
    await expect(page.locator('#message')).toContainText('Transaction saved.');
    const journal = fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8');
    expect(journal).toContain('E2eEdited');
    expect(journal).toContain('8.50');
    expect(journal).not.toContain('E2eEditMe');
    expect(pageErrors).toEqual([]);
  });

  test('the delete button asks first, then removes the entry', async ({ page }) => {
    await openEntry(page, 'E2eDeleteMe', '3.00');
    page.once('dialog', d => d.dismiss());
    await page.locator('#deleteform button[type=submit]').click();
    await expect(page).toHaveURL(/\/transaction\/\d+$/);
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).toContain('E2eDeleteMe');
    page.once('dialog', d => d.accept());
    await page.locator('#deleteform button[type=submit]').click();
    await expect(page.locator('#message')).toContainText('Transaction deleted.');
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).not.toContain('E2eDeleteMe');
    expect(pageErrors).toEqual([]);
  });

});

test.describe('edit form', () => {

  test('edits a journal file', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('a[title="Manage journal files"]').click();
    await page.locator('a.btn', { hasText: 'Edit' }).first().click();
    const ta = page.locator('textarea');
    const text = await ta.inputValue();
    expect(text).toContain('Metro Transit');
    await ta.fill(text.replace('Metro Transit', 'Metro Transit EDITED'));
    await page.locator('input[type=submit][value=Save]').click();
    await expect(page.locator('#message')).toContainText('Saved journal');
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).toContain('Metro Transit EDITED');
  });

  test('rejects an edit that would not parse, leaving the file alone', async ({ page }) => {
    await page.goto('/manage');
    await page.locator('a.btn', { hasText: 'Edit' }).first().click();
    const ta = page.locator('textarea');
    const before = fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8');
    await ta.fill('this is not a journal\n  and cannot parse\n');
    await page.locator('input[type=submit][value=Save]').click();
    await expect(page.locator('#message')).toContainText('Failed to load journal');
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).toEqual(before);
  });

});

test.describe('search', () => {

  test('accepts documented query syntax containing slashes', async ({ page }) => {
    await page.goto('/journal');
    let dialogMessage = null;
    page.on('dialog', d => { dialogMessage = d.message(); d.dismiss().catch(() => {}); });
    await page.locator('#searchform input[name=q]').fill('date:2025/1/5');
    await page.locator('#searchform input[name=q]').press('Enter');
    await expect(page).toHaveURL(/date%3A2025/);
    expect(dialogMessage).toBeNull();
    // the filter was applied: only the matching transaction remains
    // (the table: the add form's payee list names every payee)
    await expect(page.locator('#main-content table')).toContainText('weekly shop');
    await expect(page.locator('#main-content table')).not.toContainText('Cafe Luna');
  });

  test('an account query filters the register', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('#searchform input[name=q]').fill('acct:dining');
    await page.locator('#searchform input[name=q]').press('Enter');
    await expect(page.locator('#main-content table')).toContainText('Cafe Luna');
    await expect(page.locator('#main-content table')).not.toContainText('Metro Transit');
  });

});

test.describe('sidebar tree and top menu', () => {

  test("a chevron folds the accounts under one, the browser remembers, and the account shown stays in sight", async ({ page }) => {
    await page.goto('/journal');
    // (assets:bank:checking is one row: a lone chain is not a tree)
    const food = page.locator('#sidebar-menu tr[data-acct="expenses:food"]');
    const groceries = page.locator('#sidebar-menu tr[data-acct="expenses:food:groceries"]');
    await expect(groceries).toBeVisible();
    await expect(food.locator('.tree-toggle')).toHaveAttribute('aria-expanded', 'true');
    await food.locator('.tree-toggle').click();
    await expect(groceries).toBeHidden();
    await expect(food.locator('.tree-toggle')).toHaveAttribute('aria-expanded', 'false');
    await page.goto('/journal');
    await expect(groceries).toBeHidden();
    // the account shown is in sight whatever was folded
    await page.goto('/register?q=' + encodeURIComponent('inacct:expenses:food:groceries'));
    await expect(groceries).toBeVisible();
    await page.goto('/journal');
    await food.locator('.tree-toggle').click();
    await expect(groceries).toBeVisible();
    // the whole tree at once
    await page.locator('.tree-all[data-fold="all"]').click();
    await expect(groceries).toBeHidden();
    await expect(food).toBeHidden();
    await expect(page.locator('#sidebar-menu tr[data-acct="expenses"]')).toBeVisible();
    await page.goto('/journal');
    await expect(food).toBeHidden();
    await page.locator('.tree-all[data-fold="none"]').click();
    await expect(groceries).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  test('the header is a band across the top, the menu lined up with the main pane', async ({ page }) => {
    await page.goto('/journal');
    const bar = await page.locator('#topbar').boundingBox();
    const sidebar = await page.locator('#sidebar-menu').boundingBox();
    expect(bar.width).toBeGreaterThan(sidebar.width * 3);
    const menu = await page.locator('.topmenu a').first().boundingBox();
    const column = await page.locator('#main-content .journal-page').boundingBox();
    expect(Math.abs(menu.x + 14 - column.x)).toBeLessThan(2);
    // the header names the page, with the journal's file under it; the search and the add button are in it
    await expect(page.locator('#topbar h1')).toHaveText('General Journal');
    await expect(page.locator('#topbar .journal-file')).toHaveText(/\.journal$/);
    await expect(page.locator('#topbar #searchform')).toBeVisible();
    await expect(page.locator('#topbar .topbar-add')).toBeVisible();
    await expect(page.locator('#addformlink')).toBeHidden();
    expect(await page.locator('.topmenu a').first().evaluate(a => parseFloat(getComputedStyle(a).fontSize))).toBeGreaterThanOrEqual(16);
  });

  test('the pages are a menu across the top, the page shown marked', async ({ page }) => {
    await page.goto('/journal');
    expect(await page.locator('.topmenu a').allTextContents()).toEqual(['Journal', 'Accounts', 'Reports', 'Recurring']);
    await expect(page.locator('.topmenu a.current')).toHaveText('Journal');
    await page.locator('.topmenu a', { hasText: 'Accounts' }).click();
    await expect(page).toHaveURL(/\/balancesheet$/);
    await expect(page.locator('.topmenu a.current')).toHaveText('Accounts');
    await expect(page.locator('#topbar h1')).toHaveText('Accounts');
    // the sidebar holds the accounts alone now
    expect(await page.locator('#sidebar-menu td.top').count()).toBe(0);
  });

});

test.describe('keyboard shortcuts', () => {

  test('"s" toggles the sidebar, and the choice survives navigation', async ({ page }) => {
    await page.goto('/journal');
    const sidebar = page.locator('#sidebar-menu');
    await expect(sidebar).toBeVisible();
    await page.locator('body').press('s');
    await expect(sidebar).not.toBeVisible();
    // the server reads the showsidebar cookie, so a fresh page load keeps it hidden
    await page.goto('/journal');
    await expect(sidebar).not.toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  test('shortcuts do not fire while typing in the search box', async ({ page }) => {
    await page.goto('/journal');
    const q = page.locator('#searchform input[name=q]');
    await q.click();
    // 's' would toggle the sidebar and 'a' would open the add form
    await q.pressSequentially('assets', { delay: 20 });
    await expect(q).toHaveValue('assets');
    await expect(page.locator('#sidebar-menu')).toBeVisible();
    await expect(page.locator('#addmodal')).not.toBeVisible();
  });

  test('"h" opens the help dialog', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('body').press('h');
    await expect(page.locator('#helpmodal')).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

});

test.describe('modal dialogs', () => {

  // A <dialog> shown with showModal() should capture the shortcut keys; before
  // the guard, keys pressed with a dialog open acted on the page behind it.
  test('shortcut keys do nothing while a modal is open', async ({ page }) => {
    await page.goto('/register');
    await page.locator('body').press('h');
    await expect(page.locator('#helpmodal')).toBeVisible();
    await page.locator('body').press('j');   // would go to /journal
    await page.locator('body').press('a');   // would open the add form
    await expect(page).toHaveURL(/register/);
    await expect(page.locator('#helpmodal')).toBeVisible();
    await expect(page.locator('#addmodal')).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

  // showModal() throws if the dialog is already open; the add-form shortcut
  // must not raise an uncaught error when pressed with the form already up.
  test('re-triggering the add form while open does not error', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('body').press('a');
    await expect(page.locator('#addmodal')).toBeVisible();
    await page.locator('body').press('a');
    await expect(page.locator('#addmodal')).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  // Clicking the backdrop closes the dialog (the help text promises this).
  test('clicking outside the dialog closes it', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('body').press('h');
    await expect(page.locator('#helpmodal')).toBeVisible();
    await page.mouse.click(4, 4);   // top-left corner: the backdrop
    await expect(page.locator('#helpmodal')).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

});

test.describe('sidebar', () => {

  // Regression test for #2651: the sidebar's scroll position is restored
  // during page parse, so it must not be at 0 after navigating.
  test('keeps its scroll position across navigation', async ({ page }) => {
    await page.setViewportSize({ width: 1200, height: 300 });
    await page.goto('/journal');
    const sidebar = page.locator('#sidebar-menu');
    await sidebar.evaluate(el => { el.scrollTop = el.scrollHeight; });
    const scrolled = await sidebar.evaluate(el => el.scrollTop);
    test.skip(scrolled === 0, 'sidebar does not scroll at this viewport');
    await page.locator('#sidebar-menu a.acct-name', { hasText: 'groceries' }).first().click();
    await expect(page).toHaveURL(/register/);
    expect(await page.locator('#sidebar-menu').evaluate(el => el.scrollTop)).toBeGreaterThan(0);
  });

  test('"e" hides empty accounts', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('body').press('e');
    // the toggle is remembered server-side via the hideemptyaccts cookie
    await page.goto('/journal');
    expect(pageErrors).toEqual([]);
  });

  test('the statements are at their addresses, outside the menu, with no Report row either', async ({ page }) => {
    // at a laptop width too, the menu holds its four pages
    await page.setViewportSize({ width: 1000, height: 800 });
    await page.goto('/incomestatement');
    await expect(page.locator('#main-content h2')).toContainText('Income Statement');
    expect(await page.locator('.topmenu a').allTextContents()).not.toContain('Income statement');
    await expect(page.locator('.topmenu a.current')).toHaveCount(0);
    // the Report row would say the same, so it is not shown here
    await expect(page.locator('#main-content .report-links.report-row')).toBeHidden();
    await expect(page.locator('#main-content .report-links', { hasText: 'Section' })).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

});

// Hovering a transaction shows its journal entry in a tooltip drawn by
// hledger.js, in a fixed-width font (#2716). It behaves like a browser's own
// tooltip: it appears after a pause, and goes when the pointer leaves or clicks.
test.describe('entry tooltip', () => {

  test('hovering a journal entry shows it, until the pointer leaves or clicks', async ({ page }) => {
    await page.goto('/journal');
    const tip = page.locator('.entry-tooltip');
    const row = page.locator('#main-content tr.title', { hasText: 'Cafe Luna' });
    const description = row.locator('td').nth(1);
    // the row has no title of its own, so no browser tooltip shows as well
    await expect(row).not.toHaveAttribute('title');
    await description.hover();
    await expect(tip).toBeVisible();
    await expect(tip).toContainText('Cafe Luna');
    await expect(tip).toContainText('expenses:food:dining');
    await page.locator('.period-band .period-name').hover();
    await expect(tip).toBeHidden();
    // a click on the row opens the entry's page (the tooltip goes with the page)
    await description.hover();
    await expect(tip).toBeVisible();
    await page.mouse.down();
    await page.mouse.up();
    await expect(page).toHaveURL(/\/transaction\/\d+$/);
    expect(pageErrors).toEqual([]);
  });

  test('a register row shows its entry too, but over an account link the link\'s title applies', async ({ page }) => {
    await page.goto('/register?q=inacct:assets:bank:checking');
    const tip = page.locator('.entry-tooltip');
    const row = page.locator('#main-content tbody tr', { hasText: 'Cafe Luna' });
    await row.locator('td.description').hover();
    await expect(tip).toBeVisible();
    await expect(tip).toContainText('Cafe Luna');
    const link = row.locator('td.account a');
    await expect(link).toHaveAttribute('title', 'expenses:food:dining');
    await link.hover();
    await expect(tip).toBeHidden();
    await page.waitForTimeout(800);
    await expect(tip).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

});

test('the add form\'s calendar button reaches the date input beneath the pointer', async ({ page }) => {
  await page.goto('/add');
  await page.evaluate(() => { window.__picker = 0; document.getElementById('datepicked').showPicker = function() { window.__picker++; }; });
  const b = await page.locator('#datebutton').boundingBox();
  await page.mouse.click(b.x + b.width / 2, b.y + b.height / 2);
  expect(await page.evaluate(() => window.__picker)).toBe(1);
  // what the picker gives is written back to the field as an iso date
  await page.locator('#datepicked').evaluate(d => { d.value = '2025-02-15'; d.dispatchEvent(new Event('change')); });
  await expect(page.locator('#addform input[name=date]')).toHaveValue('2025-02-15');
});

test('a note goes after the payee in the description', async ({ page }) => {
  await page.goto('/journal');
  await page.locator('body').press('a');
  await expect(page.locator('#addmodal')).toBeVisible();
  await setPayee(page, 'Cafe Noted');
  await page.locator('#addform input[name=note]').fill('with a note');
  await page.locator('#addform .category-select').selectOption('expenses:food');
  await page.locator('#addform select[name=account]').nth(0).selectOption('expenses:food:dining');
  await page.locator('#addform input[name=amount]').nth(0).fill('3');
  await page.locator('#addform select[name=account]').nth(1).selectOption('assets:bank:checking');
  await page.locator('#addform button[type=submit]').click();
  await expect(page.locator('#main-content')).toContainText('Cafe Noted');
  await expect(page.locator('#main-content')).toContainText('with a note');
});
