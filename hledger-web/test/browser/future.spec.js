// Browser tests for entries dated after today: on the journal and on an
// account's page they are folded away under a row with a button, and the
// page's figures leave them out. The server runs with --today, and the
// journal gets an entry a week later.
const { test, expect } = require('@playwright/test');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { startServer } = require('./server');

let server, URL, tmpdir;
test.beforeAll(async () => {
  tmpdir = fs.mkdtempSync(path.join(os.tmpdir(), 'hledger-web-future-'));
  const journal = path.join(tmpdir, 'future.journal');
  fs.copyFileSync(path.join(__dirname, 'fixture.journal'), journal);
  fs.appendFileSync(journal, '\n2025-02-21 ! Fibre Fast written ahead\n    expenses:utilities:internet    59.99\n    assets:bank:checking\n');
  ({ child: server, url: URL } = await startServer(
    ['-f', journal, '--serve', '--host', '127.0.0.1', '--port', '0', '--today', '2025-02-14']));
});
test.afterAll(async () => {
  if (server) { server.kill(); }
  fs.rmSync(tmpdir, { recursive: true, force: true });
});

test("an account's page counts up to today, and unfolds the later entries on request", async ({ page }) => {
  await page.goto(URL + '/register?q=' + encodeURIComponent('inacct:assets:bank:checking'));
  // the balance before the entry of the 21st: 1000 + 3200 - 52.10 - 21.50 - 48.75 - 18.00
  await expect(page.locator('.account-balance')).toHaveText(/4,?059\.65/);
  const later = page.locator('.registerreport tr.future[data-entry]');   // (the day headings show on phones only)
  await expect(later.first()).toBeHidden();
  const button = page.locator('.future-button');
  await expect(button).toHaveText('Show 1 entries dated after today');
  await button.click();
  await expect(later.first()).toBeVisible();
  await expect(button).toHaveText('Hide the entries dated after today');
  await button.click();
  await expect(later.first()).toBeHidden();
});

test('the journal folds them away too, and its totals leave them out', async ({ page }) => {
  await page.goto(URL + '/journal');
  await expect(page.locator('.journal-entries tr.title.future')).toBeHidden();
  await expect(page.locator('.period-totals .spent')).toHaveText(/^\s*66\.75\s*$/);   // Grocer Green and Cafe Luna, up to the 14th
  await page.locator('.future-button').click();
  await expect(page.locator('.journal-entries tr.title.future')).toBeVisible();
});
