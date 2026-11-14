/* hledger web ui javascript */

//----------------------------------------------------------------------
// STARTUP

document.addEventListener('DOMContentLoaded', function() {

  // First, so that nothing below can stop it.
  browsePingInit();

  // Open and close the dialogs. The data-toggle/data-target/data-dismiss
  // attributes in the templates are the hooks. On a page without the dialog,
  // a link (the add buttons) goes to its href instead, the add page.
  document.querySelectorAll('[data-toggle="modal"]').forEach(function(el) {
    el.addEventListener('click', function(e) {
      var d = document.querySelector(el.getAttribute('data-target'));
      if (!d) { return; }
      e.preventDefault();
      if (d.id === 'addmodal') { addformShow(); } else { d.showModal(); }
    });
  });
  document.querySelectorAll('[data-dismiss="modal"]').forEach(function(el) {
    el.addEventListener('click', function() {
      var d = el.closest('dialog');
      if (d) { d.close(); }
    });
  });
  // Clicking the backdrop closes a modal dialog. With showModal() a click on
  // the backdrop is reported as a click on the dialog element itself.
  document.querySelectorAll('dialog').forEach(function(d) {
    d.addEventListener('click', function(e) {
      if (e.target === d) { d.close(); }
    });
  });

  // The button under the posting rows adds another.
  var addform = document.getElementById('addform');
  if (addform) {
    var addrow = document.getElementById('addrow');
    if (addrow) { addrow.addEventListener('click', addformAddPosting); }
    // Remember the first two rows' accounts for the next entry (see addformFocus).
    addform.addEventListener('submit', function() {
      if ('editing' in addform.dataset) { return; }
      addform.querySelectorAll('select[name=account]').forEach(function(sel) {
        if ((sel.dataset.row === '1' || sel.dataset.row === '2') && sel.value) {
          setCookie('addaccount' + sel.dataset.row, encodeURIComponent(sel.value));
        }
      });
    });
  }

  // The date field is a text input, so hledger's smart dates ("today", "2/15")
  // keep working. The unseen date input over the button beside it opens the
  // browser's own calendar: a phone does that for any tap on it; a desktop
  // browser opens it only from its own icon, so it is asked to (showPicker,
  // where there is one). What you pick is written back as an iso date.
  var datebutton = document.getElementById('datebutton');
  var datepicked = document.getElementById('datepicked');
  if (datebutton && datepicked) {
    datepicked.addEventListener('click', function() {
      var datefield = document.querySelector('#addform input[name=date]');
      datepicked.value = /^\d{4}-\d{2}-\d{2}$/.test(datefield.value) ? datefield.value : isoDate();
      if (datepicked.showPicker && !window.matchMedia('(pointer: coarse)').matches) {
        try { datepicked.showPicker(); } catch (e) { /* the browser opens it itself */ }
      }
    });
    datepicked.addEventListener('change', function() {
      var datefield = document.querySelector('#addform input[name=date]');
      if (datepicked.value) { datefield.value = datepicked.value; }
    });
  }

  // Keyboard shortcuts. Not while typing in a field, or the search box would
  // toggle the sidebar and open the add form as you spell "assets".
  document.addEventListener('keydown', function(e) {
    if (e.ctrlKey || e.metaKey || e.altKey) { return; }
    if (e.target.closest('input, textarea, select')) { return; }
    if (document.querySelector('dialog[open]')) { return; }
    switch (e.key) {
      case 'h': case '?': helpToggle();                                     break;
      case 'j': location.href = document.hledgerWebBaseurl + '/journal';     break;
      case 's': sidebarToggle();                                            break;
      case 'e': emptyAccountsToggle();                                      break;
      case 'a': case 'n': addformShow();                                    break;
      case 'f': focusSearch();                                              break;
      default: return;
    }
    e.preventDefault();
  });

  // Name the chosen file beside the upload button. Set as text, so a file whose
  // name contains markup is shown, not parsed.
  var fileinput = document.getElementById('file');
  if (fileinput) {
    fileinput.addEventListener('change', function() {
      var info = document.getElementById('file-info');
      if (info) {
        info.textContent = fileinput.files[0] ? fileinput.files[0].name : '';
      }
    });
  }

  // The journal shows each entry single-sided unless "Show both sides" is
  // ticked. The choice is kept in a cookie, which the server reads, so that
  // the next page is drawn the same way from the start.
  var bothsides = document.getElementById('bothsides');
  if (bothsides) {
    var showBothSides = function() {
      document.querySelectorAll('.transactionsreport').forEach(function(t) {
        t.classList.toggle('both-sides', bothsides.checked);
      });
    };
    // A page brought back from the history may have the box as it was left.
    showBothSides();
    bothsides.addEventListener('change', function() {
      showBothSides();
      setCookie('journalbothsides', bothsides.checked ? '1' : '0');
    });
  }

  // A report's choices as drop-down lists, which a phone shows instead of
  // the rows of links (see linksRow): picking one goes to it. A page
  // brought back from the history may have a list as it was left, showing
  // a choice that is not the page's; each is set back to the page's own.
  var choices = document.querySelectorAll('select[data-navigate]');
  var resetChoices = function() {
    choices.forEach(function(s) {
      s.querySelectorAll('option').forEach(function(o) { o.selected = o.defaultSelected; });
    });
  };
  resetChoices();
  window.addEventListener('pageshow', function(e) {
    if (e.persisted) { resetChoices(); }
  });
  choices.forEach(function(s) {
    s.addEventListener('change', function() {
      if (s.value) { window.location.href = s.value; }
    });
  });

  // On a phone, the tab bar's Search tab opens the search form, which is
  // out of sight there until asked for, and puts the cursor in it.
  document.querySelectorAll('#tabbar .tab-search').forEach(function(el) {
    el.addEventListener('click', function(e) {
      if (!document.getElementById('searchform')) { return; }
      e.preventDefault();
      focusSearch();
    });
  });

  document.querySelectorAll('[data-toggle="offcanvas"]').forEach(function(el) {
    el.addEventListener('click', function() {
      var row = document.querySelector('.row-offcanvas');
      if (row) { row.classList.toggle('active'); }
    });
  });

  // The add page shows the form outside the dialog; set it up as the
  // dialog does when it opens. So does an entry's own page.
  if (addform && !addform.closest('#addmodal')) { addformFocus(); }
  // Deleting an entry asks first.
  var deleteform = document.getElementById('deleteform');
  if (deleteform) {
    deleteform.addEventListener('submit', function(e) {
      if (!window.confirm(deleteform.dataset.confirm || 'Delete this entry?')) { e.preventDefault(); }
    });
  }

  entryTooltipInit();
  futureRowsInit();
  entryHoverInit();
  entryRowsInit();
  calcInit();
  entryPageInit();
  sidebarTreeInit();
  registerChartInit();
  reportsChartInit();
});

// The entry targeted by the url hash is marked by a :target rule in
// hledger.css, which needs no javascript. A hash can be anything, eg an
// old bookmark's numeric row id, so location.hash must not be passed to
// querySelector.

// The account sidebar's scroll position is preserved across page navigations
// by an inline script right after the sidebar's markup in
// default-layout.hamlet. It must run during page parse, not from this file:
// this script loads at the end of the body and DOMContentLoaded fires only
// once the whole document is parsed, while the browser paints the sidebar
// (at scroll position 0) much earlier when a long journal or register
// follows it, which made the sidebar visibly jump on page load.

//----------------------------------------------------------------------
// ADD FORM

function addformShow(showmsg) {
  var d = document.getElementById('addmodal');
  if (!d || d.open) { return; }  // showModal() throws if already open
  addformReset(typeof showmsg !== 'undefined' ? showmsg : false);
  d.showModal();
  addformFocus();
}

function helpToggle() {
  var d = document.getElementById('helpmodal');
  if (!d) { return; }
  if (d.open) {
    d.close();
  } else {
    d.showModal();
    // showModal() focuses the first focusable element, which here is the
    // close button; focus the dialog itself so it opens without a focus ring.
    d.focus();
  }
}

// Make sure the add form is empty and clean and has the default number of rows.
function addformReset(showmsg) {
  var addform = document.getElementById('addform');
  if (!addform) { return; }
  if (!showmsg) {
    var msg = document.getElementById('message');
    if (msg) { msg.innerHTML = ''; }
  }
  addform.querySelectorAll('.account-group.added-row').forEach(function(el) {
    el.remove();
  });
  addform.reset();
}

// Pre-fill today's date and focus the amount field in the add form.
// The first two account rows start on what this browser last saved
// (cookies addaccount1 and addaccount2, written on submit), if the
// books still have those accounts; else on the page's own defaults. A
// row the page filled from the account in view (data-current-row on the
// rows' container) keeps it.
function addformFocus() {
  var addform = document.getElementById('addform');
  if (!addform) { return; }
  // An existing entry's page shows that entry: no today's date, no remembered
  // accounts; only the category list is set from the entry's own account.
  if ('editing' in addform.dataset) { addformCategoryInit(); addformKindInit(); addformPayeeInit(); return; }
  addform.querySelector('input[name=date]').value = isoDate();
  var datepicked = document.getElementById('datepicked');
  if (datepicked) { datepicked.value = isoDate(); }
  addformCategoryInit();
  addformKindInit();
  addformPayeeInit();
  var postings = addform.querySelector('.account-postings');
  var currentRow = postings ? postings.dataset.currentRow : '';
  addform.querySelectorAll('select[name=account]').forEach(function(sel) {
    if (sel.dataset.row === currentRow) { return; }
    var saved = getCookie('addaccount' + sel.dataset.row);
    if (!saved) { return; }
    var value = decodeURIComponent(saved);
    if (sel.dataset.row === '1') {
      // the category is remembered; the account within it is picked each time
      var o = addformAccountOptions[0].find(function(o) { return o.value === value; });
      if (o) { addformSetCategory(o.dataset.category); }
    } else if ([].some.call(sel.options, function(o) { return o.value === value; })) {
      sel.value = value;
    }
  });
  // Deferred, so the field is focusable: http://stackoverflow.com/a/7046837
  setTimeout(function() {
    addform.querySelector('input[name=amount]').focus();
  }, 0);
}

// On a transfer, the button beside the second list swaps the accounts
// paid to and from.
function addformSwapSides() {
  var to = document.querySelector('#addform select[name=account][data-row="1"]');
  var from = document.querySelector('#addform select[name=account][data-row="2"]');
  if (!to || !from) { return; }
  var has = function(sel, v) { return v && [].some.call(sel.options, function(o) { return o.value === v; }); };
  var wasTo = to.value, wasFrom = from.value;
  to.value = has(to, wasFrom) ? wasFrom : '';
  from.value = has(from, wasTo) ? wasTo : '';
  if (!to.value) { to.options[0].selected = true; }
  if (!from.value) { from.options[0].selected = true; }
}

// The payee field offers the books' payees as it is typed into: those
// whose letters it names in order ("wlm" finds Walmart), the closest
// first, the ones used most before the others. Arrows and Enter pick one,
// as does a tap; a name no payee has is simply typed. (Nothing shows
// until something is typed: a list over the form on focus got in the way.)
// The payee and the note fields offer what the books have as they are
// typed into: the payees, and the notes, the ones used with the payee
// named first (both from the lists at the page's end).
function addformPayeeInit() {
  var payeeInput = document.querySelector('#addform input[name=description]');
  var names = document.getElementById('payeenames');
  var payees = [];
  if (names) { [].forEach.call(names.options, function(o) { if (payees.indexOf(o.value) < 0) { payees.push(o.value); } }); }
  typeaheadInit(payeeInput, document.querySelector('#addform .payee-matches'), function() { return payees; });
  var noteNames = document.getElementById('notenames');
  var notes = [];
  if (noteNames) { [].forEach.call(noteNames.options, function(o) { notes.push(o.value); }); }
  typeaheadInit(document.querySelector('#addform input[name=note]'), document.querySelector('#addform .note-matches'), function() {
    var own = [];
    var typed = payeeInput ? payeeInput.value.trim().toLowerCase() : '';
    if (names && typed) {
      [].forEach.call(names.options, function(o) {
        if (o.value.toLowerCase() === typed && o.dataset.notes) {
          o.dataset.notes.split('\t').forEach(function(n) { if (n && own.indexOf(n) < 0) { own.push(n); } });
        }
      });
    }
    var all = own.concat(notes.filter(function(n) { return own.indexOf(n) < 0; }));
    all.preferred = own.length;
    return all;
  });
}

// A field offering candidates() as it is typed into: the first
// candidates.preferred of them above the rest whatever the match, then
// the best matches first and, among equals, the earlier candidates;
// arrows, Enter and a tap pick one. Nothing shows until something is typed.
function typeaheadInit(input, list, candidates) {
  if (!input || !list || input.dataset.wired) { return; }
  input.dataset.wired = '1';
  var active = -1;
  // how well a payee answers what is typed: 0 starts with it, 1 a word of
  // it does, 2 it is somewhere inside, 3 its letters are inside in order
  function rank(q, s) {
    if (s.indexOf(q) === 0) { return 0; }
    if (s.indexOf(' ' + q) >= 0) { return 1; }
    if (s.indexOf(q) >= 0) { return 2; }
    var i = 0;
    for (var k = 0; k < s.length && i < q.length; k++) { if (s[k] === q[i]) { i++; } }
    return i === q.length ? 3 : -1;
  }
  function refresh() {
    var q = input.value.trim().toLowerCase();
    var all = candidates(), preferred = all.preferred || 0;
    var found = !q ? [] : all.map(function(p, i) { return [i < preferred ? 0 : 1, rank(q, p.toLowerCase()), i, p]; })
                      .filter(function(x) { return x[1] >= 0 && x[3].toLowerCase() !== q; })
                      .sort(function(a, b) { return a[0] - b[0] || a[1] - b[1] || a[2] - b[2]; })
                      .map(function(x) { return x[3]; });
    list.textContent = '';
    found.slice(0, 8).forEach(function(p) {
      var li = document.createElement('li');
      li.textContent = p;
      li.setAttribute('role', 'option');
      list.appendChild(li);
    });
    active = -1;
    list.hidden = !list.children.length;
  }
  function close() { list.hidden = true; active = -1; }
  function pick(li) { input.value = li.textContent; close(); input.focus(); }
  function setActive(i) {
    var items = list.children;
    if (!items.length) { return; }
    active = (i + items.length) % items.length;
    [].forEach.call(items, function(li, k) { li.classList.toggle('active', k === active); });
  }
  input.addEventListener('input', refresh);
  input.addEventListener('blur', function() { setTimeout(close, 150); });
  input.addEventListener('keydown', function(e) {
    if (list.hidden) { return; }
    if (e.key === 'ArrowDown') { setActive(active + 1); e.preventDefault(); }
    else if (e.key === 'ArrowUp') { setActive(active - 1); e.preventDefault(); }
    else if (e.key === 'Escape') { close(); }
    else if (e.key === 'Enter') {
      // what is typed is in the books already: the form goes on
      var typed = input.value.trim().toLowerCase();
      if (active < 0 && candidates().some(function(p) { return p.toLowerCase() === typed; })) { close(); return; }
      var li = list.children[active >= 0 ? active : 0];
      if (li) { pick(li); }
      e.preventDefault();
    }
  });
  // picked on the way down, before the field loses focus
  list.addEventListener('pointerdown', function(e) {
    var li = e.target.closest('li');
    if (li) { e.preventDefault(); pick(li); }
  });
}

function isoDate() {
  return new Date().toLocaleDateString("sv");  // https://stackoverflow.com/a/58633651/84401
}

// Focus the search form, opening it first on a phone (see hledger.css).
function focusSearch() {
  var form = document.getElementById('searchform');
  if (!form) { return; }
  form.classList.add('open');
  var q = form.querySelector('input');
  if (q) { q.focus(); }
}

// The first row's account list, narrowed to the category picked above
// it, and empty until one is: since a phone cannot hide single options,
// the list is rebuilt from a kept copy of all of them.
var addformAccountOptions = [];
function addformCategoryInit() {
  var category = document.querySelector('#addform .category-select');
  var account = document.querySelector('#addform select[name=account][data-row="1"]');
  if (!category || !account) { return; }
  if (!addformAccountOptions.length) {
    addformAccountOptions = [[].slice.call(account.options, 1)];
    category.addEventListener('change', function() { addformSetCategory(category.value); });
    // an account picked from the full list sets the category beside it
    // (not on a transfer, whose list is every account of the kind)
    account.addEventListener('change', function() {
      var kinds = document.querySelector('#addform .kind-select');
      if (kinds && kinds.dataset.kind === 'transfer') { return; }
      var o = account.options[account.selectedIndex];
      if (o && o.dataset.category && category.value !== o.dataset.category) {
        category.value = o.dataset.category;
        addformSetCategory(o.dataset.category, o.value);
      }
    });
  }
  var chosen = account.options[account.selectedIndex];
  category.value = chosen && chosen.dataset.category ? chosen.dataset.category : '';
  addformSetCategory(category.value, chosen ? chosen.value : '');
}
function addformSetCategory(key, keep) {
  var category = document.querySelector('#addform .category-select');
  var account = document.querySelector('#addform select[name=account][data-row="1"]');
  if (!category || !account) { return; }
  // a category of another kind of entry (a remembered one, say) brings its kind along
  if (key && addformCategoryGroups && ![].some.call(category.options, function(o) { return o.value === key; })) {
    addformSetKind(addformKindOf(key), key);
  }
  if (category.value !== key) { category.value = key; }
  var kindsEl = document.querySelector('#addform .kind-select');
  if (kindsEl && kindsEl.dataset.kind === 'transfer') { return; }   // (every account is listed already)
  var placeholder = account.options[0];
  // nothing to pick from until a category is picked
  var options = addformAccountOptions[0].filter(function(o) { return key && o.dataset.category === key; });
  while (account.options.length > 1) { account.remove(1); }
  options.forEach(function(o) { account.add(o); });
  if (keep && options.some(function(o) { return o.value === keep; })) { account.value = keep; }
  else if (key && options.length === 1) { account.value = options[0].value; }
  else { placeholder.selected = true; account.value = ''; }
}

// The kind of entry - expense, income or transfer - picked above the
// category narrows the category list to its group (the groups are kept
// aside once read). The choice is remembered in a cookie; an existing
// entry's page starts on its own account's kind. On a transfer the rows
// are named by what they are: the account paid to, and the one paid from.
var addformCategoryGroups = null;
function addformKindInit() {
  var kinds = document.querySelector('#addform .kind-select');
  var category = document.querySelector('#addform .category-select');
  if (!kinds || !category) { return; }
  if (!addformCategoryGroups) {
    addformCategoryGroups = [].map.call(category.querySelectorAll('optgroup'), function(g) {
      return { kind: g.dataset.kind, options: [].slice.call(g.children) };
    });
    kinds.querySelectorAll('.kind-option').forEach(function(b) {
      b.addEventListener('click', function() {
        addformSetKind(b.dataset.kind, '');
        addformSetCategory('');
        setCookie('addkind', b.dataset.kind);
      });
    });
    var swap = document.querySelector('#addform .swap-sides');
    if (swap) { swap.addEventListener('click', addformSwapSides); }
  }
  var account = document.querySelector('#addform select[name=account][data-row="1"]');
  var chosen = account && account.options[account.selectedIndex];
  var key = chosen && chosen.value ? chosen.dataset.category : '';
  addformSetKind(key ? addformKindOf(key) : (getCookie('addkind') || 'expenses'), key);
}
function addformKindOf(categoryKey) {
  var g = addformCategoryGroups.find(function(g) {
    return g.options.some(function(o) { return o.value === categoryKey; });
  });
  return g ? g.kind : 'expenses';
}
function addformSetKind(kind, categoryKey) {
  var kinds = document.querySelector('#addform .kind-select');
  var category = document.querySelector('#addform .category-select');
  if (!kinds || !category || !addformCategoryGroups) { return; }
  if (!addformCategoryGroups.some(function(g) { return g.kind === kind; }) && addformCategoryGroups.length) {
    kind = addformCategoryGroups[0].kind;
  }
  kinds.dataset.kind = kind;
  kinds.querySelectorAll('.kind-option').forEach(function(b) {
    var on = b.dataset.kind === kind;
    b.classList.toggle('active', on);
    b.setAttribute('aria-pressed', on ? 'true' : 'false');
  });
  // the category list: its placeholder, then the kind's categories
  while (category.children.length > 1) { category.removeChild(category.lastChild); }
  addformCategoryGroups.forEach(function(g) {
    if (g.kind === kind) { g.options.forEach(function(o) { category.appendChild(o); }); }
  });
  category.value = categoryKey || '';
  if (category.value !== (categoryKey || '')) { category.value = ''; }
  var transfer = kind === 'transfer';
  var first = document.querySelector('#addform select[name=account][data-row="1"] option[value=""]');
  var second = document.querySelector('#addform select[name=account][data-row="2"] option[value=""]');
  if (first) { first.textContent = transfer ? kinds.dataset.account : kinds.dataset.subcategory; }
  if (second) { second.textContent = kinds.dataset.account; }
  // on a transfer, "To" and "From" stand beside the two lists
  document.querySelectorAll('#addform .account-group').forEach(function(g) {
    var side = g.querySelector('.account-side');
    var sel = g.querySelector('select[name=account]');
    if (!side || !sel) { return; }
    side.textContent = sel.dataset.row === '1' ? kinds.dataset.toSide : (sel.dataset.row === '2' ? kinds.dataset.fromSide : '');
    side.hidden = !transfer || !side.textContent;
  });
  // A transfer is two accounts and an amount: no payee (unless the entry
  // has one), no category; the first row lists every account of the kind,
  // grouped as the categories would have been.
  var payeeGroup = document.querySelector('#addform .payee-group');
  var payeeInput = document.querySelector('#addform input[name=description]');
  if (payeeGroup) { payeeGroup.hidden = transfer && !(payeeInput && payeeInput.value); }
  var swapWrap = document.querySelector('#addform .swap-wrap');
  if (swapWrap) { swapWrap.hidden = !transfer; }
  var categoryCol = category.parentElement;
  if (categoryCol) { categoryCol.hidden = transfer; }
  if (transfer) {
    var account = document.querySelector('#addform select[name=account][data-row="1"]');
    var keep = account.value;
    var group = addformCategoryGroups.find(function(g) { return g.kind === 'transfer'; });
    while (account.options.length > 1) { account.remove(1); }
    (group ? group.options : []).forEach(function(c) {
      var og = document.createElement('optgroup');
      og.label = c.textContent;
      addformAccountOptions[0].filter(function(o) { return o.dataset.category === c.value; })
        .forEach(function(o) { og.appendChild(o); });
      if (og.children.length) { account.appendChild(og); }
    });
    if (keep && [].some.call(account.options, function(o) { return o.value === keep; })) { account.value = keep; }
    else { account.options[0].selected = true; account.value = ''; }
  }
}

// Insert another posting row in the add form.
function addformAddPosting() {
  var addform = document.getElementById('addform');
  if (!addform) { return; }
  var groups = addform.querySelectorAll('.account-group');
  var newrow = groups[groups.length - 1].cloneNode(true);
  newrow.classList.add('added-row');
  var num = groups.length + 1;

  // The cloned row may carry error styling from a previous failed submit.
  newrow.querySelectorAll('.has-error').forEach(function(el) { el.classList.remove('has-error'); });
  newrow.querySelectorAll('.error-block').forEach(function(el) { el.remove(); });

  var account = newrow.querySelector('select[name=account]');
  var amount = newrow.querySelector('input[name=amount]');
  var side = newrow.querySelector('.account-side');
  if (side) { side.textContent = ''; side.hidden = true; }
  var swap = newrow.querySelector('.swap-wrap');
  if (swap) { swap.remove(); }
  account.value = '';
  account.dataset.row = num;
  amount.value = '';

  // The placeholder templates come from the page, in its language.
  var postings = addform.querySelector('.account-postings');
  account.options[0].textContent = (postings.dataset.accountPlaceholder || 'Account {n}').replace('{n}', num);
  amount.placeholder = (postings.dataset.amountPlaceholder || 'Amount {n}').replace('{n}', num);

  addform.querySelector('.account-postings').appendChild(newrow);
  newrow.querySelector('select[name=account]').focus();
}

//----------------------------------------------------------------------
// SIDEBAR

function sidebarToggle() {
  var sidebar = document.getElementById('sidebar-menu');
  var main = document.getElementById('main-content');
  var spacer = document.getElementById('spacer');
  [sidebar, spacer].forEach(function(el) {
    if (el) { el.classList.toggle('col-md-4'); el.classList.toggle('col-sm-4'); el.classList.toggle('col-any-0'); }
  });
  if (main) {
    main.classList.toggle('col-md-8'); main.classList.toggle('col-sm-8');
    main.classList.toggle('col-md-12'); main.classList.toggle('col-sm-12');
  }
  // The server reads this cookie, so the next page renders the way we left it.
  setCookie('showsidebar', sidebar && sidebar.classList.contains('col-any-0') ? '0' : '1');
}

function emptyAccountsToggle() {
  document.querySelectorAll('.acct.empty').forEach(function(el) {
    el.parentNode.classList.toggle('hide');
  });
  setCookie('hideemptyaccts', getCookie('hideemptyaccts') === '1' ? '0' : '1');
}

function setCookie(name, value) {
  document.cookie = name + '=' + value + '; path=/; max-age=31536000; samesite=lax';
}

function getCookie(name) {
  return document.cookie.split('; ').reduce(function(found, c) {
    var parts = c.split('=');
    return parts[0] === name ? parts.slice(1).join('=') : found;
  }, undefined);
}

//----------------------------------------------------------------------
// ENTRY TOOLTIP
//
// Hovering a transaction in the journal or a register shows its journal
// entry, the way a browser shows a title attribute, but in a fixed-width
// font so that the amounts line up as they do in the journal. The rows carry
// the entry in a data-entry attribute; in a title attribute the browser would
// draw it itself, in a proportional font and wrapped at its own narrow width.
// As with titles, the innermost one applies: over an account link, the
// link's title (the full account name) shows instead.

// Entries dated after today (recurring ones written ahead) are folded
// away: their rows get hidden, and the row with the button, which the
// page sends hidden, shows and unfolds them. Without this script every
// row shows, and the button's row stays hidden.
// In the journal an entry is a title row and its posting rows: under the
// pointer, all of them light up together (hledger.css), not one at a time.
function entryHoverInit() {
  var table = document.querySelector('.transactionsreport');
  if (!table) { return; }
  var lit = [];
  function rowsOf(row) {
    var title = row.classList.contains('title') ? row : row.previousElementSibling;
    while (title && !title.classList.contains('title')) { title = title.previousElementSibling; }
    if (!title) { return []; }
    var rows = [title];
    for (var r = title.nextElementSibling; r && r.classList.contains('posting'); r = r.nextElementSibling) { rows.push(r); }
    return rows;
  }
  function clear() { lit.forEach(function(r) { r.classList.remove('entry-hover'); }); lit = []; }
  table.addEventListener('mouseover', function(e) {
    var row = e.target.closest('tr');
    if (!row || !(row.classList.contains('title') || row.classList.contains('posting'))) { clear(); return; }
    if (lit.indexOf(row) >= 0) { return; }
    clear();
    lit = rowsOf(row);
    lit.forEach(function(r) { r.classList.add('entry-hover'); });
  });
  table.addEventListener('mouseleave', clear);
}

// A journal or register row with data-href (one the reader may edit)
// leads to its entry's page: a tap or click anywhere on it that is not on
// a link or a control, and selects no text, goes there. In the journal a
// posting row belongs to the title row above it. On a phone the entry's
// block is one target: a tap on a link inside it goes to the entry too.
function entryRowsInit() {
  var table = document.querySelector('.transactionsreport, .registerreport');
  if (!table) { return; }
  table.addEventListener('click', function(e) {
    if (e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) { return; }
    var row = e.target.closest('tr');
    if (!row) { return; }
    var phoneBlock = e.target.closest('td.entry-card') && window.matchMedia('(max-width: 767px)').matches;
    if (!phoneBlock && e.target.closest('a, button, input, select, label, textarea')) { return; }
    var sel = window.getSelection();
    if (sel && sel.toString()) { return; }
    while (row && !row.dataset.href && row.classList.contains('posting')) { row = row.previousElementSibling; }
    if (row && row.dataset.href) {
      e.preventDefault();
      window.location = row.dataset.href;
    }
  });
}

//----------------------------------------------------------------------
// THE AMOUNT'S CALCULATOR
//
// An amount can be a sum: 12.50+3.99*2. On a phone a keypad stands in for
// the keyboard (see add-form.hamlet), as AndroMoney's does; elsewhere the
// sum is typed. Either way it is worked out when the field is left, with
// Enter, with OK, or when the form is sent, and its result shows as it is
// keyed. A plain number is left as typed.

// The result of a sum, as the amount to write: rounded to the cents, or
// to as many places as its numbers have (at most six), and whole when its
// numbers and its result are; or null when the text is not a sum (a plain
// number, or anything else).
function calcEvaluate(text) {
  var s = String(text).replace(/\s+/g, '').replace(/×/g, '*').replace(/÷/g, '/').replace(/[−–]/g, '-');
  s = s.replace(/(\d),(?=\d{3}(?!\d))/g, '$1').replace(/,/g, '.');
  if (!/^[-+*\/().0-9]+$/.test(s) || !/[-+*\/]/.test(s.replace(/^-/, ''))) { return null; }
  var i = 0, places = 0, whole = true;
  function number() {
    var m = /^(\d+\.?\d*|\.\d+)/.exec(s.slice(i));
    if (!m) { throw 0; }
    i += m[0].length;
    if (m[0].indexOf('.') >= 0) { whole = false; places = Math.max(places, m[0].split('.')[1].length); }
    return parseFloat(m[0]);
  }
  function factor() {
    if (s[i] === '-') { i++; return -factor(); }
    if (s[i] === '+') { i++; return factor(); }
    if (s[i] === '(') { i++; var v = sum(); if (s[i] !== ')') { throw 0; } i++; return v; }
    return number();
  }
  function product() {
    var v = factor();
    while (s[i] === '*' || s[i] === '/') {
      var op = s[i++], w = factor();
      if (op === '/') { if (w === 0) { throw 0; } v /= w; } else { v *= w; }
    }
    return v;
  }
  function sum() {
    var v = product();
    while (s[i] === '+' || s[i] === '-') { var op = s[i++], w = product(); v = op === '+' ? v + w : v - w; }
    return v;
  }
  var v;
  try { v = sum(); } catch (e) { return null; }
  if (i !== s.length || !isFinite(v)) { return null; }
  var dec = Math.min(6, Math.max(2, places)), f = Math.pow(10, dec);
  var r = Math.round(v * f) / f;
  if (r === 0) { r = 0; }   // (not -0)
  return whole && Number.isInteger(r) ? String(r) : r.toFixed(dec);
}

function calcInit() {
  var form = document.getElementById('addform');
  if (!form || form.dataset.calcWired) { return; }
  form.dataset.calcWired = '1';
  var pad = form.querySelector('.calc-pad');
  var preview = pad && pad.querySelector('.calc-preview');
  var inline = form.querySelector('.calc-inline');
  var touch = !!pad && window.matchMedia('(pointer: coarse)').matches;
  var target = null;
  var isAmount = function(el) { return el && el.matches && el.matches('input[name=amount]'); };
  var amounts = function() { return form.querySelectorAll('input[name=amount]'); };
  var result = function(input) { var r = calcEvaluate(input.value); return r === null ? '' : '= ' + r; };
  // a sum left in a field: its result, its last operator dropped if any
  function settle(input) {
    var r = calcEvaluate(input.value.replace(/[-+*\/×÷−]+$/, ''));
    if (r !== null) { input.value = r; }
  }
  // the result under the field being typed into (no pad)
  function showInline(input) {
    if (!inline) { return; }
    var text = result(input);
    var box = input.closest('.account-group') || input.closest('.form-group');
    if (box && inline.previousElementSibling !== box) { box.after(inline); }
    inline.textContent = text;
    inline.hidden = !text;
  }
  function hideInline() { if (inline) { inline.hidden = true; inline.textContent = ''; } }
  // the pad
  function open(input) {
    if (target === input && !pad.hidden) { return; }
    if (target && target !== input) { settle(target); target.classList.remove('calc-target'); }
    target = input;
    target.classList.add('calc-target');
    pad.hidden = false;
    form.classList.add('calc-open');
    preview.textContent = result(target);
    target.scrollIntoView({ block: 'nearest' });
  }
  function close() {
    if (!target) { return; }
    settle(target);
    target.classList.remove('calc-target');
    target = null;
    pad.hidden = true;
    form.classList.remove('calc-open');
    preview.textContent = '';
  }
  function press(key) {
    var v = target.value;
    if (key === 'ok') { close(); if (document.activeElement && isAmount(document.activeElement)) { document.activeElement.blur(); } return; }
    if (key === 'clear') { v = ''; }
    else if (key === 'del') { v = v.slice(0, -1); }
    else if ('+-*/'.indexOf(key) >= 0) {
      var sign = { '+': '+', '-': '−', '*': '×', '/': '÷' }[key];
      if (v === '' && key !== '-') { return; }
      v = v.replace(/[-+*\/×÷−]$/, '') + sign;
    } else if (key === '.') {
      var part = v.split(/[-+*\/×÷−]/).pop();
      if (part.indexOf('.') >= 0) { return; }
      v += part === '' ? '0.' : '.';
    } else {
      v = (v === '0' ? '' : v) + key;
    }
    target.value = v;
    preview.textContent = result(target);
  }
  if (touch) {
    // the pad stands in for the keyboard: the amounts take no typing
    var keep = function() { amounts().forEach(function(a) { a.readOnly = true; }); };
    keep();
    // the pad is open while an amount has the focus: focus elsewhere closes it
    form.addEventListener('focusin', function(e) {
      if (isAmount(e.target)) { keep(); open(e.target); }
      else if (!pad.contains(e.target)) { close(); }
    });
    form.addEventListener('click', function(e) { if (isAmount(e.target)) { open(e.target); } });
    // a key keeps the focus where it is, and acts once the tap is done (had
    // it acted on the way down, OK's closing the pad would hand the rest
    // of the tap to whatever lay under it)
    pad.addEventListener('pointerdown', function(e) { e.preventDefault(); });
    pad.addEventListener('mousedown', function(e) { e.preventDefault(); });
    pad.addEventListener('click', function(e) {
      var k = e.target.closest('.calc-key');
      if (k && target) { press(k.dataset.key); }
    });
    // a tap on something that takes no focus closes it too, once done
    document.addEventListener('click', function(e) {
      if (target && !pad.contains(e.target) && e.target !== target) { close(); }
    }, true);
    var dialog = form.closest('dialog');
    if (dialog) { dialog.addEventListener('close', close); }
  } else {
    form.addEventListener('input', function(e) { if (isAmount(e.target)) { showInline(e.target); } });
    form.addEventListener('focusout', function(e) { if (isAmount(e.target)) { settle(e.target); hideInline(); } });
    form.addEventListener('keydown', function(e) { if (e.key === 'Enter' && isAmount(e.target)) { settle(e.target); hideInline(); } });
  }
  // whatever is left as a sum is worked out before the form goes
  form.addEventListener('submit', function() { close(); amounts().forEach(settle); hideInline(); }, true);
}

// On an entry's page, Edit shows the form in place, without reloading.
function entryPageInit() {
  var button = document.querySelector('.entry-actions .entry-edit-button');
  var form = document.querySelector('#addform.entry-edit-form');
  if (!button || !form) { return; }
  button.addEventListener('click', function(e) {
    e.preventDefault();
    form.hidden = false;
    button.hidden = true;
    var first = form.querySelector('input[name=amount]');
    if (first) { first.focus(); }
    form.scrollIntoView({ behavior: 'smooth', block: 'start' });
  });
}

// The sidebar's account tree folds: the chevron before an account hides
// the accounts under it, and the browser remembers which are folded
// (localStorage). The account shown stays in sight: its parents are open
// for it, until one is folded on purpose.
function sidebarTreeInit() {
  var rows = [].slice.call(document.querySelectorAll('#sidebar-menu tr[data-acct]'));
  if (!rows.length) { return; }
  var key = 'hledgerSidebarFolded';
  var folded = {};
  try { JSON.parse(localStorage.getItem(key) || '[]').forEach(function(a) { folded[a] = true; }); } catch (e) {}
  var current = document.querySelector('#sidebar-menu tr.inacct[data-acct]');
  var open = {};   // the parents of the account shown, open whatever was remembered
  if (current) {
    var parts = current.dataset.acct.split(':');
    for (var i = 1; i < parts.length; i++) { open[parts.slice(0, i).join(':')] = true; }
  }
  function isFolded(a) { return !!folded[a] && !open[a]; }
  function save() { try { localStorage.setItem(key, JSON.stringify(Object.keys(folded))); } catch (e) {} }
  function apply() {
    rows.forEach(function(r) {
      var a = r.dataset.acct;
      r.hidden = Object.keys(folded).some(function(f) { return isFolded(f) && a.indexOf(f + ':') === 0; });
      var t = r.querySelector('.tree-toggle');
      if (t) {
        var isOpen = !isFolded(a);
        t.setAttribute('aria-expanded', isOpen ? 'true' : 'false');
        t.setAttribute('aria-label', isOpen ? t.dataset.collapse : t.dataset.expand);
      }
    });
  }
  rows.forEach(function(r) {
    var t = r.querySelector('.tree-toggle');
    if (!t) { return; }
    t.addEventListener('click', function(e) {
      e.preventDefault();
      var a = r.dataset.acct;
      if (isFolded(a)) { delete folded[a]; } else { folded[a] = true; delete open[a]; }
      save();
      apply();
    });
  });
  // The whole tree at once: fold all leaves the top level, and the
  // account shown; unfold all opens everything.
  var tools = document.querySelector('#sidebar-menu .tree-tools');
  if (tools) {
    tools.hidden = false;
    tools.querySelectorAll('.tree-all').forEach(function(b) {
      b.addEventListener('click', function() {
        folded = {};
        if (b.dataset.fold === 'all') {
          rows.forEach(function(r) { if (r.querySelector('.tree-toggle')) { folded[r.dataset.acct] = true; } });
        }
        save();
        apply();
      });
    });
  }
  apply();
}

function futureRowsInit() {
  document.querySelectorAll('tr.future-toggle').forEach(function(row) {
    var table = row.closest('table');
    var button = row.querySelector('.future-button');
    var rows = table.querySelectorAll('tr.future');
    if (!rows.length || !button) { return; }
    var showLabel = button.textContent;
    function fold(hide) {
      rows.forEach(function(r) { r.hidden = hide; });
      button.textContent = hide ? showLabel : button.dataset.hide;
      button.setAttribute('aria-expanded', hide ? 'false' : 'true');
    }
    fold(true);
    row.hidden = false;
    button.addEventListener('click', function() { fold(!rows[0].hidden); });
  });
}

function entryTooltipInit() {
  if (!document.querySelector('[data-entry]')) { return; }
  var tip = document.createElement('div');
  tip.className = 'entry-tooltip';
  tip.setAttribute('aria-hidden', 'true');
  tip.hidden = true;
  document.body.appendChild(tip);

  var row = null;         // the element with an entry under the pointer
  var timer = null;       // set while waiting to show its entry
  var clicked = false;    // set by a click, until the pointer leaves the row
  var x, y;               // the pointer position, where the entry appears

  function show() {
    timer = null;
    entryTooltipFill(tip, row.getAttribute('data-entry'));
    entryTooltipPlace(tip, x, y);
  }
  function hide() {
    clearTimeout(timer);
    timer = null;
    tip.hidden = true;
  }

  // Mouse only. A touch has no hover, and changing the page when a touch
  // arrives over a link makes iOS treat the first tap as a hover, so the link
  // would take two taps.
  function track(e) {
    if (e.pointerType !== 'mouse') { return; }
    x = e.clientX;
    y = e.clientY;
    var el = e.target.closest('[title], [data-entry]');
    var over = el && el.hasAttribute('data-entry') ? el : null;
    if (over !== row) {
      // Moving on to the next entry while one is showing shows the next at
      // once, as with browser tooltips; otherwise it appears after a pause.
      var showing = !tip.hidden;
      hide();
      row = over;
      clicked = false;
      if (row) {
        if (showing) { show(); } else { timer = setTimeout(show, 500); }
      }
    } else if (row && tip.hidden && !timer && !clicked) {
      // Still over the row after a scroll or a key press hid it.
      timer = setTimeout(show, 500);
    }
  }
  document.addEventListener('pointerover', track);
  document.addEventListener('pointermove', track);

  // Also like a browser tooltip, it goes away when the pointer leaves the
  // window, and on a click, a key press or a scroll. After a click it stays
  // away until the pointer leaves the row, so as not to cover a selection
  // being made. Scroll events don't bubble, and the main pane scrolls by
  // itself, hence the capture.
  document.addEventListener('pointerout', function(e) {
    if (!e.relatedTarget) { hide(); row = null; }
  });
  document.addEventListener('pointerdown', function() {
    hide();
    clicked = true;
  });
  document.addEventListener('keydown', hide);
  // A scroll before the entry has appeared leaves it pending, so that the
  // scroll a browser makes to bring a row under the pointer (eg for a
  // script) does not cancel it; the browsers' own pointer move after a
  // scroll puts the pointer over the right row.
  document.addEventListener('scroll', function() { if (!tip.hidden) { hide(); } }, true);
  window.addEventListener('blur', hide);
}

// Put an entry's lines in the tooltip.
function entryTooltipFill(tip, entry) {
  tip.textContent = '';
  entry.replace(/\s+$/, '').split('\n').forEach(function(line) {
    var div = document.createElement('div');
    // Set as text, so that journal content cannot be parsed as markup.
    div.textContent = line;
    // A line too long for the window wraps, and its continuation is indented
    // past the line's own indentation, so the entry keeps its shape.
    var indent = (line.match(/^ */)[0].length + 2) + 'ch';
    div.style.paddingLeft = indent;
    div.style.textIndent = '-' + indent;
    tip.appendChild(div);
  });
}

// Show the tooltip below the pointer, or above it if there is no room below,
// and keep it within the window.
function entryTooltipPlace(tip, x, y) {
  var margin = 8;
  var vw = document.documentElement.clientWidth;
  var vh = document.documentElement.clientHeight;
  tip.style.maxWidth = (vw - 2 * margin) + 'px';
  tip.style.maxHeight = (vh - 2 * margin) + 'px';
  tip.style.left = '0';
  tip.style.top = '0';
  tip.hidden = false;
  var w = tip.offsetWidth;
  var h = tip.offsetHeight;
  var top = y + 20;  // clear of the pointer's arrow
  if (top + h > vh - margin) { top = y - margin - h; }
  if (top < margin) { top = vh - margin - h; }
  tip.style.left = Math.max(margin, Math.min(x, vw - margin - w)) + 'px';
  tip.style.top = top + 'px';
  // An entry taller than the window is cut off; the css marks the cut.
  tip.classList.toggle('clipped', tip.scrollHeight > tip.clientHeight);
}

//----------------------------------------------------------------------
// REGISTER CHART
//
// The register page's balance chart, drawn with flot. chart.hamlet renders
// only the markup, with the data as JSON on #register-chart, so that pages
// carry no inline scripts beyond the nonced ones (#2703). flot needs jquery,
// so this section uses it too.

// Draw the register chart, if this page has one.
function registerChartInit() {
  var $chartdiv = $('#register-chart');
  // flot needs a container with a size, so do nothing while it is hidden.
  if (!$chartdiv.length || !$chartdiv.is(':visible')) { return; }
  var $label = $('#register-chart-label');
  var commodities = JSON.parse($chartdiv.attr('data-series'));
  // Each commodity is drawn as two flot series over the same points: a
  // stepped line for the running balance, and one clickable, hoverable point
  // per transaction. A point is [timestamp, balance, amount text, balance
  // text, transaction text, the transaction's row id]: flot reads the first
  // two, the tooltip and click handlers the rest.
  var series = [];
  commodities.forEach(function(c, i) {
    series.push({
      data: c.points, label: c.label, color: i,
      lines: { show: true, steps: true }, points: { show: false },
      clickable: false, hoverable: false,
    });
    series.push({
      data: c.points, color: i,
      lines: { show: false }, points: { show: true },
    });
  });
  // The page follows a change of color scheme by itself, and prints in the
  // light one, but the chart is drawn on a canvas; draw it again for those.
  var draw = function() {
    $label.text($chartdiv.attr('data-title'));
    registerChartLegend($label, registerChart($chartdiv, series));
  };
  draw();
  ['(prefers-color-scheme: dark)', 'print'].forEach(function(query) {
    window.matchMedia(query).addEventListener('change', draw);
  });
  // flot draws to the width the chart has, and so does it again when that
  // changes, eg when a phone is turned, which would otherwise leave the
  // chart too wide for the page. After a pause, so as not to draw at every
  // step of a window being resized.
  var width = $chartdiv.width(), resized = null;
  window.addEventListener('resize', function() {
    clearTimeout(resized);
    resized = setTimeout(function() {
      if ($chartdiv.width() !== width) {
        width = $chartdiv.width();
        draw();
      }
    }, 200);
  });
  $chartdiv.bind('plotclick', registerChartClick);
  $chartdiv.bind('plotselected', registerChartSelect);
}

// The reports page's chart over time (#reports-chart, its data as JSON on
// the div like the register's): the lines of the choice picked in the
// list beside the title (the whole, a part of the ring, or at the top all
// the kinds), a point per period, the periods' labels as the ticks.
// Hovering a point shows its figure, a click opens that period's
// breakdown. The lines take the ring's colours; when there are several,
// the legend names them.
function reportsChartInit() {
  var $div = $('#reports-chart');
  if (!$div.length || !$div.is(':visible')) { return; }
  var data = JSON.parse($div.attr('data-series'));
  var $label = $('#reports-chart-label');
  var $pick = $('#reports-chart-pick');
  var chosen = 0;
  data.choices.forEach(function(c, i) { $('<option>').val(i).text(c.label).appendTo($pick); });
  $pick.on('change', function() { chosen = parseInt($pick.val(), 10) || 0; draw(); });
  // a slice's colour, as the stylesheet gives it to the ring and the bars
  function sliceColour(i) {
    var probe = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    var rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    rect.setAttribute('class', 'bar-fill slice-' + i);
    probe.appendChild(rect);
    document.body.appendChild(probe);
    var c = getComputedStyle(rect).fill;
    document.body.removeChild(probe);
    return c;
  }
  var draw = function() {
    var style = getComputedStyle(document.documentElement);
    var color = function(name) { return style.getPropertyValue(name).trim(); };
    var shown = (data.choices[chosen] || data.choices[0]).lines.map(function(i) { return data.series[i]; });
    var series = shown.map(function(s) {
      return {
        data: s.values.map(function(v, i) { return [i, v]; }),
        label: shown.length > 1 ? s.label : undefined,
        color: sliceColour(s.colour), texts: s.texts, name: s.label,
        lines: { show: true }, points: { show: true },
      };
    });
    // a label per period, every other one when they would crowd
    var room = $div.width() / data.labels.length;
    var ticks = data.labels.map(function(l, i) { return [i, room < 40 && i % 2 ? '' : l]; });
    var plot = $div.plot(series, {
      series: { lines: { lineWidth: 2 }, points: { radius: 3, fillColor: color('--bg') }, shadowSize: 0 },
      xaxis: { ticks: ticks, min: -0.5, max: data.labels.length - 0.5 },
      yaxis: { min: 0, tickDecimals: 0 },
      legend: { show: false },
      grid: { color: color('--chart-grid'), borderWidth: { top: 0, right: 0, bottom: 1, left: 0 },
              hoverable: true, autoHighlight: true, clickable: true },
      tooltip: true,
      tooltipOpts: {
        defaultTheme: false,
        content: function(label, x, y, item) {
          var text = item.series.name + ', ' + data.labels[item.dataIndex] + ': ' + item.series.texts[item.dataIndex];
          return $('<div>').append(document.createTextNode(text)).html();
        },
        onHover: function(item, $tip) { $tip.css('border-color', item.series.color); },
      },
    }).data('plot');
    $label.empty();
    registerChartLegend($label, plot);
  };
  draw();
  ['(prefers-color-scheme: dark)', 'print'].forEach(function(query) {
    window.matchMedia(query).addEventListener('change', draw);
  });
  var width = $div.width(), resized = null;
  window.addEventListener('resize', function() {
    clearTimeout(resized);
    resized = setTimeout(function() {
      if ($div.width() !== width) { width = $div.width(); draw(); }
    }, 200);
  });
  $div.bind('plotclick', function(ev, pos, item) {
    if (item && data.urls[item.dataIndex]) { window.location = data.urls[item.dataIndex]; }
  });
}

function registerChart($container, series) {
  // The colors come from the palette in hledger.css, for the current scheme.
  var style = getComputedStyle(document.documentElement);
  var color = function(name) { return style.getPropertyValue(name).trim(); };
  var xaxis = {
    mode: "time",
    timeformat: "%Y/%m/%d",
  };
  // On a narrow screen (below the breakpoint in hledger.css) flot crowds
  // the date labels until they overlap, and centers the last one on its
  // tick, past the chart's edge, where it can widen the page. There, ask
  // for a label per 110px or so, and keep room for half a label at each end.
  if (window.matchMedia('(max-width: 767px)').matches) {
    xaxis.ticks = Math.max(2, Math.floor($container.width() / 110));
    xaxis.reserveSpace = true;
  }
  // https://github.com/flot/flot/blob/master/API.md
  return $container.plot(
    series,
    {
      colors: [color('--chart-line'), color('--income'), color('--negative')],
      series: {
        // hollow points: filled with the page's own background
        points: { fillColor: color('--bg') },
      },
      xaxis: xaxis,
      selection: {
        mode: "x"
      },
      // flot's legend is built from inline style attributes, which the
      // Content-Security-Policy blocks; registerChartLegend draws ours.
      legend: {
        show: false
      },
      grid: {
        color: color('--chart-grid'),
        markings: function () {
          var now = Date.now();
          return [
            {
              xaxis: { to: now }, // past
              yaxis: { to: 0 },   // <0
              color: color('--chart-past-negative'),
            },
            {
              xaxis: { from: now }, // future
              yaxis: { from: 0 },   // >0
              color: color('--chart-future'),
            },
            {
              xaxis: { from: now }, // future
              yaxis: { to: 0 },     // <0
              color: color('--chart-future-negative'),
            },
            {
              yaxis: { from: 0, to: 0 }, // =0
              color: color('--chart-zero'),
              lineWidth:1
            },
          ];
        },
        hoverable: true,
        autoHighlight: true,
        clickable: true,
      },
      // https://github.com/krzysu/flot.tooltip
      tooltip: true,
      tooltipOpts: {
        // its look is in hledger.css (#flotTip), where the palette reaches it
        defaultTheme: false,
        xDateFormat: "%Y/%m/%d",
        content:
          function(label, x, y, flotitem) {
            var data = flotitem.series.data[flotitem.dataIndex];
            // The plugin renders this as html. Build it from text nodes and
            // let the browser serialize it, so the journal text cannot be
            // parsed as markup.
            return $('<div>')
              .append(document.createTextNode(data[3] + " balance on %x after " + data[2] + " posted by transaction:"))
              .append($('<pre>').text(data[4]))
              .html();
          },
        onHover: function(flotitem, $tooltipel) {
          $tooltipel.css('border-color', flotitem.series.color);
        },
      },
    }
  ).data("plot");
}

// Add the legend to the label line: a color swatch and the commodity for
// each balance line.
function registerChartLegend($label, plot) {
  plot.getData().forEach(function(s) {
    if (typeof s.label !== 'string') { return; }
    var $swatch = $('<span class="legend-swatch">').css('background-color', s.color);
    $('<span class="legend-item">').append($swatch, document.createTextNode(s.label)).appendTo($label);
  });
}

// Handle a click on a chart point: scroll to that transaction's row.
function registerChartClick(ev, pos, item) {
  if (!item) { return; }
  var id = String(item.series.data[item.dataIndex][5]);
  var target = document.getElementById(id);
  if (target) {
    window.location.hash = '#' + id;
    $('html, body').animate({ scrollTop: $(target).offset().top }, 1000);
  }
}

// Handle a selection (zoom) on the chart: reload with a date: query for
// the selected range.
function registerChartSelect(ev, ranges) {
  // Reconstruct from/to dates carefully based on the selected x-values.
  // Those x values are unix timestamps (milliseconds since epoch) enclosing the data points' timestamps.
  // Those are generated by dayToUtcNoonTimestamp, and are UTC times representing the transaction dates.
  var from = new Date(ranges.xaxis.from);
  var to = new Date(ranges.xaxis.to + 1 * 24 * 60 * 60 * 1000);
  // as a date: term reads it: YYYY-MM-DD..YYYY-MM-DD, the end exclusive
  var iso = function(d) {
    return d.getUTCFullYear() + '-' + String(d.getUTCMonth() + 1).padStart(2, '0') +
      '-' + String(d.getUTCDate()).padStart(2, '0');
  };
  var range = iso(from) + '..' + iso(to);
  // The base link is this register's url without its date terms; add ours.
  var url = new URL($('#register-chart').attr('data-baselink'), document.baseURI);
  var q = url.searchParams.get('q');
  url.searchParams.set('q', (q ? q + ' ' : '') + 'date:' + range);
  document.location = url.href;
}

//----------------------------------------------------------------------
// BROWSE MODE

// In the default --serve-browse mode the server exits once no browser
// window has shown it for fifteen minutes (serveAndBrowse in Main.hs). It
// knows a window is open because the page pings it: on load, every 30
// seconds, and whenever the page is shown again. Browsers run the timers of
// background tabs less often, so the pings from a hidden page can be minutes
// apart; the ping on showing makes up for that as soon as the page is seen.
// Pages are marked for this by defaultLayout in browse mode only; the server
// answers /_ping in that mode only. The ping goes to the page's own origin,
// whatever address the browser reached us at (the policy allows requests to
// our origin only), under the base url's path, in case a proxy in front of
// us expects one.
//
// A ping that can't reach the server means it has stopped, so the page shows
// the #server-stopped notice, and hides it again if a later ping gets through
// (eg after the server was restarted on the same address).
function browsePingInit() {
  if (!document.body.hasAttribute('data-browse-mode')) { return; }
  var base = new URL(document.hledgerWebBaseurl, document.baseURI);
  var url = base.pathname.replace(/\/$/, '') + '/_ping';
  var notice = document.getElementById('server-stopped');
  var ping = function() {
    fetch(url, { cache: 'no-store' }).then(
      function() { if (notice) { notice.hidden = true; } },
      function() { if (notice) { notice.hidden = false; } });
  };
  ping();
  setInterval(ping, 30000);
  document.addEventListener('visibilitychange', function() {
    if (document.visibilityState === 'visible') { ping(); }
  });
  // A page restored from the back/forward cache didn't run meanwhile.
  window.addEventListener('pageshow', function(e) {
    if (e.persisted) { ping(); }
  });
}
