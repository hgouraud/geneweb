// Bulk corrections of places (m=PLACE_TERMS&fix=...), table.plan-keys:
// in the field of a new place, Return applies this line at once and
// Shift+Return skips it; both go on to the next line. The lines applied or
// skipped are unchecked, so that the submit button leaves them alone. The
// lines come unchecked, and the submit button asks for a confirmation of the
// lines checked.
(function () {
  'use strict';
  const table = document.querySelector('table.plan-keys');
  if (!table) return;
  const form = table.closest('form');
  const rows = Array.from(table.querySelectorAll('tbody tr'));
  const field = (row) => row.querySelector('input[type="text"]');
  const box = (row) => row.querySelector('input[type="checkbox"]');
  const isLine = (k) => /^n?p[0-9]+$/.test(k);
  // one update at a time: the base is locked by each of them
  let queue = Promise.resolve();

  const focusNext = (row) => {
    for (let i = rows.indexOf(row) + 1; i < rows.length; i++) {
      const f = field(rows[i]);
      if (f && !f.disabled) {
        f.focus();
        f.select();
        return;
      }
    }
    form.querySelector('button[type="submit"]').focus();
  };

  const done = (row, cls) => {
    box(row).checked = false;
    box(row).disabled = true;
    field(row).disabled = true;
    row.classList.add(cls);
  };

  const apply = (row) => {
    const body = new URLSearchParams();
    for (const [k, v] of new FormData(form)) if (!isLine(k)) body.append(k, v);
    body.append(box(row).name, 'on');
    body.append(field(row).name, field(row).value);
    done(row, 'table-warning');
    queue = queue
      .then(() => fetch(form.action, { method: 'POST', body: body }))
      .then((r) => (r.ok ? r.text() : Promise.reject(new Error(r.status))))
      .then((html) => {
        row.classList.remove('table-warning');
        const ok = html.includes('alert-success') && !html.includes('alert-danger');
        row.classList.add(ok ? 'table-success' : 'table-danger');
      })
      .catch(() => {
        row.classList.remove('table-warning');
        row.classList.add('table-danger');
      });
  };

  form.addEventListener('submit', (e) => {
    const n = rows.filter((r) => box(r) && box(r).checked).length;
    if (n === 0) {
      e.preventDefault();
      window.alert(table.dataset.none);
    } else if (!window.confirm(table.dataset.confirm.replace('%d', n))) {
      e.preventDefault();
    }
  });

  rows.forEach((row) => {
    const f = field(row);
    if (!f) return;
    f.addEventListener('keydown', (e) => {
      if (e.key !== 'Enter' || e.isComposing) return;
      e.preventDefault();
      if (e.shiftKey) done(row, 'opacity-50');
      else apply(row);
      focusNext(row);
    });
  });
  const first = rows.map(field).find((f) => f && !f.disabled);
  if (first) first.focus();
})();
