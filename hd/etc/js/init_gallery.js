function init_gallery() {
  const title   = document.getElementById('ig_page_title').value.trim();
  const folder  = document.getElementById('ig_folder').value.trim();
  const notes   = document.getElementById('notes_comments');
  const modal   = document.getElementById('init_gallery');
  const hide    = () => bootstrap.Modal.getInstance(modal)?.hide();
  const fnameEl = document.getElementById('ig_fname');
  const fname   = fnameEl ? fnameEl.value.trim() : '';
  // ig_img holds the actual image path (set when coming from carrousel)
  const imgEl   = document.getElementById('ig_img');
  const img     = imgEl ? imgEl.value.trim() : '';

  const write = (t, images) => {
    const data = { title: t, images };
    notes.value = 'TITLE=' + t + '\nTYPE=gallery\n'
      + JSON.stringify(data, null, 2);
    hide();
  };

  if (img) {
    // Single specific image passed from carrousel
    const fullPath = folder ? folder + '/' + img : img;
    write(title || img.replace(/\.[^.]+$/, ''),
      [{ img: fullPath, desc: '', map: [], groups: [] }]);
  } else if (fname) {
    // Plain single image filename typed manually
    write(title || fname.replace(/\.[^.]+$/, ''),
      [{ img: fname, desc: '', map: [], groups: [] }]);
  } else if (folder) {
    // All images from folder
    fetch(GW.prefix + 'm=FOLDER_IMAGES&folder=' + encodeURIComponent(folder))
      .then(r => { if (!r.ok) throw new Error('HTTP ' + r.status); return r.json(); })
      .then(files => {
        files = Array.isArray(files) ? files : (files.images || []);
        write(title || folder,
          files.map(f => ({ img: f, desc: '', map: [], groups: [] })));
      })
      .catch(() => alert('[*loading/error]1'));
  }
}

document.addEventListener('DOMContentLoaded', () => {
  const fname  = document.getElementById('ig_fname');
  const folder = document.getElementById('ig_folder');
  const modal  = document.getElementById('init_gallery');

  fname?.addEventListener('input', function () {
    if (folder) folder.disabled = this.value.trim() !== '';
  });
  folder?.addEventListener('input', function () {
    if (fname) fname.disabled = this.value.trim() !== '';
  });
  modal?.addEventListener('show.bs.modal', () => {
    ['ig_fname', 'ig_folder', 'ig_page_title', 'ig_img'].forEach(id => {
      const el = document.getElementById(id);
      if (el) { el.value = ''; el.disabled = false; }
    });
    // Reset carrousel-specific UI
    document.getElementById('ig_img_row')?.classList.add('d-none');
    ['ig_fname_row', 'ig_or_row', 'ig_folder_row'].forEach(id =>
      document.getElementById(id)?.classList.remove('d-none')
    );
  });
  document.getElementById('ig_ok_btn')
    ?.addEventListener('click', init_gallery);

  // Auto-open the modal when coming from carrousel (?folder= or ?img= present)
  const urlParams = new URLSearchParams(window.location.search);
  const urlImg    = urlParams.get('img');
  const urlFolder = urlParams.get('folder');

  if ((urlImg || urlFolder) && modal) {
    modal.addEventListener('shown.bs.modal', () => {
      const imgEl    = document.getElementById('ig_img');
      const folderEl = document.getElementById('ig_folder');
      if (imgEl)    imgEl.value    = urlImg    || '';
      if (folderEl) { folderEl.value = urlFolder || ''; folderEl.disabled = !!urlFolder; }

      if (urlImg) {
        const imgRow  = document.getElementById('ig_img_row');
        const imgHint = document.getElementById('ig_img_hint');
        if (imgRow)  imgRow.classList.remove('d-none');
        if (imgHint) imgHint.textContent = (urlFolder ? urlFolder + '/' : '') + urlImg;

        ['ig_fname_row', 'ig_or_row', 'ig_folder_row'].forEach(id =>
          document.getElementById(id)?.classList.add('d-none')
        );

        const fnameEl = document.getElementById('ig_fname');
        if (fnameEl) {
          fnameEl.value = urlImg.replace(/\.[^.]+$/, '')
                                .replace(/[^a-zA-Z0-9_-]/g, '_');
        }
      }
    }, { once: true });

    bootstrap.Modal.getOrCreateInstance(modal).show();
  }
});