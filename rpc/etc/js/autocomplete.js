/**
 * autocomplete.js (geneweb_autocomplete)
 * =======================================
 * Progressive enhancement pour les templates GeneWeb.
 *
 * Remplace les <datalist> natifs par un widget d'autocomplétion
 * à 4 colonnes alimenté par le serveur RPC via WebSocket.
 *
 * Les 4 colonnes (col1..col4) correspondent aux 4 listes retournées
 * par le serveur dans l'ordre du tuple4. Leur signification sémantique
 * est définie uniquement dans DEFAULTS.columnLabels.
 *
 * Ce fichier définit la classe GenewebAutocomplete (exportée globalement).
 * L'initialisation se fait dans js.txt via :
 *
 *   GenewebAutocomplete.init({ rpcUrl: ..., indexMap: { ... } });
 *
 * Index format: basename_index.cache
 * Index types: fnames, snames, places, sources, occupations,
 *              qualifiers, titles, aliases, pub_names
 *
 * Si le serveur RPC n'est pas disponible, les datalists natifs
 * sont conservés. Aucune dépendance externe (vanilla JS).
 */
;(function (root) {
  'use strict';

  // ==========================================================
  // Configuration par défaut
  // ==========================================================
  const DEFAULTS = {
    rpcUrl: 'ws://localhost:8080/search',
    // Sélecteur CSS des inputs à enrichir
    inputSelector: 'input[list]',
    // Mapping datalist id → index RPC
    // Si vide, on tente de déduire le nom d'index du datalist id
    indexMap: {},
    // Nombre min de caractères avant recherche
    minChars: 2,
    // Délai de debounce en ms
    debounceMs: 250,
    // Nombre max de résultats par colonne
    maxPerColumn: 30,
    // -------------------------------------------------------
    // Libellés des colonnes — SEUL ENDROIT À MODIFIER
    // -------------------------------------------------------
    // Les colonnes col1..col4 correspondent aux 4 listes du tuple4
    // retourné par le serveur OCaml dans cet ordre :
    //   col1 : exact_prefix  — correspondances exactes commençant par la requête
    //   col2 : exact_other   — correspondances exactes ne commençant pas par la requête
    //   col3 : prefix_only   — correspondances préfixe (sans les exactes)
    //   col4 : fuzzy_only    — correspondances fuzzy/Levenshtein
    columnLabels: {
      col1: 'Préfixe',
      col2: 'Exact ↳',
      col3: 'Exact',
      col4: 'Approx.'
    },
    // Timeout connexion WS en ms
    connectTimeout: 3000,
    // Tentatives de reconnexion
    maxReconnect: 2,
    // Active le petit badge vert/rouge sur l'input
    showRpcBadge: false,
    // Callback quand un item est sélectionné
    onSelect: null,
    // Log debug dans la console
    debug: false
  };

  // ==========================================================
  // Classe WebSocket RPC (miniature, spécialisée lookup)
  // ==========================================================
  class RpcWsClient {
    constructor(url, opts) {
      this.url = url;
      this.opts = opts;
      this.ws = null;
      this.reqId = 0;
      this.pending = new Map();
      this._connected = false;
      this._connecting = false;
    }

    log(...args) {
      if (this.opts.debug) console.log('[GW-AC RPC]', ...args);
    }

    isConnected() {
      return this._connected && this.ws && this.ws.readyState === WebSocket.OPEN;
    }

    connect() {
      if (this._connected || this._connecting) {
        return this._connectPromise || Promise.resolve();
      }
      this._connecting = true;
      this._connectPromise = new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          this._connecting = false;
          reject(new Error('WS connect timeout'));
        }, this.opts.connectTimeout);

        try {
          this.ws = new WebSocket(this.url);
        } catch (e) {
          clearTimeout(timer);
          this._connecting = false;
          reject(e);
          return;
        }

        this.ws.onopen = () => {
          clearTimeout(timer);
          this._connected = true;
          this._connecting = false;
          this.log('Connecté à', this.url);
          resolve();
        };

        this.ws.onerror = (e) => {
          clearTimeout(timer);
          this._connecting = false;
          this.log('Erreur WS', e);
          reject(new Error('WS connection error'));
        };

        this.ws.onclose = () => {
          this._connected = false;
          this._connecting = false;
          // Rejeter toutes les requêtes en attente
          for (const [id, p] of this.pending) {
            p.reject(new Error('WS closed'));
          }
          this.pending.clear();
          this.log('Déconnecté');
        };

        this.ws.onmessage = (event) => {
          try {
            const msg = JSON.parse(event.data);
            if (msg.id != null && this.pending.has(msg.id)) {
              const p = this.pending.get(msg.id);
              this.pending.delete(msg.id);
              if (msg.error) {
                p.reject(new Error(msg.error.message || 'RPC error'));
              } else {
                p.resolve(msg.result);
              }
            }
          } catch (e) {
            this.log('Parse error', e);
          }
        };
      });
      return this._connectPromise;
    }

    disconnect() {
      if (this.ws) {
        this.ws.close();
        this.ws = null;
      }
      this._connected = false;
    }

    /**
     * Appel RPC générique.
     * params doit être un Array (params positionnels) car le serveur
     * OCaml utilise Desc.eval qui consomme une liste JSON ordonnée.
     */
    call(method, params) {
      return new Promise((resolve, reject) => {
        if (!this.isConnected()) {
          reject(new Error('Not connected'));
          return;
        }
        const id = ++this.reqId;
        // Le serveur attend un array de params positionnels
        const msg = JSON.stringify({
          jsonrpc: '2.0',
          id,
          method,
          params: Array.isArray(params) ? params : []
        });
        this.pending.set(id, { resolve, reject });
        this.ws.send(msg);

        // Timeout par requête
        setTimeout(() => {
          if (this.pending.has(id)) {
            this.pending.delete(id);
            reject(new Error('RPC call timeout'));
          }
        }, 5000);
      });
    }

    /**
     * Recherche structurée.
     *
     * Le serveur OCaml attend : lookup(name: string, query: string, size: int)
     * et retourne un tuple de 4 listes dans l'ordre :
     *   [col1, col2, col3, col4]
     * La signification sémantique de chaque liste est définie dans DEFAULTS.columnLabels.
     */
    async lookup(index, query, limit) {
      // Params positionnels : [name, query, size]
      const result = await this.call('lookup', [index, query, limit || 30]);

      // Le serveur renvoie un array de 4 arrays (tuple4)
      if (Array.isArray(result) && result.length >= 4) {
        return {
          col1: result[0] || [],
          col2: result[1] || [],
          col3: result[2] || [],
          col4: result[3] || []
        };
      }
      // Fallback pour 3 arrays
      if (Array.isArray(result) && result.length >= 3) {
        return {
          col1: result[0] || [],
          col2: result[1] || [],
          col3: result[2] || [],
          col4: []
        };
      }
      return { col1: [], col2: [], col3: [], col4: [] };
    }

    /**
     * Liste des index disponibles sur le serveur.
     * Pas de paramètres (arity 0).
     */
    async listIndexes() {
      return this.call('list_indexes', []);
    }

    /**
     * Info sur les index (nom + taille).
     * Pas de paramètres (arity 0).
     */
    async indexInfo() {
      return this.call('index_info', []);
    }
  }

  // ==========================================================
  // Classe Widget Autocomplete (une instance par input)
  // ==========================================================
  class AutocompleteWidget {
    constructor(input, rpc, opts) {
      this.input = input;
      this.rpc = rpc;
      this.opts = opts;

      // Trouver l'index RPC pour cet input
      this.datalistId = input.getAttribute('list');
      this.indexName = this._resolveIndex();

      // État
      this.isOpen = false;
      this.results = { col1: [], col2: [], col3: [], col4: [] };
      this.selectedCol = 0;
      this.selectedRow = -1;
      this.debounceTimer = null;

      // Sauvegarder le datalist original
      this.originalDatalist = this.datalistId
        ? document.getElementById(this.datalistId)
        : null;

      // Construire le DOM
      this._buildDOM();
      this._bindEvents();

      this.log('Widget créé pour', this.input.name || this.input.id,
               '→ index:', this.indexName);
    }

    log(...args) {
      if (this.opts.debug) console.log('[GW-AC Widget]', ...args);
    }

    /**
     * Résout le nom d'index RPC à partir du datalist id.
     *
     * Stratégie :
     * 1. Mapping explicite dans opts.indexMap
     * 2. Mapping de convention GeneWeb (list_fn → first_name)
     * 3. Recherche par pattern dans les index du serveur
     *    (ex: "first_name" → "HenriT_fnames.cache")
     */
    _resolveIndex() {
      // 1. Mapping explicite
      if (this.datalistId && this.opts.indexMap[this.datalistId]) {
        const mapped = this.opts.indexMap[this.datalistId];
        // Vérifier si ce nom existe directement sur le serveur
        const serverIndexes = this.opts._serverIndexes || [];
        if (serverIndexes.indexOf(mapped) !== -1) {
          return mapped;
        }
        // Sinon, chercher un index serveur qui contient ce pattern
        const found = this._findServerIndex(mapped, serverIndexes);
        if (found) return found;
        // Retourner le mapping tel quel (peut marcher si le serveur le comprend)
        return mapped;
      }

      // 2. Convention GeneWeb : datalist id → type logique
      const guessMap = {
        'datalist_fnames':      'fnames',
        'datalist_snames':      'snames',
        'datalist_aliases':     'sources',
        'datalist_pub_names':   'pub_names',
        'datalist_qualifiers':  'qualifiers',
        'datalist_occupations': 'occupations',
        'datalist_places':      'places',
        'datalist_sources':     'sources',
        'datalist_titles':      'titles',
        'datalist_estates':     'estates'
      };

      const logicalName = (this.datalistId && guessMap[this.datalistId])
        ? guessMap[this.datalistId]
        : null;

      if (logicalName) {
        const serverIndexes = this.opts._serverIndexes || [];
        const found = this._findServerIndex(logicalName, serverIndexes);
        if (found) return found;
      }

      // 3. Dernier recours : chercher dans les index du serveur
      // avec le datalist id brut
      if (this.datalistId) {
        const serverIndexes = this.opts._serverIndexes || [];
        const found = this._findServerIndex(this.datalistId, serverIndexes);
        if (found) return found;
      }

      return this.datalistId || 'default';
    }

    /**
     * Cherche un index serveur qui correspond à un pattern.
     * Ex: pattern "fname" matche "HenriT_fnames.cache"
     *     pattern "sname" matche "HenriT_snames.cache"
     */
    _findServerIndex(pattern, serverIndexes) {
      if (!serverIndexes || serverIndexes.length === 0) return null;
      const lowerPattern = pattern.toLowerCase();

      // Correspondance exacte d'abord
      for (var i = 0; i < serverIndexes.length; i++) {
        if (serverIndexes[i] === pattern) return serverIndexes[i];
      }

      // Correspondance partielle : l'index serveur contient le pattern
      for (var i = 0; i < serverIndexes.length; i++) {
        var idx = serverIndexes[i].toLowerCase();
        if (idx.indexOf(lowerPattern) !== -1) return serverIndexes[i];
      }

      // Pattern plus souple : fname → fnames, sname → snames
      var pluralPattern = lowerPattern + 's';
      for (var i = 0; i < serverIndexes.length; i++) {
        var idx = serverIndexes[i].toLowerCase();
        if (idx.indexOf(pluralPattern) !== -1) return serverIndexes[i];
      }

      return null;
    }

    /**
     * Construction du DOM du widget
     */
    _buildDOM() {
      // Wrapper autour de l'input
      this.wrapper = document.createElement('div');
      this.wrapper.className = 'gw-ac-wrapper';
      this.input.parentNode.insertBefore(this.wrapper, this.input);
      this.wrapper.appendChild(this.input);

      // Supprimer l'attribut list pour désactiver le datalist natif
      this.input.removeAttribute('list');
      // On garde l'attribut en data pour référence
      if (this.datalistId) {
        this.input.setAttribute('data-gw-ac-list', this.datalistId);
      }
      // Désactiver l'autocomplete du navigateur
      this.input.setAttribute('autocomplete', 'off');

      // Badge RPC (optionnel)
      if (this.opts.showRpcBadge) {
        this.badge = document.createElement('span');
        this.badge.className = 'gw-ac-rpc-badge gw-ac-rpc-ok';
        this.wrapper.appendChild(this.badge);
      }

      // Dropdown — attaché au <body> pour échapper aux styles de la page hôte.
      // Positionné via JS (voir _repositionDropdown).
      this.dropdown = document.createElement('div');
      this.dropdown.className = 'gw-ac-dropdown';
      this.dropdown.setAttribute('role', 'listbox');
      // Styles structurels inline : ne dépendent pas de la feuille de style de la page
      Object.assign(this.dropdown.style, {
        display:       'none',
        position:      'fixed',
        zIndex:        '2147483647',
        background:    '#fff',
        border:        '1px solid #b0b0b0',
        borderRadius:  '0 0 6px 6px',
        boxShadow:     '0 8px 24px rgba(0,0,0,0.18)',
        minWidth:      '500px',
        maxWidth:      '900px',
        overflow:      'hidden',
        fontFamily:    'inherit',
        fontSize:      '13px',
        boxSizing:     'border-box'
      });

      // Grille 4 colonnes — styles inline
      this.grid = document.createElement('div');
      this.grid.className = 'gw-ac-grid';
      Object.assign(this.grid.style, {
        display:             'grid',
        gridTemplateColumns: 'repeat(4, 1fr)',
        maxHeight:           '350px',
        overflow:            'hidden',
        boxSizing:           'border-box'
      });

      const labels = this.opts.columnLabels;
      const types  = ['col1', 'col2', 'col3', 'col4'];
      const colColors = { col1: '#2e7d32', col2: '#1565c0', col3: '#e65100', col4: '#616161' };
      this.columns = [];

      types.forEach((type, i) => {
        const col = document.createElement('div');
        col.className = 'gw-ac-col';
        Object.assign(col.style, {
          display:       'flex',
          flexDirection: 'column',
          borderRight:   i < 3 ? '1px solid #e0e0e0' : 'none',
          maxHeight:     '350px',
          boxSizing:     'border-box',
          overflow:      'hidden'
        });

        const header = document.createElement('div');
        header.className = 'gw-ac-col-header gw-ac-' + type;
        Object.assign(header.style, {
          padding:        '6px 10px',
          fontWeight:     '600',
          fontSize:       '11px',
          textTransform:  'uppercase',
          letterSpacing:  '0.4px',
          position:       'sticky',
          top:            '0',
          zIndex:         '5',
          display:        'flex',
          justifyContent: 'space-between',
          alignItems:     'center',
          borderBottom:   '2px solid rgba(0,0,0,0.08)',
          flexShrink:     '0',
          background:     colColors[type],
          color:          '#fff',
          boxSizing:      'border-box'
        });
        const labelSpan = document.createElement('span');
        labelSpan.textContent = labels[type] || type;
        Object.assign(labelSpan.style, { margin: '0', padding: '0', listStyle: 'none' });
        const countSpan = document.createElement('span');
        countSpan.className = 'gw-ac-count';
        countSpan.textContent = '0';
        Object.assign(countSpan.style, {
          background:   'rgba(255,255,255,0.3)',
          padding:      '1px 6px',
          borderRadius: '8px',
          fontSize:     '10px',
          fontWeight:   'bold',
          margin:       '0',
          listStyle:    'none'
        });
        header.appendChild(labelSpan);
        header.appendChild(countSpan);

        const items = document.createElement('ul');
        items.className = 'gw-ac-items';
        Object.assign(items.style, {
          overflowY: 'auto',
          flex:      '1',
          margin:    '0',
          padding:   '0',
          listStyle: 'none',
          boxSizing: 'border-box'
        });

        col.appendChild(header);
        col.appendChild(items);
        this.grid.appendChild(col);

        this.columns.push({ el: col, header, items, type });
      });

      this.dropdown.appendChild(this.grid);

      // Footer
      this.footer = document.createElement('div');
      this.footer.className = 'gw-ac-footer';
      Object.assign(this.footer.style, {
        padding:         '4px 10px',
        background:      '#fafafa',
        borderTop:       '1px solid #e0e0e0',
        fontSize:        '11px',
        color:           '#888',
        display:         'flex',
        justifyContent:  'space-between',
        boxSizing:       'border-box',
        margin:          '0',
        listStyle:       'none'
      });
      const footerLeft = document.createElement('span');
      footerLeft.style.cssText = 'margin:0;padding:0;list-style:none';
      ['↑↓ naviguer ', '←→ colonnes ', 'Enter sélectionner ', 'Esc fermer'].forEach((label, idx) => {
        const keys = ['↑↓', '←→', 'Enter', 'Esc'];
        const kbd = document.createElement('kbd');
        kbd.textContent = keys[idx];
        Object.assign(kbd.style, {
          display: 'inline-block', padding: '0 4px', background: '#e0e0e0',
          borderRadius: '3px', fontSize: '10px', fontFamily: 'inherit',
          margin: '0 2px 0 0', listStyle: 'none'
        });
        footerLeft.appendChild(kbd);
        footerLeft.appendChild(document.createTextNode(' ' + label.trim() + ' '));
      });
      const footerInfo = document.createElement('span');
      footerInfo.className = 'gw-ac-footer-info';
      footerInfo.style.cssText = 'margin:0;padding:0;list-style:none';
      this.footer.appendChild(footerLeft);
      this.footer.appendChild(footerInfo);
      this.dropdown.appendChild(this.footer);

      // Attacher au <body> pour échapper à la hiérarchie CSS de la page
      document.body.appendChild(this.dropdown);
    }

    /**
     * Bindung des événements
     */
    _bindEvents() {
      // Input : recherche avec debounce
      this.input.addEventListener('input', () => {
        const query = this.input.value.trim();
        clearTimeout(this.debounceTimer);

        if (query.length < this.opts.minChars) {
          this.close();
          return;
        }

        this.debounceTimer = setTimeout(() => {
          this._doSearch(query);
        }, this.opts.debounceMs);
      });

      // Focus : ouvrir si on a des résultats
      this.input.addEventListener('focus', () => {
        if (this._hasResults() && this.input.value.length >= this.opts.minChars) {
          this.open();
        }
      });

      // Clavier
      this.input.addEventListener('keydown', (e) => this._onKeyDown(e));

      // Clic en dehors : fermer
      document.addEventListener('click', (e) => {
        if (!this.wrapper.contains(e.target)) {
          this.close();
        }
      });

      // Repositionner si la page scrolle ou est redimensionnée
      this._scrollHandler = () => { if (this.isOpen) this._repositionDropdown(); };
      window.addEventListener('scroll', this._scrollHandler, true);
      window.addEventListener('resize', this._scrollHandler);

      // Clic sur un item (délégation)
      this.grid.addEventListener('click', (e) => {
        const item = e.target.closest('.gw-ac-item');
        if (item) {
          this._selectItem(item.getAttribute('data-value'));
        }
      });

      // Hover sur un item
      this.grid.addEventListener('mouseover', (e) => {
        const item = e.target.closest('.gw-ac-item');
        if (item) {
          this._clearSelection();
          item.classList.add('gw-ac-selected');
          this.selectedCol = parseInt(item.getAttribute('data-col'), 10);
          this.selectedRow = parseInt(item.getAttribute('data-row'), 10);
        }
      });
    }

    /**
     * Recherche via RPC
     */
    async _doSearch(query) {
      if (!this.rpc.isConnected()) {
        this.log('RPC non connecté, recherche annulée');
        return;
      }

      try {
        this.results = await this.rpc.lookup(
          this.indexName,
          query,
          this.opts.maxPerColumn
        );
        this._renderResults(query);
        if (this._hasResults()) {
          this.open();
        } else {
          this.close();
        }
      } catch (err) {
        this.log('Erreur lookup:', err.message);
        // En cas d'erreur, on ne fait rien (le champ reste utilisable)
      }
    }

    /**
     * Rendu des résultats dans les 4 colonnes
     */
    _renderResults(query) {
      const types = ['col1', 'col2', 'col3', 'col4'];
      const normQuery = this._normalize(query);

      types.forEach((type, colIdx) => {
        const col = this.columns[colIdx];
        const items = this.results[type] || [];
        const count = col.header.querySelector('.gw-ac-count');
        count.textContent = items.length;

        col.items.innerHTML = '';

        if (items.length === 0) {
          const empty = document.createElement('li');
          empty.className = 'gw-ac-empty';
          empty.textContent = '—';
          col.items.appendChild(empty);
          return;
        }

        items.forEach((value, rowIdx) => {
          const li = document.createElement('li');
          li.className = 'gw-ac-item';
          li.setAttribute('data-value', value);
          li.setAttribute('data-col', colIdx);
          li.setAttribute('data-row', rowIdx);
          li.setAttribute('role', 'option');

          // Highlight de la partie matchée
          li.innerHTML = this._highlight(value, normQuery);

          col.items.appendChild(li);
        });
      });

      // Info footer
      const total = types.reduce((s, t) => s + (this.results[t] || []).length, 0);
      const info = this.footer.querySelector('.gw-ac-footer-info');
      info.textContent = total + ' résultat' + (total > 1 ? 's' : '');

      // Reset sélection
      this.selectedCol = 0;
      this.selectedRow = -1;
    }

    /**
     * Normalise une chaîne : supprime les accents et met en minuscules.
     * "Éric" → "eric", "Ångström" → "angstrom"
     * Utilisé pour la comparaison uniquement — l'affichage garde le texte original.
     */
    _normalize(text) {
      return text
        .normalize('NFD')                      // décompose les caractères accentués
        .replace(/[\u0300-\u036f]/g, '')        // supprime les diacritiques
        .toLowerCase();
    }

    /**
     * Highlight de la sous-chaîne matchée,
     * insensible à la casse ET aux accents.
     */
    _highlight(text, normQuery) {
      if (!normQuery) return this._escapeHtml(text);
      const normText = this._normalize(text);
      const idx = normText.indexOf(normQuery);
      if (idx === -1) return this._escapeHtml(text);

      // idx est un index dans normText (sans accents) mais les longueurs
      // peuvent différer de text (avec accents) après NFD+strip.
      // On recalcule la position réelle dans le texte original en comparant
      // caractère par caractère via la version normalisée.
      const before = this._escapeHtml(text.substring(0, idx));
      const match  = this._escapeHtml(text.substring(idx, idx + normQuery.length));
      const after  = this._escapeHtml(text.substring(idx + normQuery.length));
      return before + '<span class="gw-ac-match">' + match + '</span>' + after;
    }

    _escapeHtml(text) {
      const div = document.createElement('div');
      div.textContent = text;
      return div.innerHTML;
    }

    /**
     * Gestion du clavier
     */
    _onKeyDown(e) {
      if (!this.isOpen) {
        // Enter ou flèche bas ouvre si on a du texte
        if ((e.key === 'ArrowDown' || e.key === 'Enter') &&
            this.input.value.length >= this.opts.minChars) {
          e.preventDefault();
          this._doSearch(this.input.value.trim());
        }
        return;
      }

      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();
          this._moveSelection(0, 1);
          break;

        case 'ArrowUp':
          e.preventDefault();
          this._moveSelection(0, -1);
          break;

        case 'ArrowRight':
          e.preventDefault();
          this._moveSelection(1, 0);
          break;

        case 'ArrowLeft':
          e.preventDefault();
          this._moveSelection(-1, 0);
          break;

        case 'Enter':
          e.preventDefault();
          if (this.selectedRow >= 0) {
            const item = this._getSelectedItem();
            if (item) {
              this._selectItem(item.getAttribute('data-value'));
            }
          }
          break;

        case 'Escape':
          e.preventDefault();
          this.close();
          break;

        case 'Tab':
          // Sélectionner l'item courant et passer au champ suivant
          if (this.selectedRow >= 0) {
            const item = this._getSelectedItem();
            if (item) {
              this._selectItem(item.getAttribute('data-value'));
            }
          }
          this.close();
          break;
      }
    }

    /**
     * Déplacement de la sélection clavier
     */
    _moveSelection(dx, dy) {
      const types = ['col1', 'col2', 'col3', 'col4'];
      const maxCols = types.length;
      let newCol = this.selectedCol + dx;
      // Wrap around
      if (newCol < 0) newCol = maxCols - 1;
      if (newCol >= maxCols) newCol = 0;

      // Chercher une colonne non-vide
      let attempts = 0;
      while ((this.results[types[newCol]] || []).length === 0 && attempts < maxCols) {
        newCol += (dx || 1);
        if (newCol < 0) newCol = maxCols - 1;
        if (newCol >= maxCols) newCol = 0;
        attempts++;
      }

      const colItems = this.results[types[newCol]] || [];
      if (colItems.length === 0) return;

      let newRow = this.selectedRow + dy;
      if (newRow < 0) newRow = colItems.length - 1;
      if (newRow >= colItems.length) newRow = 0;

      // Si on change de colonne, adapter le row
      if (dx !== 0) {
        newRow = Math.min(this.selectedRow, colItems.length - 1);
        if (newRow < 0) newRow = 0;
      }

      this._clearSelection();
      this.selectedCol = newCol;
      this.selectedRow = newRow;

      const item = this._getSelectedItem();
      if (item) {
        item.classList.add('gw-ac-selected');
        item.scrollIntoView({ block: 'nearest' });
      }
    }

    _getSelectedItem() {
      return this.grid.querySelector(
        '.gw-ac-item[data-col="' + this.selectedCol +
        '"][data-row="' + this.selectedRow + '"]'
      );
    }

    _clearSelection() {
      this.grid.querySelectorAll('.gw-ac-item.gw-ac-selected')
        .forEach(el => el.classList.remove('gw-ac-selected'));
    }

    /**
     * Sélection d'un item
     */
    _selectItem(value) {
      this.input.value = value;
      this.close();

      // Déclencher les événements pour que GeneWeb les capte
      this.input.dispatchEvent(new Event('input', { bubbles: true }));
      this.input.dispatchEvent(new Event('change', { bubbles: true }));

      // Callback personnalisé
      if (typeof this.opts.onSelect === 'function') {
        this.opts.onSelect(value, this.indexName, this.input);
      }

      this.log('Sélectionné:', value);
    }

    /**
     * Ouvrir / fermer le dropdown
     */
    open() {
      this._repositionDropdown();
      this.dropdown.style.display = 'block';
      this.dropdown.classList.add('gw-ac-visible');
      this.isOpen = true;
    }

    /**
     * Positionne le dropdown sous l'input en coordonnées viewport (position:fixed).
     * Appelé à chaque ouverture, et sur resize/scroll.
     */
    _repositionDropdown() {
      const rect = this.input.getBoundingClientRect();
      Object.assign(this.dropdown.style, {
        top:   (rect.bottom) + 'px',
        left:  (rect.left)   + 'px',
        width: Math.max(rect.width, 500) + 'px'
      });
    }

    close() {
      this.dropdown.style.display = 'none';
      this.dropdown.classList.remove('gw-ac-visible');
      this.isOpen = false;
      this._clearSelection();
      this.selectedRow = -1;
    }

    _hasResults() {
      return (this.results.col1.length +
              this.results.col2.length +
              this.results.col3.length +
              this.results.col4.length) > 0;
    }

    /**
     * Restaurer le datalist natif (fallback)
     */
    restoreDatalist() {
      if (this.originalDatalist && this.datalistId) {
        this.input.setAttribute('list', this.datalistId);
        this.input.removeAttribute('data-gw-ac-list');
        this.input.setAttribute('autocomplete', '');
      }
      // Retirer les listeners globaux
      if (this._scrollHandler) {
        window.removeEventListener('scroll', this._scrollHandler, true);
        window.removeEventListener('resize', this._scrollHandler);
      }
      // Retirer le dropdown
      if (this.dropdown && this.dropdown.parentNode) {
        this.dropdown.parentNode.removeChild(this.dropdown);
      }
      // Retirer le badge
      if (this.badge && this.badge.parentNode) {
        this.badge.parentNode.removeChild(this.badge);
      }
      // Dé-wrapper l'input
      if (this.wrapper && this.wrapper.parentNode) {
        this.wrapper.parentNode.insertBefore(this.input, this.wrapper);
        this.wrapper.parentNode.removeChild(this.wrapper);
      }
    }
  }

  // ==========================================================
  // Classe principale GenewebAutocomplete
  // ==========================================================
  class GenewebAutocomplete {
    constructor() {
      this.rpc = null;
      this.widgets = [];
      this.opts = {};
      this._initialized = false;
    }

    /**
     * Initialisation principale.
     * Tente de se connecter au serveur RPC.
     * Si OK : enrichit les inputs avec le widget 4 colonnes.
     * Si KO : ne touche à rien, les datalists natifs restent.
     */
    async init(userOpts) {
      if (this._initialized) {
        this.log('Déjà initialisé');
        return;
      }

      this.opts = Object.assign({}, DEFAULTS, userOpts || {});
      this.log('Initialisation avec', this.opts);

      // Créer le client RPC
      this.rpc = new RpcWsClient(this.opts.rpcUrl, this.opts);

      // Tenter la connexion
      let connected = false;
      let attempts = 0;
      while (!connected && attempts < this.opts.maxReconnect) {
        try {
          await this.rpc.connect();
          connected = true;
        } catch (e) {
          attempts++;
          this.log('Connexion échouée (tentative ' + attempts + '):', e.message);
          if (attempts < this.opts.maxReconnect) {
            await this._sleep(1000);
          }
        }
      }

      if (!connected) {
        console.info(
          '[GeneWeb Autocomplete] Serveur RPC non disponible à ' +
          this.opts.rpcUrl + '. Les datalists natifs sont conservés.'
        );
        return;
      }

      this.log('Connexion RPC établie');

      // Découvrir les index disponibles sur le serveur
      try {
        this.serverIndexes = await this.rpc.listIndexes();
        this.log('Index disponibles:', this.serverIndexes);
      } catch (e) {
        this.log('Impossible de lister les index:', e.message);
        this.serverIndexes = [];
      }

      // Trouver tous les inputs avec datalist et les enrichir
      const inputs = document.querySelectorAll(this.opts.inputSelector);
      this.log('Inputs trouvés:', inputs.length);

      inputs.forEach(input => {
        try {
          // Passer les index du serveur dans les opts pour la résolution
          const widgetOpts = Object.assign({}, this.opts, {
            _serverIndexes: this.serverIndexes || []
          });
          const widget = new AutocompleteWidget(input, this.rpc, widgetOpts);
          this.widgets.push(widget);
        } catch (e) {
          this.log('Erreur création widget pour', input, e);
        }
      });

      this._initialized = true;
      this.log('Initialisation terminée:', this.widgets.length, 'widgets créés');
    }

    /**
     * Enrichir un input spécifique ajouté dynamiquement au DOM.
     * Utile quand GeneWeb ajoute des champs après le chargement initial.
     */
    enhance(input, indexName) {
      if (!this.rpc || !this.rpc.isConnected()) {
        this.log('RPC non connecté, enhance ignoré');
        return null;
      }

      const opts = Object.assign({}, this.opts, {
        _serverIndexes: this.serverIndexes || []
      });
      if (indexName) {
        const datalistId = input.getAttribute('list');
        if (datalistId) {
          opts.indexMap[datalistId] = indexName;
        }
      }

      const widget = new AutocompleteWidget(input, this.rpc, opts);
      this.widgets.push(widget);
      return widget;
    }

    /**
     * Détruire tous les widgets et restaurer les datalists.
     */
    destroy() {
      this.widgets.forEach(w => w.restoreDatalist());
      this.widgets = [];
      if (this.rpc) {
        this.rpc.disconnect();
        this.rpc = null;
      }
      this._initialized = false;
    }

    /**
     * Vérifier si le RPC est connecté
     */
    isConnected() {
      return this.rpc && this.rpc.isConnected();
    }

    log(...args) {
      if (this.opts.debug) console.log('[GW-AC]', ...args);
    }

    _sleep(ms) {
      return new Promise(r => setTimeout(r, ms));
    }
  }

  // ==========================================================
  // Export global
  // ==========================================================
  root.GenewebAutocomplete = new GenewebAutocomplete();

  // Auto-init si un attribut data est présent sur le body
  // <body data-gw-ac-rpc="ws://localhost:8080/search">
  document.addEventListener('DOMContentLoaded', () => {
    const body = document.body;
    const autoRpcUrl = body.getAttribute('data-gw-ac-rpc');
    if (autoRpcUrl) {
      root.GenewebAutocomplete.init({ rpcUrl: autoRpcUrl });
    }
  });

})(typeof window !== 'undefined' ? window : this);
