/**
 * dagSvg.js — GeneWeb DAG table: overlay clean SVG connectors
 *
 * Content cells: td.dag-cell that is NOT dag-bar (TDitem person cells).
 * y endpoints meet at branch CENTER (same Y as the drawn hr line).
 * cx = centre of the content td.
 */

(function () {
  'use strict';

  document.addEventListener('DOMContentLoaded', init);

  function init() {
    const table = document.getElementById('dag');
    if (!table) return;

    table.querySelectorAll('td.dag-collapse').forEach(function (td) {
      td.style.visibility = 'hidden';
      td.style.padding    = '0';
    });
    table.querySelectorAll('td.dag-bar').forEach(function (td) {
      td.style.visibility = 'hidden';
    });
    table.querySelectorAll('td hr').forEach(function (hr) {
      hr.style.visibility = 'hidden';
    });

    const parent = table.parentElement;
    if (window.getComputedStyle(parent).position === 'static') {
      parent.style.position = 'relative';
    }
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.id = 'dag-svg-overlay';
    svg.style.cssText = 'position:absolute;top:0;left:0;width:100%;height:100%;' +
                        'overflow:visible;pointer-events:none;z-index:5';
    parent.appendChild(svg);

    requestAnimationFrame(function () { drawConnectors(table, svg); });
  }

  /* ── Row classification ───────────────────────────────────────── */

  function isContentTd(td) {
    return td.classList.contains('dag-cell') && !td.classList.contains('dag-bar');
  }

  function classifyRow(tr) {
    let hasBar = false, hasBranch = false, hasContent = false, hasHr = false;
    for (const td of tr.cells) {
      if (td.classList.contains('dag-bar')) { hasBar = true; continue; }
      const hr = td.querySelector('hr');
      if (hr) {
        hasHr = true;
        if (hr.className === 'right' || hr.className === 'left') hasBranch = true;
        continue;
      }
      if (isContentTd(td)) hasContent = true;
    }
    if (hasContent) return 'content';
    if (hasBar)     return 'bar';
    if (hasBranch)  return 'branch';
    if (hasHr)      return 'sibling';
    return 'empty';
  }

  /* ── Column helpers ───────────────────────────────────────────── */

  function buildColStarts(tr) {
    const map = new Map();
    let col = 0;
    for (const td of tr.cells) {
      map.set(td, col);
      col += parseInt(td.getAttribute('colspan') || '1', 10);
    }
    return map;
  }

  /* ── Tight content rect for a td ─────────────────────────────── */
  /*
   * The td may be stretched taller than its rendered content by siblings.
   * Before %dag_cell.item; the template may emit:
   *   - nothing  (line_nbr=0 or index="-1")
   *   - <a href=... title=...></a>                  (RLM / dag / em=R branch)
   *   - <div class="position-relative"><a ...></a></div>  (D/A branch)
   * These navigation elements are zero- or near-zero-height.
   * %dag_cell.item; itself emits:
   *   [div.dag-img-slot | div.text-center]   (portrait, may be absent)
   *   span.text-nowrap                        (name + dates, always present)
   *   [<br> + span.text-nowrap]*              (inline spouses)
   *
   * Strategy: iterate all descendants, collect rects of IMG and SPAN
   * elements (the actual rendered atoms), take their union.
   * Fallback to full td rect if nothing found.
   */
  function contentRect(td, toSVG) {
    const tdR = toSVG(td.getBoundingClientRect());
    let top = Infinity, bottom = -Infinity;

    /* Walk every element inside the td */
    const walker = document.createTreeWalker(td, NodeFilter.SHOW_ELEMENT);
    let node = walker.nextNode();
    while (node) {
      const tag = node.tagName;
      /* Only measure leaf content nodes — images and inline spans */
      if (tag === 'IMG' || tag === 'SPAN') {
        const r = toSVG(node.getBoundingClientRect());
        if (r.h >= 1) {
          if (r.y        < top)    top    = r.y;
          if (r.y + r.h  > bottom) bottom = r.y + r.h;
        }
      }
      node = walker.nextNode();
    }

    if (!isFinite(top) || bottom <= top) {
      top    = tdR.y;
      bottom = tdR.y + tdR.h;
    }
    return { top, bottom };
  }

  /* ── Content index ────────────────────────────────────────────── */

  function buildContentIndex(rows, toSVG) {
    const idx = {};
    rows.forEach(function (tr, ri) {
      if (classifyRow(tr) !== 'content') return;
      const colMap = buildColStarts(tr);
      idx[ri] = [];
      for (const td of tr.cells) {
        if (!isContentTd(td)) continue;
        const tdR = toSVG(td.getBoundingClientRect());
        const cs  = colMap.get(td);
        const ce  = cs + parseInt(td.getAttribute('colspan') || '1', 10);
        const cr  = contentRect(td, toSVG);
        idx[ri].push({
          cs, ce,
          cx:            tdR.cx,
          contentTop:    cr.top,
          contentBottom: cr.bottom,
        });
      }
    });
    return idx;
  }

  function findOverlap(idx, ri, cs, ce) {
    const entries = idx[ri];
    if (!entries) return null;
    for (const e of entries) {
      if (e.cs < ce && e.ce > cs) return e;
    }
    return null;
  }

  /* ── Branch index ─────────────────────────────────────────────── */

  function buildBranchIndex(rows, toSVG) {
    const idx = {};
    rows.forEach(function (tr, ri) {
      const rtype = classifyRow(tr);
      if (rtype !== 'branch' && rtype !== 'sibling') return;
      let rowTop = null, rowBottom = null, rowY = null;
      for (const td of tr.cells) {
        const hr = td.querySelector('hr');
        if (!hr) continue;
        const r = toSVG(td.getBoundingClientRect());
        if (rowY === null) {
          rowY      = r.cy;
          rowTop    = r.y;
          rowBottom = r.y + r.h;
        }
      }
      idx[ri] = { y: rowY, top: rowTop, bottom: rowBottom };
    });
    return idx;
  }

  /* ── Main draw ────────────────────────────────────────────────── */

  function drawConnectors(table, svg) {
    const pRect = svg.parentElement.getBoundingClientRect();
    function toSVG(r) {
      return {
        x: r.left - pRect.left, y: r.top - pRect.top,
        w: r.width, h: r.height,
        cx: r.left - pRect.left + r.width  / 2,
        cy: r.top  - pRect.top  + r.height / 2,
      };
    }

    const rows       = Array.from(table.rows);
    const nRows      = rows.length;
    const rowType    = rows.map(classifyRow);
    const contentIdx = buildContentIndex(rows, toSVG);
    const branchIdx  = buildBranchIndex(rows, toSVG);

    const STROKE = 'var(--color-border-secondary, #999)';

    function nearestSig(ri, dir) {
      for (let r = ri + dir; r >= 0 && r < nRows; r += dir) {
        const t = rowType[r];
        if (t === 'content' || t === 'branch' || t === 'sibling') {
          return { ri: r, type: t };
        }
      }
      return null;
    }

    /* ── Vertical bars ──────────────────────────────────────────── */
    rows.forEach(function (tr, ri) {
      if (rowType[ri] !== 'bar') return;
      const colMap = buildColStarts(tr);

      for (const td of tr.cells) {
        if (!td.classList.contains('dag-bar')) continue;
        const barR = toSVG(td.getBoundingClientRect());
        if (barR.w < 1) continue;

        const cs = colMap.get(td);
        const ce = cs + parseInt(td.getAttribute('colspan') || '1', 10);

        const sigAbove = nearestSig(ri, -1);
        const sigBelow = nearestSig(ri, +1);

        const aboveIsBranch = sigAbove &&
          (sigAbove.type === 'branch' || sigAbove.type === 'sibling');
        const belowIsBranch = sigBelow &&
          (sigBelow.type === 'branch' || sigBelow.type === 'sibling');

        /* ── y1 ── */
        let y1 = barR.y;
        if (aboveIsBranch) {
          const b = branchIdx[sigAbove.ri];
          if (b && b.y !== null) y1 = b.y;
        } else if (sigAbove && sigAbove.type === 'content') {
          const e = findOverlap(contentIdx, sigAbove.ri, cs, ce);
          if (e) y1 = e.contentBottom;
        }

        /* ── y2 ── */
        let y2 = barR.y + barR.h;
        if (belowIsBranch) {
          const b = branchIdx[sigBelow.ri];
          if (b && b.y !== null) y2 = b.y;
        } else if (sigBelow && sigBelow.type === 'content') {
          const e = findOverlap(contentIdx, sigBelow.ri, cs, ce);
          if (e) y2 = e.contentTop;
        }

        /* ── cx ── */
        let cx = barR.cx;
        if (aboveIsBranch) {
          if (sigBelow && sigBelow.type === 'content') {
            const e = findOverlap(contentIdx, sigBelow.ri, cs, ce);
            if (e) cx = e.cx;
          }
        } else if (sigAbove && sigAbove.type === 'content') {
          const e = findOverlap(contentIdx, sigAbove.ri, cs, ce);
          if (e) cx = e.cx;
        }

        if (y2 > y1 + 1) {
          svg.appendChild(makeLine(cx, y1, cx, y2, STROKE, '2', false));
        }
      }
    });

    /* ── Horizontal connectors ──────────────────────────────────── */
    rows.forEach(function (tr, ri) {
      const rtype = rowType[ri];
      if (rtype !== 'branch' && rtype !== 'sibling') return;

      const colMap  = buildColStarts(tr);
      const bInfo   = branchIdx[ri];
      const midY    = bInfo && bInfo.y !== null ? bInfo.y : null;

      const hrCells = [];
      for (const td of tr.cells) {
        const hr = td.querySelector('hr');
        if (!hr) continue;
        const cs = colMap.get(td);
        const ce = cs + parseInt(td.getAttribute('colspan') || '1', 10);
        hrCells.push({ td, cls: hr.className, cs, ce });
      }
      hrCells.sort(function (a, b) { return a.cs - b.cs; });

      const runs = [];
      let cur = [];
      hrCells.forEach(function (c, i) {
        if (i > 0 && c.cs !== hrCells[i-1].ce) { runs.push(cur); cur = []; }
        cur.push(c);
      });
      if (cur.length) runs.push(cur);

      runs.forEach(function (run) {
        const dashed = !run.some(function (c) {
          return c.cls === 'right' || c.cls === 'left';
        });
        run.forEach(function (c) {
          const r = toSVG(c.td.getBoundingClientRect());
          if (r.w < 1) return;
          const y = midY !== null ? midY : r.cy;
          if (c.cls === 'full') {
            svg.appendChild(makeLine(r.x, y, r.x + r.w, y, STROKE, '2', dashed));
          } else if (c.cls === 'right') {
            svg.appendChild(makeLine(r.cx, y, r.x + r.w, y, STROKE, '2', false));
          } else if (c.cls === 'left') {
            svg.appendChild(makeLine(r.x, y, r.cx, y, STROKE, '2', false));
          }
        });
      });
    });
  }

  function makeLine(x1, y1, x2, y2, stroke, sw, dashed) {
    const l = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    l.setAttribute('x1', x1); l.setAttribute('y1', y1);
    l.setAttribute('x2', x2); l.setAttribute('y2', y2);
    l.setAttribute('stroke', stroke);
    l.setAttribute('stroke-width', sw);
    l.setAttribute('stroke-linecap', 'round');
    if (dashed) l.setAttribute('stroke-dasharray', '4 3');
    return l;
  }

})();
