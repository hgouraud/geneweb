/* rlm_spouse_arcs.js
 * Draws SVG arcs below the RLM dag table connecting leaf-level spouse
 * pairs that have no hr-bar in the table (because neither is a parent
 * of the other in the displayed graph).
 *
 * Requires: rlmSpousePairs to be defined by the inline <script> block
 * emitted by RelationDisplay.print_rlm_spouse_pairs_script.
 *
 * Each entry in rlmSpousePairs is [iper1, iper2, year_or_null].
 * Person cells are identified by id="i{iper}" in the dag table.
 */
document.addEventListener('DOMContentLoaded', function () {
  var dag = document.getElementById('dag');
  if (!dag) return;
  if (!window.rlmSpousePairs || rlmSpousePairs.length === 0) return;

  /* Create an SVG overlay absolutely positioned over the dag table.
   * Extra height accommodates arcs that curve below the last row. */
  var dagRect = dag.getBoundingClientRect();
  var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.style.position      = 'absolute';
  svg.style.top           = (dagRect.top  + window.scrollY) + 'px';
  svg.style.left          = (dagRect.left + window.scrollX) + 'px';
  svg.style.width         = dagRect.width  + 'px';
  svg.style.height        = (dagRect.height + 80) + 'px';
  svg.style.pointerEvents = 'none';
  svg.style.overflow      = 'visible';
  svg.style.zIndex        = '10';
  document.body.appendChild(svg);

  rlmSpousePairs.forEach(function (triple) {
    var i1   = triple[0];
    var i2   = triple[1];
    var year = triple[2];   /* integer or null */

    var el1 = document.getElementById('i' + i1);
    var el2 = document.getElementById('i' + i2);
    if (!el1 || !el2) return;

    var r1 = el1.getBoundingClientRect();
    var r2 = el2.getBoundingClientRect();

    /* Horizontal centres of each person cell, relative to dag origin */
    var x1 = r1.left + r1.width  / 2 - dagRect.left;
    var x2 = r2.left + r2.width  / 2 - dagRect.left;

    /* y just below the bottom of the lower of the two cells */
    var y  = Math.max(r1.bottom, r2.bottom) - dagRect.top + 6;

    /* Arc depth: proportional to distance, minimum 30 px */
    var sag = Math.max(30, Math.abs(x2 - x1) * 0.15);

    /* Cubic Bézier curving downward */
    var path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    path.setAttribute('d',
      'M ' + x1 + ' ' + y +
      ' C ' + x1 + ' ' + (y + sag) + ',' +
              x2 + ' ' + (y + sag) + ',' +
              x2 + ' ' + y);
    path.setAttribute('stroke',           '#886644');
    path.setAttribute('stroke-width',     '1.5');
    path.setAttribute('fill',             'none');
    path.setAttribute('stroke-dasharray', '4,3');
    svg.appendChild(path);

    /* Marriage year label at arc midpoint, consistent with "&year"
     * style used for hr-connected couples in the dag table */
    if (year !== null) {
      var xMid = (x1 + x2) / 2;
      var yMid = y + sag + 12;
      var text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
      text.setAttribute('x',           xMid);
      text.setAttribute('y',           yMid);
      text.setAttribute('text-anchor', 'middle');
      text.setAttribute('font-size',   '70%');
      text.setAttribute('fill',        '#886644');
      text.textContent = '\u0026' + year;   /* &year */
      svg.appendChild(text);
    }
  });
});
