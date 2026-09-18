// Sélecteur de langue pour le manuel GeneWeb.
//
// Hypothèse sur la structure de publication (voir .github/workflows/docs.yml) :
//   site/          -> français (langue source, à la racine)
//   site/en/       -> anglais
//   site/<lang>/   -> une future langue
//
// Ce script ajoute un petit sélecteur en haut de page. Il calcule l'URL
// équivalente dans l'autre langue en ajoutant/retirant le préfixe /<lang>/,
// et pointe vers l'accueil de cette langue si la page traduite n'existe pas
// encore (cas normal : toutes les pages ne sont pas traduites en même temps).

(function () {
  var KNOWN_LANGS = {
    fr: "Français",
    en: "English",
  };

  function currentLang() {
    var parts = window.location.pathname.split("/").filter(Boolean);
    // Le premier segment du chemin est un code de langue connu ?
    if (parts.length > 0 && KNOWN_LANGS.hasOwnProperty(parts[0])) {
      return { lang: parts[0], rest: parts.slice(1).join("/") };
    }
    return { lang: "fr", rest: parts.join("/") };
  }

  function buildSwitcher() {
    var current = currentLang();
    var container = document.createElement("div");
    container.className = "lang-switcher";

    Object.keys(KNOWN_LANGS).forEach(function (code) {
      var link = document.createElement("a");
      link.textContent = KNOWN_LANGS[code];
      link.href =
        code === "fr" ? "/" + current.rest : "/" + code + "/" + current.rest;
      if (code === current.lang) {
        link.classList.add("lang-switcher-active");
      }
      container.appendChild(link);
    });

    return container;
  }

  document.addEventListener("DOMContentLoaded", function () {
    var menuBar = document.querySelector(".menu-title") || document.body;
    var switcher = buildSwitcher();
    // Insertion juste après la barre de menu mdBook, avant le contenu.
    var content = document.getElementById("content");
    if (content && content.parentNode) {
      content.parentNode.insertBefore(switcher, content);
    } else {
      document.body.insertBefore(switcher, document.body.firstChild);
    }
  });
})();
