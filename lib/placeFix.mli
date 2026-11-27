(** Bulk corrections of the places dictionary: places whose whole string is
    rewritten by a fixed rule, previewed and applied like the other renamings of
    places ({!PlaceRename}). *)

type rule =
  | Dept_suffix
      (** ["(info) terms, Département"] becomes
          ["[info] - terms (Département)"]: a leading parenthesised part is made
          the suburb, and a final French department is put in parentheses (see
          {!dept_suffix}). *)
  | Dept_info_last
      (** ["terms, Département, info"] becomes ["[info] - terms (Département)"]:
          a last term after the department, possibly in parentheses
          (["terms, Département (info)"]), is made the suburb (see
          {!dept_info_last}). *)
  | Dept_number
      (** a department given by its number: ["terms, 50, Manche"] or
          ["terms, Manche (50)"] becomes ["terms (Manche)"], and ["terms (50)"]
          becomes ["terms (Manche)"] when that place exists (see
          {!dept_number}). *)
  | Address
      (** a street address or a parish first: ["12 rue X, terms (Manche)"]
          becomes ["[12, rue X] - terms (Manche)"],
          ["paroisse X, terms, Manche"] becomes
          ["[paroisse X] - terms (Manche)"] (see {!address}). *)

val rules : rule list
(** All the rules, in the order of their pages. *)

val rule_of_string : string -> rule option
(** The rule of the form field [fix]: ["dept"] is {!Dept_suffix}, ["dept-info"]
    {!Dept_info_last}, ["dept-num"] {!Dept_number}, ["addr"] {!Address}. *)

val string_of_rule : rule -> string

val departement : string -> string option
(** [departement t] is the official spelling of the French department [t], or
    [None] when [t] is no department. Case, accents, hyphens, spaces and
    "Saint"/"St" are ignored ({!PlaceTerms.cluster_key}). The departments of
    today (Paris excepted: it is mostly a town in the places) and the former
    ones (Seine-et-Oise, Basses-Pyrénées, Côtes-du-Nord...) are known. *)

val dept_suffix : string -> string option
(** [dept_suffix s] rewrites [s] when it is ["(info) sep t1, ..., tn, D"]:
    - [info] not empty, without brackets nor parentheses;
    - [sep] one or several spaces, or a comma or a dash (-, – or —) with
      optional spaces around;
    - [t1, ..., tn] at least one term, without brackets nor parentheses;
    - [D] a French department ({!departement}), possibly written ["tn (D)"]
      instead of ["tn, D"].

    The result is ["[info] - t1, ..., tn (D)"], [D] in its official spelling.
    [None] when [s] has not this form, or is already written so. *)

val dept_info_last : string -> string option
(** [dept_info_last s] rewrites [s] when it is ["t1, ..., tn, D, info"] or
    ["t1, ..., tn, D (info)"]:
    - [t1, ..., tn] at least one term, without brackets nor parentheses;
    - [D] a French department ({!departement}), possibly written ["tn (D)"]
      instead of ["tn, D"];
    - [info] not empty, without brackets nor parentheses, and neither a
      department nor a level above one: France, a region, an old province, an
      overseas term (outre-mer, DOM-TOM, Antilles, collectivities...)
      ("Saint-Pair, Manche, France" or "Saint-Pair, Manche, Normandie" are left
      as they are).

    The result is ["[info] - t1, ..., tn (D)"], [D] in its official spelling.
    [None] when [s] has not this form (a place with a suburb already, for
    instance). *)

val dept_number : exists:(string -> bool) -> string -> string option
(** [dept_number ~exists s] rewrites a place giving its department by its
    number, 1 to 95 (2A and 2B for Corsica; the former departments by the number
    they had: 78 is Yvelines or Seine-et-Oise), in two cases:
    - the number and the name are both its last terms, in either order and
      either form: ["t1, ..., tn, 50, Manche"], ["t1, ..., tn, Manche (50)"],
      ["t1, ..., tn (Manche), 50"], ["t1, ..., tn, Manche 50"]... becomes
      ["t1, ..., tn (Manche)"] when the number is the one of the name, the name
      kept as written (in its official spelling);
    - the number alone is its last term: ["t1, ..., tn (50)"] or
      ["t1, ..., tn, 50"] becomes the existing place ["t1, ..., tn (Manche)"]
      (or ["t1, ..., tn, Manche"]), [exists] telling whether a string is a place
      of the base.

    There must be at least one term before the department. A suburb
    (["[x] - ..."]) is kept. A number in a term with other text ("Paris 16",
    "Marseille 13", which may be districts) is not read as a department. *)

val address : string -> string option
(** [address s] rewrites a place starting or ending with a street address or a
    church. The address is one or two terms:
    - a street: a word such as rue, avenue, boulevard, place, impasse, allée,
      chemin, quai, route... with or without a number ("12", "12-14", "12 bis",
      "12b", "n° 12") before or after it, the comma between them optional:
      ["12 rue X"], ["12, rue X"], ["rue X 12"], ["rue X, 12"], ["rue X"]; it is
      written ["12, rue X"] when it has a number;
    - a church: paroisse, église or chapelle, then a text.

    It comes first (["rue X, t1, ..., tn (D)"]) or last, after a comma or in
    parentheses (["t1, ..., tn (D), rue X"], ["t1, ..., tn, D (12 rue X)"]). The
    department [D] may be written ["tn, D"], ["tn (D)"] or ["tn, (D)"], and
    there must be at least one term before it. The result is
    ["[address] - t1, ..., tn (D)"], [D] in its official spelling. The words are
    compared ignoring case and accents. [None] when [s] has not this form. *)

val rewrite : ?exists:(string -> bool) -> rule -> string -> string option
(** The rewriting of a place by a rule; [exists] (none by default) is used by
    {!Dept_number}. *)

val plan :
  ?only:Geneweb_db.Driver.istr list ->
  Geneweb_db.Driver.base ->
  rule ->
  PlaceRename.item list
(** Every place of the base rewritten by the rule, or those of [only],
    classified by {!PlaceRename.classify}: the new string may already exist, or
    be shared by several places. The places of the base are read from the index
    of the dictionaries. *)
