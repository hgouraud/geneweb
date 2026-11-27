open Geneweb
open Alcotest

let normalize () =
  (check string) "" "foo-bar, boobar (baz)"
    (Place.normalize "[foo-bar] - boobar (baz)");
  (check string) "" "[foo-bar - boobar (baz)"
    (Place.normalize "[foo-bar - boobar (baz)");
  (check string) "" "[foo-bar] boobar (baz)"
    (Place.normalize "[foo-bar] boobar (baz)");
  ()

let split_suburb () =
  (check (pair string string))
    ""
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] - boobar (baz)");
  (check (pair string string))
    "test split suburb emdash - 93"
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] – boobar (baz)");
  (check (pair string string))
    "test split suburb endash - 94"
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] — boobar (baz)");
  (check (pair string string))
    "" ("", "boobar (baz)")
    (Place.split_suburb "boobar (baz)");
  ()

let only_suburb () =
  (check string) "" "foo-bar" (Place.only_suburb "[foo-bar] - boobar (baz)");
  (check string) "" "" (Place.only_suburb "boobar (baz)");
  ()

let without_suburb () =
  (check string) "" "boobar (baz)"
    (Place.without_suburb "[foo-bar] - boobar (baz)");
  (check string) "" "boobar (baz)" (Place.without_suburb "boobar (baz)");
  ()

let compare_places () =
  (check int) "" 0 (Place.compare_places "boobar (baz)" "boobar (baz)");
  (check int) "" (-1) (Place.compare_places "baz (boobar)" "boobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "baz (boobar)" "[foo-bar] - baz (boobar)");
  (check int) "" (-1)
    (Place.compare_places "[bar-foo] - baz (boobar)" "[foo-bar] - baz (boobar)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - baz (boobar)" "[bar-foo] - boobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - ebaz (boobar)"
       "[bar-foo] - éboobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - baz, boobar, barboo"
       "[foo-bar] - baz, boobar, barboo, bam");
  ()

let normalize_place () =
  (* Canonical leaf-first form; must byte-match m=L p0/p%d and *_place_norm. *)
  (check string) "berlin false" "Berlin, Allemagne"
    (Place.normalize_place false "Berlin, Allemagne");
  (check string) "berlin true" "Allemagne, Berlin"
    (Place.normalize_place true "Berlin, Allemagne");
  (check string) "paren false" "Paris, 75"
    (Place.normalize_place false "Paris (75)");
  (check string) "paren true" "75, Paris"
    (Place.normalize_place true "Paris (75)");
  (check string) "suburb false" "Paris 16e"
    (Place.normalize_place false "[Hameau Boileau] - Paris 16e");
  (check string) "suburb true" "Paris 16e"
    (Place.normalize_place true "[Hameau Boileau] - Paris 16e");
  (check string) "three false" "Paris, \195\142le-de-France, France"
    (Place.normalize_place false "Paris, \195\142le-de-France, France");
  (check string) "three true" "France, \195\142le-de-France, Paris"
    (Place.normalize_place true "Paris, \195\142le-de-France, France");
  ()

let fold_place_long () =
  (check (pair (list string) string))
    "fold non-inverted"
    ([ "France"; "Paris" ], "")
    (Place.fold_place_long false "Paris, France");
  (check (pair (list string) string))
    "fold inverted"
    ([ "Paris"; "France" ], "")
    (Place.fold_place_long true "Paris, France");
  (check (pair (list string) string))
    "fold paren"
    ([ "75"; "Paris" ], "")
    (Place.fold_place_long false "Paris (75)");
  (check (pair (list string) string))
    "fold suburb"
    ([ "Paris 16e" ], "Hameau Boileau")
    (Place.fold_place_long false "[Hameau Boileau] - Paris 16e");
  ()

let components () =
  let texts inv s =
    List.map
      (fun (c : Place.component) ->
        (* the span must point at the text in the raw string *)
        assert (String.sub s c.pos c.len = c.text);
        if c.suburb then "[" ^ c.text ^ "]" else c.text)
      (Place.components inv s)
  in
  let same_as_fold inv s =
    let pl, sub = Place.fold_place_long inv s in
    (check (list string))
      ("components = fold_place_long: " ^ s)
      (pl @ if sub = "" then [] else [ "[" ^ sub ^ "]" ])
      (texts inv s)
  in
  (check (list string))
    "suburb last, root first"
    [ "75"; "Paris"; "[Boileau]" ]
    (texts false "[Boileau] - Paris (75)");
  (check (list string))
    "inverted" [ "Paris"; "France" ]
    (texts true "Paris, France");
  List.iter
    (fun s ->
      same_as_fold false s;
      same_as_fold true s)
    [
      "Paris, France";
      "Paris (75)";
      "[Hameau Boileau] - Paris 16e";
      "[Boileau] \226\128\147 Saint-Martin, Manche, France";
      "a (b) , c (d)";
      "Paris , 75 ,France";
      " Paris";
      "x ()";
      "[a(b)] - c";
      "[a] - c, d (e f)";
      "a,,b";
    ];
  ()

let of_places () =
  let istr = Geneweb_db.Driver.Istr.of_string in
  let entries =
    PlaceTerms.of_places false
      [
        (istr "1", "Saint-Martin, Manche, France", 3);
        (istr "2", "[Le Bourg] - Saint-Martin, Manche, France", 2);
        (istr "3", "Saint-Martin, Calvados, France", 1);
      ]
  in
  let find t = List.find (fun (e : PlaceTerms.entry) -> e.term = t) entries in
  let sm = find "Saint-Martin" in
  (check int) "Saint-Martin contexts" 2 (List.length sm.contexts);
  (check int) "Saint-Martin places" 3 sm.nb_places;
  (check int) "Saint-Martin uses" 6 sm.uses;
  (check (list (list string)))
    "Saint-Martin parents, root first"
    [ [ "France"; "Calvados" ]; [ "France"; "Manche" ] ]
    (List.map (fun (c : PlaceTerms.context) -> c.parents) sm.contexts);
  let fr = find "France" in
  (check int) "France one top-level context" 1 (List.length fr.contexts);
  (check int) "France uses" 6 fr.uses;
  let bourg = find "Le Bourg" in
  (check bool) "suburb flagged" true (List.hd bourg.contexts).suburb;
  (check (list string))
    "suburb parents = full chain"
    [ "France"; "Manche"; "Saint-Martin" ]
    (List.hd bourg.contexts).parents;
  ()

let rename_rewrite () =
  let rw inv t nt raw ds =
    PlaceRename.rewrite inv ~terms:[ t ] ~new_term:nt raw ds
  in
  (check (option string))
    "leaf" (Some "Saint-L\195\180, Manche, France")
    (rw false "St-L\195\180" "Saint-L\195\180" "St-L\195\180, Manche, France"
       [ Some 2 ]);
  (check (option string))
    "trailing parenthesis kept" (Some "St-L\195\180 (Manche)")
    (rw false "50" "Manche" "St-L\195\180 (50)" [ Some 0 ]);
  (check (option string))
    "only the chosen depth" (Some "Paris, Lut\195\168ce, France")
    (rw false "Paris" "Lut\195\168ce" "Paris, Paris, France" [ Some 1 ]);
  (check (option string))
    "two depths" (Some "Lut\195\168ce, Lut\195\168ce, France")
    (rw false "Paris" "Lut\195\168ce" "Paris, Paris, France" [ Some 1; Some 2 ]);
  (check (option string))
    "suburb, separator kept" (Some "[Bourg] \226\128\147 Saint-Martin")
    (rw false "Le Bourg" "Bourg" "[Le Bourg] \226\128\147 Saint-Martin" [ None ]);
  (check (option string))
    "inverted" (Some "FR, Manche, Saint-L\195\180")
    (rw true "France" "FR" "France, Manche, Saint-L\195\180" [ Some 0 ]);
  (check (option string))
    "term no longer there" None
    (rw false "Manche" "Calvados" "Saint-Lo, Orne, France" [ Some 1 ]);
  (check bool) "new term ok" true (PlaceRename.valid_new_term "Saint-L\195\180");
  (check bool) "comma allowed" true (PlaceRename.valid_new_term "a, b");
  (check bool) "empty refused" false (PlaceRename.valid_new_term "  ");
  (check bool) "trailing parenthesis allowed" true
    (PlaceRename.valid_new_term "a (b)");
  (check bool) "inner parenthesis refused" false
    (PlaceRename.valid_new_term "a (b) c");
  (check bool) "empty parentheses refused" false
    (PlaceRename.valid_new_term "()");
  (check bool) "nested parentheses refused" false
    (PlaceRename.valid_new_term "(b (c))");
  (check bool) "bracket refused" false (PlaceRename.valid_new_term "a [b]");
  (check bool) "two parentheses refused" false
    (PlaceRename.valid_new_term "a (b) (c)");
  (check (option (pair string string)))
    "paren_form reads the last parenthesis"
    (Some ("a (b)", "c"))
    (Geneweb_db.Dict.paren_form "a (b) (c)");
  List.iter
    (fun (s, ok) ->
      (check bool) ("valid_place " ^ s) ok (PlaceRename.valid_place s))
    [
      ("Toulouse (Haute-Garonne)", true);
      ("[Saint-Simplice] - Metz, Moselle", true);
      ("Paris (75), France", true);
      ("", false);
      ("Metz [Saint-Simplice", false);
      ("[] - Metz", false);
      ("Metz, [Saint-Simplice] - Moselle", false);
      ("Toulouse (Haute-Garonne", false);
      ("Toulouse ((31))", false);
      ("Toulouse ()", false);
    ];
  (check bool) "suburb allowed" true
    (PlaceRename.valid_new_term "[Saint-Simplice] \xE2\x80\x93 Metz");
  (check bool) "lone parenthesis term allowed" true
    (PlaceRename.valid_new_term "(Haute-Garonne)");
  (check (option string))
    "last term into parentheses" (Some "Toulouse (Haute-Garonne)")
    (PlaceRename.rewrite false ~terms:[ "Haute-Garonne" ]
       ~new_term:"(Haute-Garonne)" "Toulouse, Haute-Garonne" [ Some 0 ]);
  (check (option string))
    "first term not into parentheses" (Some "Haute-Garonne, France")
    (PlaceRename.rewrite false ~terms:[ "Haute-Garonne" ]
       ~new_term:"(Haute-Garonne)" "Haute-Garonne, France" [ Some 1 ]);
  (check bool) "empty suburb refused" false
    (PlaceRename.valid_new_term "[] - Metz");
  (check (option string))
    "suburb at the start" (Some "[Saint-Simplice] - Metz, Moselle")
    (PlaceRename.rewrite false ~terms:[ "Saint-Simplice" ]
       ~new_term:"[Saint-Simplice] - Metz" "Saint-Simplice, Moselle" [ Some 1 ]);
  (check (option string))
    "suburb not as a parent" (Some "Metz, Saint-Simplice")
    (PlaceRename.rewrite false ~terms:[ "Saint-Simplice" ]
       ~new_term:"[Saint-Simplice] - Metz" "Metz, Saint-Simplice" [ Some 0 ]);
  ()

let rename_occurrence_key () =
  let i = Geneweb_db.Driver.Istr.of_string "123" in
  List.iter
    (fun o ->
      (check bool) "round trip" true
        (PlaceRename.occurrence_of_key (PlaceRename.occurrence_key o) = Some o))
    [ (i, Some 0); (i, Some 4); (i, None) ];
  (check bool) "other key" true (PlaceRename.occurrence_of_key "oc" = None);
  (check bool) "bad depth" true (PlaceRename.occurrence_of_key "o12.x" = None);
  ()

let clusters () =
  let k = PlaceTerms.cluster_key in
  List.iter
    (fun (a, b) -> (check string) (a ^ " ~ " ^ b) (k a) (k b))
    [
      ("Saint-Malo", "St Malo");
      ("Saint-Malo", "Saint Malo");
      ("Saint-Malo", "saint-malo");
      ("Saint-L\195\180", "St-L\195\180");
      ("Saint-L\195\180", "Saint Lo");
      ("Sainte-M\195\168re-\195\137glise", "Ste M\195\168re Eglise");
      ("Granville", "Granville ");
      ("Granville", "\u{200B}Granville\u{00A0}");
    ];
  (check bool) "Saint-Martin / Saint-Malo" false
    (k "Saint-Martin" = k "Saint-Malo");
  let istr = Geneweb_db.Driver.Istr.of_string in
  let entries =
    PlaceTerms.of_places false
      [
        (istr "1", "Saint-Malo, Ille-et-Vilaine, France", 3);
        (istr "2", "St Malo, Ille-et-Vilaine, France", 1);
        (istr "3", "saint-malo, Ille-et-Vilaine, France", 1);
        (istr "4", "Granville, Manche, France", 1);
      ]
  in
  (check (list (pair string (list string))))
    "clusters, most used first"
    [ ("st malo", [ "Saint-Malo"; "St Malo"; "saint-malo" ]) ]
    (List.map
       (fun (key, l) ->
         (key, List.map (fun (e : PlaceTerms.entry) -> e.term) l))
       (PlaceTerms.clusters entries));
  (* a single spelling is a cluster only when it must be cleaned *)
  (check (list string))
    "single unclean spelling" [ "Granville\u{00A0}" ]
    (List.concat_map
       (fun (_, l) -> List.map (fun (e : PlaceTerms.entry) -> e.term) l)
       (PlaceTerms.clusters
          (PlaceTerms.of_places false
             [
               (istr "5", "Granville\u{00A0}, Manche, France", 1);
               (istr "6", "Avranches, Manche, France", 1);
             ])));
  (check (option string))
    "rewrite several spellings" (Some "Saint-Malo, Saint-Malo, France")
    (PlaceRename.rewrite false
       ~terms:[ "St Malo"; "saint-malo" ]
       ~new_term:"Saint-Malo" "saint-malo, St Malo, France" [ Some 1; Some 2 ]);
  ()

let groups () =
  let istr i = Geneweb_db.Driver.Istr.of_string (string_of_int i) in
  let groups =
    PlaceGroups.of_places false
      (List.mapi
         (fun i raw -> (istr (i + 1), raw, 1))
         [
           "Avranches";
           "Avranches (Manche)";
           "Avranches, Manche";
           "Avranches, Manche, France";
           "Saint-Martin";
           "Saint-Martin, Manche, France";
           "Saint-Martin, Calvados, France";
           "[Le Bourg] - Saint-Martin, Manche, France";
           "Manche, France";
         ])
  in
  let raws l = List.map (fun (p : PlaceTerms.place) -> p.raw) l in
  (check (list (pair (list string) (list string))))
    "groups"
    [
      ( [ "Avranches, Manche, France" ],
        [ "Avranches"; "Avranches, Manche"; "Avranches (Manche)" ] );
      ([ "Saint-Martin, Calvados, France" ], [ "Saint-Martin" ]);
      ([ "Saint-Martin, Manche, France" ], [ "Saint-Martin" ]);
    ]
    (List.map
       (fun (g : PlaceGroups.group) -> (raws g.targets, raws g.members))
       groups);
  (check int) "Saint-Martin is ambiguous" 2
    (PlaceGroups.count_groups groups (istr 5));
  (check int) "Avranches is not" 1 (PlaceGroups.count_groups groups (istr 1));
  (* same terms: "aaaa (bbbb)" and "aaaa, bbbb" are variants of each other *)
  (check (list (pair (list string) (list string))))
    "variants"
    [ ([ "Avranches (Manche)"; "Avranches, Manche" ], []) ]
    (List.map
       (fun (g : PlaceGroups.group) -> (raws g.targets, raws g.members))
       (PlaceGroups.of_places false
          [
            (istr 1, "Avranches, Manche", 1); (istr 2, "Avranches (Manche)", 2);
          ]));
  ()

let summary () =
  let istr i = Geneweb_db.Driver.Istr.of_string (string_of_int i) in
  let places =
    List.mapi
      (fun i (raw, uses) -> (istr (i + 1), raw, uses))
      [
        ("Saint-Martin, Manche, France", 3);
        ("[Le Bourg] - Saint-Martin, Manche, France", 2);
        ("Saint-Martin, Calvados, France", 1);
        ("Paris, Paris, France", 4);
        ("Paris (75)", 1);
        ("Saint Martin, Manche", 5);
      ]
  in
  let counts l =
    List.map
      (fun (e : PlaceTerms.entry) -> (e.term, e.nb_places, e.uses, e.cluster))
      l
    |> List.sort compare
  in
  (check (list (pair (pair string int) (pair int string))))
    "same numbers"
    (List.map
       (fun (t, n, u, c) -> ((t, n), (u, c)))
       (counts (PlaceTerms.of_places false places)))
    (List.map
       (fun (t, n, u, c) -> ((t, n), (u, c)))
       (counts (PlaceTerms.summary false places)));
  (check (list (pair string (list string))))
    "same clusters"
    (List.map
       (fun (k, l) -> (k, List.map (fun (e : PlaceTerms.entry) -> e.term) l))
       (PlaceTerms.clusters (PlaceTerms.of_places false places)))
    (List.map
       (fun (k, l) -> (k, List.map (fun (e : PlaceTerms.entry) -> e.term) l))
       (PlaceTerms.clusters (PlaceTerms.summary false places)));
  (* only the terms of a cluster *)
  (check (list string))
    "only"
    [ "Saint Martin"; "Saint-Martin" ]
    (List.map
       (fun (e : PlaceTerms.entry) -> e.term)
       (PlaceTerms.of_places
          ~only:(PlaceTerms.in_cluster (PlaceTerms.cluster_key "St Martin"))
          false places));
  ()

let fix_dept () =
  let ok s expected =
    (check (option string)) s (Some expected) (PlaceFix.dept_suffix s)
  and no s = (check (option string)) s None (PlaceFix.dept_suffix s) in
  (* the three separators *)
  ok "(Le Bourg) Saint-Pair, Manche" "[Le Bourg] - Saint-Pair (Manche)";
  ok "(Le Bourg)   Saint-Pair, Manche" "[Le Bourg] - Saint-Pair (Manche)";
  ok "(Le Bourg), Saint-Pair, Manche" "[Le Bourg] - Saint-Pair (Manche)";
  ok "(Le Bourg) - Saint-Pair, Manche" "[Le Bourg] - Saint-Pair (Manche)";
  ok "(Le Bourg)-Saint-Pair, Manche" "[Le Bourg] - Saint-Pair (Manche)";
  ok "(Le Bourg) \xE2\x80\x93 Saint-Pair, Manche"
    "[Le Bourg] - Saint-Pair (Manche)";
  (* several terms, the department already in parentheses *)
  ok "(La Lande) Saint-Pair, Granville, Manche"
    "[La Lande] - Saint-Pair, Granville (Manche)";
  ok "(La Lande) Saint-Pair (Manche)" "[La Lande] - Saint-Pair (Manche)";
  (* the department in its official spelling, former departments *)
  ok "(Kerlouan) Brest, finistere" "[Kerlouan] - Brest (Finist\xC3\xA8re)";
  ok "(Le Moulin) Plouha, Cotes du Nord"
    "[Le Moulin] - Plouha (C\xC3\xB4tes-du-Nord)";
  ok "(x) Saint-Denis, Seine St Denis" "[x] - Saint-Denis (Seine-Saint-Denis)";
  (* not this form *)
  no "Saint-Pair, Manche";
  no "(Le Bourg) Manche";
  no "(Le Bourg) Saint-Pair, Normandie";
  no "() Saint-Pair, Manche";
  no "(Le (Haut) Bourg) Saint-Pair, Manche";
  no "(Le Bourg)Saint-Pair, Manche";
  no "(Le Bourg) Saint-Pair, , Manche";
  no "(Le Bourg) Saint-Pair (Haut), Manche";
  no "(rue X) 16e, Paris";
  no "[Le Bourg] - Saint-Pair (Manche)";
  ()

let fix_dept_info () =
  let ok s expected =
    (check (option string)) s (Some expected) (PlaceFix.dept_info_last s)
  and no s = (check (option string)) s None (PlaceFix.dept_info_last s) in
  ok "Saint-Pair, Manche, Le Bourg" "[Le Bourg] - Saint-Pair (Manche)";
  ok "Saint-Pair (Manche), Le Bourg" "[Le Bourg] - Saint-Pair (Manche)";
  ok "Saint-Pair (Manche) , Le Bourg" "[Le Bourg] - Saint-Pair (Manche)";
  ok "Saint-Pair, Granville, Manche, La Lande"
    "[La Lande] - Saint-Pair, Granville (Manche)";
  ok "Plouha, Cotes du Nord, Le Moulin"
    "[Le Moulin] - Plouha (C\xC3\xB4tes-du-Nord)";
  ok "Saint-Pair, Manche (Le Bourg)" "[Le Bourg] - Saint-Pair (Manche)";
  ok "Saint-Pair, Granville, Manche (La Lande)"
    "[La Lande] - Saint-Pair, Granville (Manche)";
  ok "Saint-Pair, Manche ( Le Bourg )" "[Le Bourg] - Saint-Pair (Manche)";
  (* the last term is no information on the place *)
  no "Saint-Pair, Manche, France";
  no "Saint-Pair, Manche (France)";
  no "Le Lamentin, Martinique, Antilles";
  no "Le Lamentin, Martinique, antilles";
  no "Le Lamentin (Martinique), Antilles fran\xC3\xA7aises";
  no "Le Lamentin, Martinique (DOM)";
  no "Le Lamentin, Martinique, D\xC3\xA9partements d'outre-mer";
  no "Le Lamentin, Martinique, departements d outre mer";
  no "Saint-Paul, R\xC3\xA9union, Ile Bourbon";
  no "Basse-Terre, Guadeloupe, Martinique";
  ok "Le Lamentin, Martinique, Morne Pitault"
    "[Morne Pitault] - Le Lamentin (Martinique)";
  no "Saint-Pair, Manche (Normandie)";
  no "Granville, Saint-Pair (Manche)";
  no "Saint-Pair (Manche)";
  no "Saint-Pair, Manche ()";
  no "Saint-Pair (Manche), Normandie";
  no "Saint-Pair, Manche, Basse-Normandie";
  no "Saint-Pair, Manche, Calvados";
  (* not this form *)
  no "Saint-Pair, Manche";
  no "Manche, Le Bourg";
  no "Saint-Pair, Normandie, Le Bourg";
  no "[Le Bourg] - Saint-Pair (Manche)";
  no "(Le Bourg) Saint-Pair, Manche";
  no "Saint-Pair, Manche, (Le Bourg)";
  ()

let fix_dept_number () =
  let places =
    [ "Saint-Pair (Manche)"; "Granville, Manche"; "Versailles (Seine-et-Oise)" ]
  in
  let exists s = List.mem s places in
  let ok s expected =
    (check (option string)) s (Some expected) (PlaceFix.dept_number ~exists s)
  and no s = (check (option string)) s None (PlaceFix.dept_number ~exists s) in
  (* both forms in the place *)
  ok "Saint-Pair, 50, Manche" "Saint-Pair (Manche)";
  ok "Saint-Pair, Manche, 50" "Saint-Pair (Manche)";
  ok "Saint-Pair, Manche (50)" "Saint-Pair (Manche)";
  ok "Saint-Pair (50), Manche" "Saint-Pair (Manche)";
  ok "Saint-Pair, Manche 50" "Saint-Pair (Manche)";
  ok "Saint-Pair (Manche - 50)" "Saint-Pair (Manche)";
  ok "Saint-Pair, 50-Manche" "Saint-Pair (Manche)";
  ok "Chaumont, 52-Haute-Marne" "Chaumont (Haute-Marne)";
  ok "Lolif, Avranches, 050, manche" "Lolif, Avranches (Manche)";
  ok "[Le Bourg] - Saint-Pair, 50, Manche" "[Le Bourg] - Saint-Pair (Manche)";
  ok "Ajaccio, Corse-du-Sud (2A)" "Ajaccio (Corse-du-Sud)";
  ok "Ajaccio, 20, Corse-du-Sud" "Ajaccio (Corse-du-Sud)";
  no "Ajaccio, Haute-Corse (2A)";
  ok "Versailles, 78, Seine-et-Oise" "Versailles (Seine-et-Oise)";
  no "Saint-Pair, 14, Manche";
  no "Saint-Pair (Manche)";
  no "50, Manche";
  (* the number alone, the city with its department in the dictionary *)
  ok "Saint-Pair (50)" "Saint-Pair (Manche)";
  ok "Saint-Pair, 50" "Saint-Pair (Manche)";
  ok "Granville (50)" "Granville, Manche";
  ok "Versailles (78)" "Versailles (Seine-et-Oise)";
  ok "[Le Bourg] - Saint-Pair (50)" "[Le Bourg] - Saint-Pair (Manche)";
  no "Coutances (50)";
  no "Saint-Pair (14)";
  no "Saint-Pair (96)";
  (* a number inside a term may be a district *)
  no "Paris 16";
  no "Marseille 13, Bouches-du-Rh\xC3\xB4ne";
  ()

let fix_address () =
  let ok s expected =
    (check (option string)) s (Some expected) (PlaceFix.address s)
  and no s = (check (option string)) s None (PlaceFix.address s) in
  ok "12, rue Lecampion, Granville (Manche)"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "12 rue Lecampion, Granville (Manche)"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "12 rue Lecampion, Granville, Manche"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "12 bis, Avenue de la Gare, Saint-Pair, Granville, (Manche)"
    "[12 bis, Avenue de la Gare] - Saint-Pair, Granville (Manche)";
  ok "3-5 boulevard des Amiraux, Granville, manche"
    "[3-5, boulevard des Amiraux] - Granville (Manche)";
  ok "7b all\xC3\xA9e des Pins, Granville (Manche)"
    "[7b, all\xC3\xA9e des Pins] - Granville (Manche)";
  ok "paroisse Saint-Paterne, Granville (Manche)"
    "[paroisse Saint-Paterne] - Granville (Manche)";
  ok "Eglise Notre-Dame, Granville, (Manche)"
    "[Eglise Notre-Dame] - Granville (Manche)";
  ok "chapelle de Lihou, Saint-Pair, Granville, Manche"
    "[chapelle de Lihou] - Saint-Pair, Granville (Manche)";
  (* the address last *)
  ok "Granville (Manche), 12 rue Lecampion"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "Granville, Manche, 12, rue Lecampion"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "Granville, Manche (12 rue Lecampion)"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "Granville (Manche) (12 bis, rue Lecampion)"
    "[12 bis, rue Lecampion] - Granville (Manche)";
  ok "Saint-Pair, Granville, Manche, paroisse Saint-Pair"
    "[paroisse Saint-Pair] - Saint-Pair, Granville (Manche)";
  ok "Granville, Manche (chapelle Notre-Dame du Cap Lihou)"
    "[chapelle Notre-Dame du Cap Lihou] - Granville (Manche)";
  (* no number, or the number after the street *)
  ok "rue Lecampion, Granville (Manche)" "[rue Lecampion] - Granville (Manche)";
  ok "Granville, Manche, rue Lecampion" "[rue Lecampion] - Granville (Manche)";
  ok "Place du March\xC3\xA9, Granville, Manche"
    "[Place du March\xC3\xA9] - Granville (Manche)";
  ok "rue Lecampion 12, Granville (Manche)"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "rue Lecampion, 12, Granville (Manche)"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "rue Lecampion 12 bis, Granville (Manche)"
    "[12 bis, rue Lecampion] - Granville (Manche)";
  ok "rue Lecampion n\xC2\xB0 12, Granville (Manche)"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "Granville (Manche), rue Lecampion, 12"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "Granville, Manche (rue Lecampion 12)"
    "[12, rue Lecampion] - Granville (Manche)";
  ok "Granville, Manche (rue Lecampion)" "[rue Lecampion] - Granville (Manche)";
  no "rue Lecampion, Manche";
  no "rue, Granville, Manche";
  no "12 rue, Granville, Manche";
  no "Granville, 12 rue Lecampion";
  no "Manche, 12 rue Lecampion";
  no "Granville, Manche, 12 Lecampion";
  no "12 rue Lecampion, Granville";
  no "12 rue Lecampion, Manche";
  no "12 Lecampion, Granville (Manche)";
  no "12rue Lecampion, Granville (Manche)";
  no "paroissien, Granville (Manche)";
  no "Granville (Manche)";
  no "[12, rue Lecampion] - Granville (Manche)";
  no "12 rue (haute), Granville (Manche)";
  ()

let v =
  [
    ("place-normalize", [ test_case "Place normalize" `Quick normalize ]);
    ( "place-normalize-place",
      [ test_case "Place normalize_place ordering" `Quick normalize_place ] );
    ( "place-fold-long",
      [ test_case "Place fold_place_long ordering" `Quick fold_place_long ] );
    ( "place-split-suburb",
      [ test_case "Place split suburb" `Quick split_suburb ] );
    ("place-only-suburb", [ test_case "Place only suburb" `Quick only_suburb ]);
    ( "place-without-suburb",
      [ test_case "Place without suburb" `Quick without_suburb ] );
    ("place-compare", [ test_case "Place compare" `Quick compare_places ]);
    ( "place-components",
      [ test_case "Place components spans" `Quick components ] );
    ("place-terms", [ test_case "PlaceTerms of_places" `Quick of_places ]);
    ( "place-terms-summary",
      [ test_case "PlaceTerms summary = of_places" `Quick summary ] );
    ("place-groups", [ test_case "PlaceGroups of_places" `Quick groups ]);
    ( "place-clusters",
      [ test_case "PlaceTerms cluster_key and clusters" `Quick clusters ] );
    ( "place-fix",
      [
        test_case "PlaceFix dept_suffix" `Quick fix_dept;
        test_case "PlaceFix dept_info_last" `Quick fix_dept_info;
        test_case "PlaceFix dept_number" `Quick fix_dept_number;
        test_case "PlaceFix address" `Quick fix_address;
      ] );
    ( "place-rename",
      [
        test_case "PlaceRename rewrite" `Quick rename_rewrite;
        test_case "PlaceRename occurrence keys" `Quick rename_occurrence_key;
      ] );
  ]
