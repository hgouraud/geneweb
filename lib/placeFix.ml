module Driver = Geneweb_db.Driver

type rule = Dept_suffix | Dept_info_last | Dept_number | Address

let rules = [ Dept_suffix; Dept_info_last; Dept_number; Address ]

let rule_of_string s =
  match s with
  | "dept" -> Some Dept_suffix
  | "dept-info" -> Some Dept_info_last
  | "dept-num" -> Some Dept_number
  | "addr" -> Some Address
  | _ -> None

let string_of_rule r =
  match r with
  | Dept_suffix -> "dept"
  | Dept_info_last -> "dept-info"
  | Dept_number -> "dept-num"
  | Address -> "addr"

let departements =
  [
    "Ain";
    "Aisne";
    "Allier";
    "Alpes-de-Haute-Provence";
    "Hautes-Alpes";
    "Alpes-Maritimes";
    "Ardèche";
    "Ardennes";
    "Ariège";
    "Aube";
    "Aude";
    "Aveyron";
    "Bouches-du-Rhône";
    "Calvados";
    "Cantal";
    "Charente";
    "Charente-Maritime";
    "Cher";
    "Corrèze";
    "Corse-du-Sud";
    "Haute-Corse";
    "Côte-d'Or";
    "Côtes-d'Armor";
    "Creuse";
    "Dordogne";
    "Doubs";
    "Drôme";
    "Eure";
    "Eure-et-Loir";
    "Finistère";
    "Gard";
    "Haute-Garonne";
    "Gers";
    "Gironde";
    "Hérault";
    "Ille-et-Vilaine";
    "Indre";
    "Indre-et-Loire";
    "Isère";
    "Jura";
    "Landes";
    "Loir-et-Cher";
    "Loire";
    "Haute-Loire";
    "Loire-Atlantique";
    "Loiret";
    "Lot";
    "Lot-et-Garonne";
    "Lozère";
    "Maine-et-Loire";
    "Manche";
    "Marne";
    "Haute-Marne";
    "Mayenne";
    "Meurthe-et-Moselle";
    "Meuse";
    "Morbihan";
    "Moselle";
    "Nièvre";
    "Nord";
    "Oise";
    "Orne";
    "Pas-de-Calais";
    "Puy-de-Dôme";
    "Pyrénées-Atlantiques";
    "Hautes-Pyrénées";
    "Pyrénées-Orientales";
    "Bas-Rhin";
    "Haut-Rhin";
    "Rhône";
    "Haute-Saône";
    "Saône-et-Loire";
    "Sarthe";
    "Savoie";
    "Haute-Savoie";
    "Seine-Maritime";
    "Seine-et-Marne";
    "Yvelines";
    "Deux-Sèvres";
    "Somme";
    "Tarn";
    "Tarn-et-Garonne";
    "Var";
    "Vaucluse";
    "Vendée";
    "Vienne";
    "Haute-Vienne";
    "Vosges";
    "Yonne";
    "Territoire de Belfort";
    "Essonne";
    "Hauts-de-Seine";
    "Seine-Saint-Denis";
    "Val-de-Marne";
    "Val-d'Oise";
    "Guadeloupe";
    "Martinique";
    "Guyane";
    "La Réunion";
    "Mayotte";
    (* former departments *)
    "Basses-Alpes";
    "Basses-Pyrénées";
    "Charente-Inférieure";
    "Corse";
    "Côtes-du-Nord";
    "Loire-Inférieure";
    "Meurthe";
    "Seine";
    "Seine-et-Oise";
    "Seine-Inférieure";
  ]

(* the departments by number, 1 to 95: today's, then the former ones that
   had the same number; 2A and 2B are read as 201 and 202, 20 being the
   former Corse and the code still used for both *)
let numbered =
  [
    (1, "Ain");
    (2, "Aisne");
    (3, "Allier");
    (4, "Alpes-de-Haute-Provence");
    (4, "Basses-Alpes");
    (5, "Hautes-Alpes");
    (6, "Alpes-Maritimes");
    (7, "Ardèche");
    (8, "Ardennes");
    (9, "Ariège");
    (10, "Aube");
    (11, "Aude");
    (12, "Aveyron");
    (13, "Bouches-du-Rhône");
    (14, "Calvados");
    (15, "Cantal");
    (16, "Charente");
    (17, "Charente-Maritime");
    (17, "Charente-Inférieure");
    (18, "Cher");
    (19, "Corrèze");
    (20, "Corse");
    (20, "Corse-du-Sud");
    (20, "Haute-Corse");
    (201, "Corse-du-Sud");
    (202, "Haute-Corse");
    (21, "Côte-d'Or");
    (22, "Côtes-d'Armor");
    (22, "Côtes-du-Nord");
    (23, "Creuse");
    (24, "Dordogne");
    (25, "Doubs");
    (26, "Drôme");
    (27, "Eure");
    (28, "Eure-et-Loir");
    (29, "Finistère");
    (30, "Gard");
    (31, "Haute-Garonne");
    (32, "Gers");
    (33, "Gironde");
    (34, "Hérault");
    (35, "Ille-et-Vilaine");
    (36, "Indre");
    (37, "Indre-et-Loire");
    (38, "Isère");
    (39, "Jura");
    (40, "Landes");
    (41, "Loir-et-Cher");
    (42, "Loire");
    (43, "Haute-Loire");
    (44, "Loire-Atlantique");
    (44, "Loire-Inférieure");
    (45, "Loiret");
    (46, "Lot");
    (47, "Lot-et-Garonne");
    (48, "Lozère");
    (49, "Maine-et-Loire");
    (50, "Manche");
    (51, "Marne");
    (52, "Haute-Marne");
    (53, "Mayenne");
    (54, "Meurthe-et-Moselle");
    (55, "Meuse");
    (56, "Morbihan");
    (57, "Moselle");
    (58, "Nièvre");
    (59, "Nord");
    (60, "Oise");
    (61, "Orne");
    (62, "Pas-de-Calais");
    (63, "Puy-de-Dôme");
    (64, "Pyrénées-Atlantiques");
    (64, "Basses-Pyrénées");
    (65, "Hautes-Pyrénées");
    (66, "Pyrénées-Orientales");
    (67, "Bas-Rhin");
    (68, "Haut-Rhin");
    (69, "Rhône");
    (70, "Haute-Saône");
    (71, "Saône-et-Loire");
    (72, "Sarthe");
    (73, "Savoie");
    (74, "Haute-Savoie");
    (75, "Seine");
    (76, "Seine-Maritime");
    (76, "Seine-Inférieure");
    (77, "Seine-et-Marne");
    (78, "Yvelines");
    (78, "Seine-et-Oise");
    (79, "Deux-Sèvres");
    (80, "Somme");
    (81, "Tarn");
    (82, "Tarn-et-Garonne");
    (83, "Var");
    (84, "Vaucluse");
    (85, "Vendée");
    (86, "Vienne");
    (87, "Haute-Vienne");
    (88, "Vosges");
    (89, "Yonne");
    (90, "Territoire de Belfort");
    (91, "Essonne");
    (92, "Hauts-de-Seine");
    (93, "Seine-Saint-Denis");
    (94, "Val-de-Marne");
    (95, "Val-d'Oise");
  ]

(* other spellings whose key differs from the official one *)
let aliases = [ ("Réunion", "La Réunion") ]

(* terms above a department: a last term after the department that is one
   of them is no information on the place, as in "Saint-Pair, Manche,
   France" *)
let higher_levels =
  [
    "France";
    "Europe";
    "Royaume de France";
    (* overseas: the overseas departments themselves are departments *)
    "Départements d'outre-mer";
    "Département d'outre-mer";
    "Territoires d'outre-mer";
    "Collectivités d'outre-mer";
    "Outre-mer";
    "France d'outre-mer";
    "DOM";
    "TOM";
    "DOM-TOM";
    "DROM";
    "COM";
    "Antilles";
    "Antilles françaises";
    "Petites Antilles";
    "Caraïbes";
    "Indes occidentales";
    "Océan Indien";
    "Mascareignes";
    "Île Bourbon";
    "Île de la Réunion";
    "Réunion";
    "Saint-Pierre-et-Miquelon";
    "Saint-Barthélemy";
    "Wallis-et-Futuna";
    "Polynésie française";
    "Nouvelle-Calédonie";
    "Terres australes et antarctiques françaises";
    (* regions *)
    "Auvergne-Rhône-Alpes";
    "Bourgogne-Franche-Comté";
    "Bretagne";
    "Centre-Val de Loire";
    "Corse";
    "Grand Est";
    "Hauts-de-France";
    "Île-de-France";
    "Normandie";
    "Nouvelle-Aquitaine";
    "Occitanie";
    "Pays de la Loire";
    "Provence-Alpes-Côte d'Azur";
    "Alsace";
    "Aquitaine";
    "Auvergne";
    "Basse-Normandie";
    "Bourgogne";
    "Centre";
    "Champagne-Ardenne";
    "Franche-Comté";
    "Haute-Normandie";
    "Languedoc-Roussillon";
    "Limousin";
    "Lorraine";
    "Midi-Pyrénées";
    "Nord-Pas-de-Calais";
    "Picardie";
    "Poitou-Charentes";
    "Rhône-Alpes";
    "PACA";
    (* provinces *)
    "Angoumois";
    "Anjou";
    "Artois";
    "Aunis";
    "Béarn";
    "Berry";
    "Bourbonnais";
    "Champagne";
    "Dauphiné";
    "Flandre";
    "Foix";
    "Gascogne";
    "Guyenne";
    "Languedoc";
    "Lyonnais";
    "Maine";
    "Marche";
    "Nivernais";
    "Orléanais";
    "Poitou";
    "Provence";
    "Roussillon";
    "Saintonge";
    "Touraine";
  ]

let higher_keys = lazy (List.map PlaceTerms.cluster_key higher_levels)

let keys =
  lazy
    (List.map (fun d -> (PlaceTerms.cluster_key d, d)) departements
    @ List.map (fun (a, d) -> (PlaceTerms.cluster_key a, d)) aliases)

let departement t =
  let t = PlaceTerms.clean_term t in
  if t = "" then None
  else List.assoc_opt (PlaceTerms.cluster_key t) (Lazy.force keys)

let has_delims s =
  String.exists (fun c -> c = '(' || c = ')' || c = '[' || c = ']') s

(* a dash: "-", or the en dash or em dash (3 bytes in UTF-8) *)
let dash_len s i =
  let n = String.length s in
  if i < n && s.[i] = '-' then 1
  else if
    i + 2 < n
    && s.[i] = '\xE2'
    && s.[i + 1] = '\x80'
    && (s.[i + 2] = '\x93' || s.[i + 2] = '\x94')
  then 3
  else 0

let rec skip_spaces s i =
  if i < String.length s && s.[i] = ' ' then skip_spaces s (i + 1) else i

(* the end of the separator starting at [i], [None] when there is none *)
let separator s i =
  let j = skip_spaces s i in
  let k =
    if j < String.length s && s.[j] = ',' then j + 1 else j + dash_len s j
  in
  let k = skip_spaces s k in
  if k > i then Some k else None

(* "t1, ..., tn, D" or "t1, ..., tn (D)" split in ([t1; ...; tn], D) *)
let split_terms rest =
  let n = String.length rest in
  let body =
    match String.rindex_opt rest '(' with
    | Some i when n > 0 && rest.[n - 1] = ')' ->
        String.sub rest 0 i ^ "," ^ String.sub rest (i + 1) (n - i - 2)
    | Some _ | None -> rest
  in
  if has_delims body then None
  else
    let terms = List.map String.trim (String.split_on_char ',' body) in
    if List.exists (fun t -> t = "") terms then None
    else
      match List.rev terms with
      | d :: (_ :: _ as rev_terms) -> Some (List.rev rev_terms, d)
      | [ _ ] | [] -> None

let dept_suffix s =
  let s' = String.trim s in
  if s' = "" || s'.[0] <> '(' then None
  else
    match String.index_opt s' ')' with
    | None -> None
    | Some close -> (
        let info = String.trim (String.sub s' 1 (close - 1)) in
        if info = "" || has_delims info then None
        else
          match separator s' (close + 1) with
          | None -> None
          | Some k -> (
              let rest = String.sub s' k (String.length s' - k) in
              match split_terms (String.trim rest) with
              | None -> None
              | Some (terms, d) -> (
                  match departement d with
                  | None -> None
                  | Some d ->
                      let r =
                        Printf.sprintf "[%s] - %s (%s)" info
                          (String.concat ", " terms) d
                      in
                      if r = s then None else Some r)))

let is_higher t =
  List.mem (PlaceTerms.cluster_key t) (Lazy.force higher_keys)
  || departement t <> None

(* "rest, info" or "rest (info)": [Some (rest, info)] *)
let split_info s =
  let n = String.length s in
  let last_paren =
    if n > 0 && s.[n - 1] = ')' then
      match String.rindex_opt s '(' with
      | Some i -> Some (String.sub s 0 i, String.sub s (i + 1) (n - i - 2))
      | None -> None
    else None
  in
  match last_paren with
  | Some _ -> last_paren
  | None -> (
      match String.rindex_opt s ',' with
      | Some i -> Some (String.sub s 0 i, String.sub s (i + 1) (n - i - 1))
      | None -> None)

let dept_info_last s =
  let s' = String.trim s in
  if s' = "" || s'.[0] = '(' || s'.[0] = '[' then None
  else
    match split_info s' with
    | None -> None
    | Some (rest, info) -> (
        let info = String.trim info in
        if info = "" || has_delims info || is_higher info then None
        else
          match split_terms (String.trim rest) with
          | None -> None
          | Some (terms, d) -> (
              match departement d with
              | None -> None
              | Some d ->
                  let r =
                    Printf.sprintf "[%s] - %s (%s)" info
                      (String.concat ", " terms) d
                  in
                  if r = s then None else Some r))

(* "50", "05" or "2A"/"2B" (Corsica): the numbers of a department *)
let number_of t =
  let t = String.trim t in
  match String.uppercase_ascii t with
  | "2A" | "02A" -> Some 201
  | "2B" | "02B" -> Some 202
  | u ->
      if
        u <> ""
        && String.length u <= 3
        && String.for_all (fun c -> c >= '0' && c <= '9') u
      then
        let n = int_of_string u in
        if n >= 1 && n <= 95 then Some n else None
      else None

let names_of_number n =
  List.filter_map (fun (m, d) -> if m = n then Some d else None) numbered

(* a term of a place: a department number, a department name, both
   ("Manche 50", "50-Manche", "Manche - 50") or something else *)
type term = Num of int | Name of string | Both of int * string | Other

let strip_dash s =
  let s = String.trim s in
  let n = String.length s in
  if n > 0 && s.[n - 1] = '-' then String.trim (String.sub s 0 (n - 1))
  else if n > 0 && s.[0] = '-' then String.trim (String.sub s 1 (n - 1))
  else s

(* [t] cut at the first or the last space or dash, into a number and a
   department name in either order *)
let both_of t =
  let n = String.length t in
  let cuts =
    List.concat_map
      (fun c ->
        List.filter_map Fun.id [ String.index_opt t c; String.rindex_opt t c ])
      [ ' '; '-' ]
  in
  List.find_map
    (fun i ->
      let a = strip_dash (String.sub t 0 i)
      and b = strip_dash (String.sub t (i + 1) (n - i - 1)) in
      match ((number_of a, departement b), (number_of b, departement a)) with
      | (Some k, Some d), _ | _, (Some k, Some d) -> Some (Both (k, d))
      | _ -> None)
    cuts

let term_of t =
  let t = String.trim t in
  match (number_of t, departement t) with
  | Some n, _ -> Num n
  | None, Some d -> Name d
  | None, None -> Option.value ~default:Other (both_of t)

let consistent n d = List.mem d (names_of_number n)

(* "a (b)" read as "a, b" *)
let unparen seg =
  let seg = String.trim seg in
  let n = String.length seg in
  match String.index_opt seg '(' with
  | Some i when n > 0 && seg.[n - 1] = ')' ->
      [ String.sub seg 0 i; String.sub seg (i + 1) (n - i - 2) ]
  | Some _ | None -> [ seg ]

(* the suburb part "[x] - " kept as is, and the terms of the rest, each
   "a (b)" read as "a, b" *)
let suburb_and_terms s =
  let sub, rest = Place.split_suburb s in
  let prefix = if sub = "" then "" else "[" ^ sub ^ "] - " in
  let terms =
    List.concat_map unparen (String.split_on_char ',' rest)
    |> List.map String.trim
  in
  if List.exists (fun t -> t = "" || has_delims t) terms then None
  else Some (prefix, terms)

let dept_number ~exists s =
  match suburb_and_terms (String.trim s) with
  | None -> None
  | Some (prefix, terms) -> (
      let changed r = if r = s then None else Some r in
      let with_dept city d =
        changed (Printf.sprintf "%s%s (%s)" prefix (String.concat ", " city) d)
      in
      (* "city, 50": the same city with its department, if it exists *)
      let existing city n =
        let c = String.concat ", " city in
        List.find_map
          (fun d ->
            List.find_opt exists
              [ Printf.sprintf "%s (%s)" c d; Printf.sprintf "%s, %s" c d ])
          (names_of_number n)
        |> Fun.flip Option.bind (fun f -> changed (prefix ^ f))
      in
      let last_term rev_city last =
        match term_of last with
        | Both (n, d) when consistent n d -> with_dept (List.rev rev_city) d
        | Num n -> existing (List.rev rev_city) n
        | Both _ | Name _ | Other -> None
      in
      match List.rev terms with
      | last :: prev :: (_ :: _ as rev_city) -> (
          (* both forms in two terms: "..., 50, Manche", "..., Manche (50)",
             "... (Manche), 50" *)
          match (term_of prev, term_of last) with
          | Num n, Name d | Name d, Num n ->
              if consistent n d then with_dept (List.rev rev_city) d else None
          | (Num _ | Name _ | Both _ | Other), _ ->
              last_term (prev :: rev_city) last)
      | [ last; prev ] -> last_term [ prev ] last
      | [ _ ] | [] -> None)

(* the first words of a street, after its number *)
let road_words =
  [
    "rue";
    "avenue";
    "av.";
    "boulevard";
    "bd";
    "place";
    "impasse";
    "allée";
    "chemin";
    "quai";
    "route";
    "cours";
    "passage";
    "square";
    "ruelle";
    "sentier";
    "cité";
    "villa";
    "faubourg";
    "rond-point";
    "chaussée";
    "promenade";
    "esplanade";
    "carrefour";
  ]

(* the first words of a parish or a church *)
let church_words = [ "paroisse"; "église"; "chapelle" ]
let word_keys words = lazy (List.map PlaceTerms.cluster_key words)
let road_keys = word_keys road_words
let church_keys = word_keys church_words

(* the first word of [s] is one of [keys] *)
let starts_with_word keys s =
  let w =
    match String.index_opt s ' ' with Some i -> String.sub s 0 i | None -> s
  in
  w <> "" && List.mem (PlaceTerms.cluster_key w) (Lazy.force keys)

let is_digit c = c >= '0' && c <= '9'

(* the end of a street number starting at [i0] in [s]: "12", "12-14",
   "12 bis", "12bis", "12b", after an optional "n°" *)
let number_end s i0 =
  let n = String.length s in
  let i0 =
    if i0 + 2 < n && s.[i0] = 'n' && String.sub s (i0 + 1) 2 = "\xC2\xB0" then
      skip_spaces s (i0 + 3)
    else i0
  in
  let rec digits i = if i < n && is_digit s.[i] then digits (i + 1) else i in
  let i = digits i0 in
  if i = i0 then None
  else
    let i =
      if i + 1 < n && s.[i] = '-' && is_digit s.[i + 1] then digits (i + 1)
      else i
    in
    let lower = String.lowercase_ascii s in
    let at_end j = j = n || s.[j] = ' ' || s.[j] = ',' in
    let suffix =
      List.find_opt
        (fun x ->
          let l = String.length x in
          i + l <= n && String.sub lower i l = x && at_end (i + l))
        [ " quater"; " bis"; " ter"; "quater"; "bis"; "ter" ]
    in
    match suffix with
    | Some x -> Some (i + String.length x)
    | None ->
        if i < n && lower.[i] >= 'a' && lower.[i] <= 'z' && at_end (i + 1) then
          Some (i + 1)
        else if at_end i then Some i
        else None

(* [t] is a street number and nothing else *)
let number_only t =
  let t = String.trim t in
  number_end t 0 = Some (String.length t)

(* [Some (number, rest)] when [s] starts with a number, then a space or a
   comma *)
let leading_number s =
  let n = String.length s in
  match number_end s 0 with
  | None -> None
  | Some j ->
      let k = skip_spaces s j in
      let k = if k < n && s.[k] = ',' then skip_spaces s (k + 1) else k in
      if k = j || k >= n then None
      else Some (String.trim (String.sub s 0 j), String.sub s k (n - k))

(* [Some (street, number)] when [s] ends with a number after a space:
   "rue X 12", "rue X n° 12 bis" *)
let trailing_number s =
  let s = String.trim s in
  let n = String.length s in
  let rec try_from i =
    if i <= 0 then None
    else
      match String.rindex_from_opt s (i - 1) ' ' with
      | None -> None
      | Some k ->
          let nb = String.trim (String.sub s (k + 1) (n - k - 1)) in
          let street = String.trim (String.sub s 0 k) in
          if number_only nb && street <> "" then
            let street =
              (* "rue X n° 12": "n°" alone before the number *)
              let l = String.length street in
              if l > 3 && String.sub street (l - 3) 3 = "n\xC2\xB0" then
                String.trim (String.sub street 0 (l - 3))
              else street
            in
            Some (street, nb)
          else try_from k
  in
  try_from n

(* "terms, D", "terms (D)" or "terms, (D)": [Some (terms, D)] *)
let terms_and_dept tail =
  let tail = String.trim tail in
  let n = String.length tail in
  let rec unstutter i acc =
    (* ", (" read as " (" *)
    if i >= n then String.concat "" (List.rev acc)
    else if tail.[i] = ',' then
      let j = skip_spaces tail (i + 1) in
      if j < n && tail.[j] = '(' then unstutter j (" " :: acc)
      else unstutter (i + 1) (String.make 1 ',' :: acc)
    else unstutter (i + 1) (String.make 1 tail.[i] :: acc)
  in
  match split_terms (unstutter 0 []) with
  | Some (terms, d) -> Option.map (fun d -> (terms, d)) (departement d)
  | None -> None

(* a street from one or two terms, written "12, rue X" when it has a
   number: "12 rue X", "12, rue X", "rue X 12", "rue X, 12", "rue X";
   or a church: "paroisse X" *)
let street_of_terms terms =
  let terms = List.map String.trim terms in
  if List.exists (fun t -> t = "" || has_delims t) terms then None
  else
    (* the word of the road and at least one word after it *)
    let road t = starts_with_word road_keys t && String.contains t ' ' in
    let with_nb nb street = Some (nb ^ ", " ^ street) in
    match terms with
    | [ a; b ] when number_only a && road b && trailing_number b = None ->
        with_nb a b
    | [ a; b ] when road a && number_only b && trailing_number a = None ->
        with_nb b a
    | [ t ] -> (
        match leading_number t with
        | Some (nb, rest) when road rest && trailing_number rest = None ->
            with_nb nb rest
        | Some _ | None ->
            if road t then
              match trailing_number t with
              | Some (street, nb) when road street -> with_nb nb street
              | Some _ -> None
              | None -> Some t
            else if starts_with_word church_keys t then Some t
            else None)
    | _ -> None

let with_suburb s info tail =
  match terms_and_dept tail with
  | Some (terms, d) ->
      let r =
        Printf.sprintf "[%s] - %s (%s)" info (String.concat ", " terms) d
      in
      if r = s then None else Some r
  | None -> None

let take_drop k l =
  let rec aux k acc l =
    if k = 0 then Some (List.rev acc, l)
    else match l with x :: l -> aux (k - 1) (x :: acc) l | [] -> None
  in
  aux k [] l

(* the address as the first one or two terms, the place after it *)
let address_first s segs =
  List.find_map
    (fun k ->
      match take_drop k segs with
      | Some (street, (_ :: _ as tail)) ->
          Option.bind (street_of_terms street) (fun info ->
              with_suburb s info (String.concat "," tail))
      | Some (_, []) | None -> None)
    [ 2; 1 ]

(* the address as the last one or two terms, or in the parentheses ending
   the place *)
let address_last s s' segs =
  let n = String.length s' in
  let in_parens =
    match String.rindex_opt s' '(' with
    | Some i when n > 0 && s'.[n - 1] = ')' ->
        let inside = String.sub s' (i + 1) (n - i - 2) in
        Option.bind
          (street_of_terms (String.split_on_char ',' inside))
          (fun info -> with_suburb s info (String.sub s' 0 i))
    | Some _ | None -> None
  in
  match in_parens with
  | Some r -> Some r
  | None ->
      let rev = List.rev segs in
      List.find_map
        (fun k ->
          match take_drop k rev with
          | Some (street, (_ :: _ as head)) ->
              Option.bind
                (street_of_terms (List.rev street))
                (fun info ->
                  with_suburb s info (String.concat "," (List.rev head)))
          | Some (_, []) | None -> None)
        [ 2; 1 ]

let address s =
  let s' = String.trim s in
  if s' = "" || s'.[0] = '[' || s'.[0] = '(' then None
  else
    let segs = String.split_on_char ',' s' in
    match address_first s segs with
    | Some r -> Some r
    | None -> address_last s s' segs

let rewrite ?(exists = fun _ -> false) rule s =
  match rule with
  | Dept_suffix -> dept_suffix s
  | Dept_info_last -> dept_info_last s
  | Dept_number -> dept_number ~exists s
  | Address -> address s

let plan ?only base rule =
  let places = List.map fst (PlaceTerms.place_uses base) in
  (* a string is a place of the base: used by an event *)
  let exists =
    let h = Hashtbl.create (List.length places) in
    List.iter (fun i -> Hashtbl.replace h i ()) places;
    fun str ->
      match Driver.find_string base str with
      | Some i -> Hashtbl.mem h i
      | None -> false
  in
  (match only with
    | None -> places
    | Some l ->
        (* the ids posted are kept only if they are places *)
        let h = Hashtbl.create (List.length l) in
        List.iter (fun i -> Hashtbl.replace h i ()) l;
        List.filter (Hashtbl.mem h) places)
  |> List.filter_map (fun istr ->
      let old_s = Driver.sou base istr in
      match rewrite ~exists rule old_s with
      | None -> None
      | Some new_s ->
          let new_s = Mutil.normalize_utf_8 new_s in
          if new_s = old_s then None
          else
            Some
              {
                PlaceRename.istr;
                old_s;
                new_s;
                depths = [];
                kind = PlaceRename.New_place;
              })
  |> PlaceRename.classify base
