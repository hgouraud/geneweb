(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util
open Def

let get_number var key env = p_getint env (var ^ "_" ^ key)

let reconstitute_date_dmy conf var =
  match get_number var "yyyy" conf.env with
  | Some y -> (
      match get_number var "mm" conf.env with
      | Some m -> (
          match get_number var "dd" conf.env with
          | Some d ->
              if d >= 1 && d <= 31 && m >= 1 && m <= 12 then
                Some
                  Date.{ day = d; month = m; year = y; prec = Sure; delta = 0 }
              else None
          | None ->
              if m >= 1 && m <= 12 then
                Some { day = 0; month = m; year = y; prec = Sure; delta = 0 }
              else None)
      | None -> Some { day = 0; month = 0; year = y; prec = Sure; delta = 0 })
  | None -> None

let rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i

let rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i

let string_incl x y =
  let rec loop j_ini =
    if j_ini = String.length y then false
    else
      let rec loop1 i j =
        if i = String.length x then
          if j = String.length y then true
          else String.unsafe_get y j = ' ' || String.unsafe_get y (j - 1) = ' '
        else if
          j < String.length y && String.unsafe_get x i = String.unsafe_get y j
        then loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
  in
  loop 0

let abbrev_lower x = Name.abbrev (Name.lower x)
let sex_of_string = function "M" -> Def.Male | "F" -> Female | _ -> Neuter

(* Get the field name of an event criteria depending of the search type. *)
let get_event_field_name gets event_criteria event_name search_type =
  if search_type <> "OR" then event_name ^ "_" ^ event_criteria
  else if "on" = gets ("event_" ^ event_name) then event_criteria
  else ""

module Fields : sig
  type search = And | Or
  type name = string

  module OR : sig
    val date : name
    val place : name
  end

  module AND : sig
    val bapt_date : name
    val birth_date : name
    val death_date : name
    val burial_date : name
    val marriage_date : name
    val bapt_place : name
    val birth_place : name
    val death_place : name
    val burial_place : name
    val marriage_place : name
  end
end = struct
  type search = And | Or
  type name = string

  module OR = struct
    let place = "place"
    let date = "date"
  end

  module AND = struct
    let field_base criteria event = event ^ "_" ^ criteria
    let date_field = field_base "date"
    let place_field = field_base "place"
    let bapt_date = date_field "bapt"
    let birth_date = date_field "birth"
    let death_date = date_field "death"
    let burial_date = date_field "burial"
    let marriage_date = date_field "marriage"
    let bapt_place = place_field "bapt"
    let birth_place = place_field "birth"
    let death_place = place_field "death"
    let burial_place = place_field "burial"
    let marriage_place = place_field "marriage"
  end
end

module AdvancedSearchMatch : sig
  val match_name :
    search_list:string list list -> exact:bool -> string list -> bool

  val match_civil_status :
    base:Gwdb.base ->
    p:Gwdb.person ->
    sex:Def.sex ->
    married:string ->
    occupation:string ->
    first_name_list:string list list ->
    surname_list:string list list ->
    alias_list:string list list ->
    skip_fname:bool ->
    skip_sname:bool ->
    skip_alias:bool ->
    exact_first_name:bool ->
    exact_surname:bool ->
    exact_alias:bool ->
    bool

  val match_marriage :
    exact_place:bool ->
    conf:Config.config ->
    base:Gwdb.base ->
    p:Gwdb.person ->
    values:string list ->
    default:bool ->
    dates:Date.dmy option * Date.dmy option ->
    bool

  module type Match = sig
    val match_baptism :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_birth :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_burial :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_death :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool
  end

  module And : Match
  module Or : Match
end = struct
  let match_date ~p ~df ~default ~dates =
    let d1, d2 = dates in
    match (d1, d2) with
    | Some d1, Some d2 -> (
        match df p with
        | Some d -> Date.compare_dmy d d1 >= 0 && Date.compare_dmy d d2 <= 0
        | None -> false)
    | Some d1, None -> (
        match df p with Some d -> Date.compare_dmy d d1 >= 0 | None -> false)
    | None, Some d2 -> (
        match df p with Some d -> Date.compare_dmy d d2 <= 0 | None -> false)
    | None, None -> default

  let do_compare p y get cmp =
    let s = abbrev_lower @@ get p in
    List.exists (fun s' -> cmp (abbrev_lower s') s) y

  let apply_to_field_values_raw ~cmp ~p ~values ~get ~default =
    if values = [] then default else do_compare p values get cmp

  let apply_to_field_values ~get ~cmp ~base =
    apply_to_field_values_raw ~get:(fun p -> sou base @@ get p) ~cmp

  let match_sex ~p ~sex = if sex = Def.Neuter then true else get_sex p = sex

  let married_cmp p = function
    | "Y" -> get_family p <> [||]
    | "N" -> get_family p = [||]
    | _ -> true

  let match_married ~p ~married =
    if married = "" then true else married_cmp p married

  let exact_place_wrapper f ~exact_place =
    let cmp = if exact_place then ( = ) else string_incl in
    f ~cmp

  let match_baptism_place =
    exact_place_wrapper @@ apply_to_field_values ~get:get_baptism_place

  let match_birth_place =
    exact_place_wrapper @@ apply_to_field_values ~get:get_birth_place

  let match_death_place =
    exact_place_wrapper @@ apply_to_field_values ~get:get_death_place

  let match_burial_place =
    exact_place_wrapper @@ apply_to_field_values ~get:get_burial_place

  let match_marriage ~cmp ~conf ~base ~p ~values ~default ~dates =
    let d1, d2 = dates in
    let test_date_place df =
      Array.exists
        (fun ifam ->
          let fam = foi base ifam in
          let sp = poi base @@ Gutil.spouse (get_iper p) fam in
          if authorized_age conf base sp then
            df fam
            && (values = []
               || do_compare fam values
                    (fun f -> sou base @@ get_marriage_place f)
                    cmp)
          else false)
        (get_family p)
    in
    match (d1, d2) with
    | Some d1, Some d2 ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (get_marriage fam) with
            | Some d ->
                if Date.compare_dmy d d1 < 0 then false
                else if Date.compare_dmy d2 d < 0 then false
                else true
            | None -> false)
    | Some d1, None ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (get_marriage fam) with
            | Some d when authorized_age conf base p ->
                if Date.compare_dmy d d1 < 0 then false else true
            | Some _ | None -> false)
    | None, Some d2 ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (get_marriage fam) with
            | Some d when authorized_age conf base p ->
                if Date.compare_dmy d d2 > 0 then false else true
            | Some _ | None -> false)
    | None, None ->
        if places = [] then default else test_date_place (fun _ -> true)

  let match_marriage = exact_place_wrapper match_marriage

  let match_occupation ~base ~p ~occupation =
    if occupation = "" then true
    else
      string_incl (abbrev_lower occupation)
        (abbrev_lower @@ sou base @@ get_occupation p)

  let date_wrapper get_date =
    match_date ~df:(fun p -> Date.od_of_cdate (get_date p))

  let match_baptism_date = date_wrapper get_baptism
  let match_birth_date = date_wrapper get_birth

  let match_burial_date =
    let get_burial p =
      match get_burial p with
      | Buried cod | Cremated cod -> Date.od_of_cdate cod
      | _ -> None
    in
    match_date ~df:get_burial

  let match_death_date =
    let get_death p =
      match get_death p with
      | Death (_, cd) -> Some (Date.date_of_cdate cd)
      | _ -> None
    in
    match_date ~df:get_death

  let match_name ~search_list ~exact : string list -> bool =
    let eq : string list -> string list -> bool =
      if exact then fun x search ->
        List.sort compare search = List.sort compare x
      else fun x search -> List.for_all (fun s -> List.mem s x) search
    in
    fun x -> List.exists (eq x) search_list

  let wrap_match_name ~base ~search_list ~get ~exact ~split =
    if search_list = [] then fun _ -> true
    else
      let eq = match_name ~search_list ~exact in
      fun p ->
        eq
          (List.flatten
             (List.map
                (fun name -> List.map Name.lower (split name))
                (List.map (sou base) (get p))))

  let match_first_name ~base ~first_name_list ~exact =
    wrap_match_name ~base ~search_list:first_name_list
      ~get:(fun p ->
        (* wrap it in a list so it works on aliases too *) [ get_first_name p ])
      ~exact ~split:Name.split_fname

  (* we use [first_name_list] as the list of aliases to search for.
     so searching for a first name will also look at first name aliases *)
  let match_first_names_aliases ~base ~first_name_list ~exact =
    wrap_match_name ~base ~search_list:first_name_list
      ~get:get_first_names_aliases ~exact ~split:Name.split_fname

  let match_surname ~base ~surname_list ~exact =
    wrap_match_name ~base ~search_list:surname_list
      ~get:(fun p -> [ get_surname p ])
      ~exact ~split:Name.split_sname

  let match_surnames_aliases ~base ~surname_list ~exact =
    wrap_match_name ~base ~search_list:surname_list ~get:get_surnames_aliases
      ~exact ~split:Name.split_sname

  (* Check the civil status. The test is the same for an AND or a OR search request. *)
  let match_civil_status ~base ~p ~sex ~married ~occupation ~first_name_list
      ~surname_list ~alias_list ~skip_fname ~skip_sname ~skip_alias
      ~exact_first_name ~exact_surname ~exact_alias =
    match_sex ~p ~sex
    && (skip_fname
       || match_first_name ~base ~first_name_list ~exact:exact_first_name p
       || match_first_names_aliases ~base ~first_name_list
            ~exact:exact_first_name p)
    && (skip_sname
       || match_surname ~base ~surname_list ~exact:exact_surname p
       || match_surnames_aliases ~base ~surname_list ~exact:exact_surname p)
    && match_married ~p ~married
    && match_occupation ~base ~p ~occupation

  module type Match = sig
    val match_baptism :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_birth :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_burial :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_death :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool
  end

  module And = struct
    let match_and date_f place_f ~(base : Gwdb.base) ~p ~dates
        ~(places : string list) ~(exact_place : bool) =
      date_f ~p ~default:true ~dates
      && place_f ~exact_place ~base ~p ~values:places ~default:true

    let match_baptism = match_and match_baptism_date match_baptism_place
    let match_birth = match_and match_birth_date match_birth_place
    let match_burial = match_and match_burial_date match_burial_place
    let match_death = match_and match_death_date match_death_place
  end

  module Or = struct
    let match_or date_f place_f ~(base : Gwdb.base) ~p ~dates
        ~(places : string list) ~(exact_place : bool) =
      date_f ~p ~default:false ~dates
      || place_f ~exact_place ~base ~p ~values:places ~default:false

    let match_baptism = match_or match_baptism_date match_baptism_place
    let match_birth = match_or match_birth_date match_birth_place
    let match_burial = match_or match_burial_date match_burial_place
    let match_death = match_or match_death_date match_death_place
  end
end

(*
  Search for other persons in the base matching with the provided infos.

  On search semantic:

   Search can be set to be exact on the first name and/or the surname,
   if no first name or surname is provided then the search ignores the
   parameter in both the exact and the loose case.

   - When search is loose it is only necessary for each name atom (name atoms
   for "Jean-Pierre" are: [Jean] [Pierre]) to be found at least once in another
   person's name atoms in the base.

   - When search is exact, it is necessary for each atom to be found exactly the
   number of times it occurs in the given name but order is not considered for
   a person from the base to match. (ie. "Pierre-Jean de Bourbon de Vallois" matches
   with "Jean Pierre de Vallois de Bourbon" but not with "Jean de Bourbon")
*)
let advanced_search conf base max_answers =
  let open AdvancedSearchMatch in
  let hs = Hashtbl.create 73 in
  let hss = Hashtbl.create 73 in
  let hd = Hashtbl.create 73 in
  let getd x =
    try Hashtbl.find hd x
    with Not_found ->
      let v =
        ( reconstitute_date_dmy conf (x ^ "1"),
          reconstitute_date_dmy conf (x ^ "2") )
      in
      Hashtbl.add hd x v;
      v
  in
  let gets x =
    try Hashtbl.find hs x
    with Not_found ->
      let v = match p_getenv conf.env x with Some v -> v | None -> "" in
      Hashtbl.add hs x v;
      v
  in
  let getss x =
    let y = gets x in
    if y <> "" then [ y ]
    else
      match Hashtbl.find_opt hss x with
      | Some v -> v
      | None ->
          let rec loop acc i =
            let k = x ^ "_" ^ string_of_int i in
            match p_getenv conf.env k with
            | Some v -> loop (if v <> "" then v :: acc else acc) (i + 1)
            | None -> acc
          in
          let v = loop [] 1 in
          Hashtbl.add hss x v;
          v
  in
  let fn_list =
    List.map
      (fun s -> List.map Name.lower @@ Name.split_fname s)
      (getss "first_name")
  in
  let sn_list =
    List.map
      (fun s -> List.map Name.lower @@ Name.split_sname s)
      (getss "surname")
  in
  let alias_list =
    List.map
      (fun s ->
        List.map Name.lower
        @@ (* TODO which spil function to use here *) Name.split_sname s)
      (getss "alias")
  in
  let search_type = get_search_type gets in

  let exact_place = "on" = gets "exact_place" in

  let match_person ?(skip_fname = false) ?(skip_sname = false)
      ?(skip_alias = false) ((list, len) as acc) p search_type =
    let auth = authorized_age conf base p in

    let civil_match =
      lazy
        (match_civil_status ~base ~p
           ~sex:(gets "sex" |> sex_of_string)
           ~married:(gets "married") ~occupation:(gets "occu") ~skip_fname
           ~skip_sname ~first_name_list:fn_list ~surname_list:sn_list
           ~exact_first_name:(gets "exact_first_name" = "on")
           ~exact_surname:(gets "exact_surname" = "on")
           ~exact_alias:(gets "exact_alias" = "on"))
    in

    let pmatch =
      match search_type with
      | _ when not auth -> false
      | Fields.And ->
          Lazy.force civil_match
          && And.match_baptism ~base ~p ~exact_place
               ~dates:(getd Fields.AND.bapt_date)
               ~places:(getss Fields.AND.bapt_place)
          && And.match_birth ~base ~p ~exact_place
               ~dates:(getd Fields.AND.birth_date)
               ~places:(getss Fields.AND.birth_place)
          && And.match_burial ~base ~p ~exact_place
               ~dates:(getd Fields.AND.burial_date)
               ~places:(getss Fields.AND.burial_place)
          && And.match_death ~base ~p ~exact_place
               ~dates:(getd Fields.AND.death_date)
               ~places:(getss Fields.AND.death_place)
          && match_marriage ~conf ~base ~p ~exact_place ~default:true
               ~values:(getss Fields.AND.marriage_place)
               ~dates:(getd Fields.AND.marriage_date)
      | Fields.Or ->
          let match_f or_f and_f =
            or_f ~base ~p ~dates:(getd Fields.OR.date)
              ~places:(getss Fields.OR.place) ~exact_place
            && and_f ~base ~p ~dates:(getd Fields.OR.date)
                 ~places:(getss Fields.OR.place) ~exact_place
          in
          Lazy.force civil_match
          && (getss "place" = []
              && gets "date2_yyyy" = ""
              && gets "date1_yyyy" = ""
             || match_f Or.match_baptism And.match_baptism
             || match_f Or.match_birth And.match_birth
             || match_f Or.match_burial And.match_burial
             || match_f Or.match_death And.match_death
             || match_marriage ~conf ~base ~p ~exact_place ~default:false
                  ~values:(getss Fields.OR.place) ~dates:(getd Fields.OR.date))
    in

    if pmatch then (p :: list, len + 1) else acc
  in

  let list, len =
    if "on" = gets "sosa_filter" then
      match Util.find_sosa_ref conf base with
      | Some sosa_ref ->
          let rec loop p (set, acc) =
            if not (IperSet.mem (get_iper p) set) then
              let set = IperSet.add (get_iper p) set in
              let acc = match_person acc p search_type in
              match get_parents p with
              | Some ifam ->
                  let fam = foi base ifam in
                  let set, acc =
                    loop (pget conf base @@ get_mother fam) (set, acc)
                  in
                  loop (pget conf base @@ get_father fam) (set, acc)
              | None -> (set, acc)
            else (set, acc)
          in
          loop (pget conf base @@ get_iper sosa_ref) (IperSet.empty, ([], 0))
          |> snd
      | None -> ([], 0)
    else if fn_list <> [] || sn_list <> [] || alias_list <> [] then
      let list_aux strings_of persons_of split n_list exact =
        List.map
          (List.map (fun x ->
               let eq = match_name ~search_list:n_list ~exact in
               let istrs = strings_of base x in
               List.fold_left
                 (fun acc istr ->
                   let str = Mutil.nominative (sou base istr) in
                   if eq (List.map Name.lower @@ split str) then istr :: acc
                   else acc)
                 [] istrs))
          n_list
        |> List.flatten |> List.flatten |> List.sort_uniq compare
        |> List.map (spi_find @@ persons_of base)
        |> List.flatten |> List.sort_uniq compare
      in
      let skip_fname, skip_sname, list =
        if sn_list <> [] then
          ( false,
            true,
            list_aux Gwdb.base_strings_of_surname Gwdb.persons_of_surname
              Name.split_sname sn_list
              (gets "exact_surname" = "on") )
        else if fn_list <> [] then
          ( true,
            false,
            list_aux Gwdb.base_strings_of_first_name Gwdb.persons_of_first_name
              Name.split_fname fn_list
              (gets "exact_first_name" = "on") )
        else
          ( true,
            true,
            list_aux Gwdb.base_strings_of_alias Gwdb.persons_of_alias
              Name.split_sname alias_list
              (gets "exact_alias" = "on") )
      in
      let rec loop ((_, len) as acc) = function
        | [] -> acc
        | _ when len > max_answers -> acc
        | ip :: l ->
            loop
              (match_person ~skip_fname ~skip_sname acc (pget conf base ip)
                 search_type)
              l
      in
      loop ([], 0) list
    else
      Gwdb.Collection.fold_until
        (fun (_, len) -> len <= max_answers)
        (fun acc i -> match_person acc (pget conf base i) search_type)
        ([], 0) (Gwdb.ipers base)
  in
  (List.rev list, len)

(*
  Returns a description string for the current advanced search results in the correct language.
  e.g. "Search all Pierre, born in Paris, died in Paris"
*)
let searching_fields conf base =
  let test_date x =
    reconstitute_date_dmy conf (x ^ "1") <> None
    || reconstitute_date_dmy conf (x ^ "2") <> None
  in
  let gets x =
    match p_getenv conf.env x with
    | Some v when v <> "" -> v
    | _ ->
        let rec loop acc i =
          let k = x ^ "_" ^ string_of_int i in
          match p_getenv conf.env k with
          | Some v ->
              loop
                (if acc = "" then v
                else if v = "" then acc
                else acc ^ " / " ^ v)
                (i + 1)
          | None -> acc
        in
        loop "" 1
  in
  let test_string x = gets x <> "" in
  let getd x =
    (reconstitute_date_dmy conf (x ^ "1"), reconstitute_date_dmy conf (x ^ "2"))
  in
  let sex = match gets "sex" with "M" -> 0 | "F" -> 1 | _ -> 2 in
  (* Fonction pour tester un simple champ texte (e.g: first_name). *)
  let string_field x search =
    if test_string x then search ^ " " ^ gets x else search
  in
  (* Returns the place and date request. (e.g.: ...in Paris between 1800 and 1900) *)
  let get_place_date_request place_prefix_field_name date_prefix_field_name
      search =
    let search =
      match getd date_prefix_field_name with
      | Some d1, Some d2 ->
          Printf.sprintf "%s %s %s %s %s" search
            (transl conf "between (date)")
            (DateDisplay.string_of_dmy conf d1 :> string)
            (transl conf "and")
            (DateDisplay.string_of_dmy conf d2 :> string)
      | Some d1, _ ->
          Printf.sprintf "%s %s %s" search
            (transl conf "after (date)")
            (DateDisplay.string_of_dmy conf d1 :> string)
      | _, Some d2 ->
          Printf.sprintf "%s %s %s" search
            (transl conf "before (date)")
            (DateDisplay.string_of_dmy conf d2 :> string)
      | _ -> search
    in
    if test_string place_prefix_field_name then
      search ^ " " ^ transl conf "in (place)" ^ " "
      ^ gets place_prefix_field_name
    else search
  in
  (* Returns the event request. (e.g.: born in...) *)
  let get_event_field_request place_prefix_field_name date_prefix_field_name
      event_name search search_type =
    (* Separator character depends on search type operator, a comma for AND search, a slash for OR search. *)
    let sep =
      if search = "" then ""
      else
        match search_type with
        | Fields.And -> ", "
        | Or -> " / " (* TODO should be "|" ... *)
    in
    let search =
      if test_string place_prefix_field_name || test_date date_prefix_field_name
      then search ^ sep ^ transl_nth conf event_name sex
      else search
    in
    (* The place and date have to be shown after each event only for the AND request. *)
    match search_type with
    | Fields.And ->
        get_place_date_request place_prefix_field_name date_prefix_field_name
          search
    | Or -> search
  in
  let sosa_field search =
    if gets "sosa_filter" <> "" then
      match Util.find_sosa_ref conf base with
      | None -> search
      | Some p ->
          let s =
            Printf.sprintf
              (ftransl conf "direct ancestor of %s")
              (Util.gen_person_text conf base p :> string (* TODO check this *))
          in
          if search = "" then s
          else if s = "" then search
          else search ^ ", " ^ s
    else search
  in
  (* Search type can be AND or OR. *)
  let search_type = gets "search_type" in
  let bapt_date_field_name =
    get_event_field_name gets "date" "bapt" search_type
  in
  let birth_date_field_name =
    get_event_field_name gets "date" "birth" search_type
  in
  let death_date_field_name =
    get_event_field_name gets "date" "death" search_type
  in
  let burial_date_field_name =
    get_event_field_name gets "date" "burial" search_type
  in
  let marriage_date_field_name =
    get_event_field_name gets "date" "marriage" search_type
  in
  let bapt_place_field_name =
    get_event_field_name gets "place" "bapt" search_type
  in
  let birth_place_field_name =
    get_event_field_name gets "place" "birth" search_type
  in
  let death_place_field_name =
    get_event_field_name gets "place" "death" search_type
  in
  let burial_place_field_name =
    get_event_field_name gets "place" "burial" search_type
  in
  let marriage_place_field_name =
    get_event_field_name gets "place" "marriage" search_type
  in
  let search = "" in
  let search = string_field "first_name" search in
  let search = string_field "surname" search in
  let search = sosa_field search in
  let build_event_search event_search (s1, s2) =
    let date_field_name = get_event_field_name conf.env "date" s1 search_type in
    let place_field_name =
      get_event_field_name conf.env "place" s1 search_type
    in
    get_event_field_request place_field_name date_field_name s2 event_search
      search_type
  in
  let events =
    [|
      ("birth", "born");
      ("bapt", "baptized");
      ("marriage", "married");
      ("death", "died");
      ("burial", "buried");
    |]
  in
  let event_search = Array.fold_left build_event_search "" events in
  let search =
    if search = "" then event_search
    else if event_search = "" then search
    else search ^ ", " ^ event_search
  in
  (* Adding the place and date at the end for the OR request. *)
  let search =
    match search_type with
    | And -> search
    | Fields.Or ->
        if
          gets "place" != ""
          || gets "date2_yyyy" != ""
          || gets "date1_yyyy" != ""
        then get_place_date_request "place" "date" search
        else search
  in
  let search =
    if not (test_string marriage_place_field_name || test_date "marriage") then
      let sep = if search <> "" then ", " else "" in
      if gets "married" = "Y" then search ^ sep ^ transl conf "having a family"
      else if gets "married" = "N" then
        search ^ sep ^ transl conf "having no family"
      else search
    else search
  in

  let sep = if search <> "" then "," else "" in
  Adef.safe @@ string_field "occu" (search ^ sep)
