open Config

let t conf ?(c = 1) l =
  let s = Util.transl conf l in
  if c <> 0 then Utf8.capitalize_fst s else s

let tn conf ?(c = 1) l n =
  let s = Util.transl_nth conf l n in
  if c <> 0 then Utf8.capitalize_fst s else s

(* Helper to get selected dictionaries from form *)
let get_selected_dicts conf =
  let dicts = ref [] in
  if Util.p_getenv conf.env "d_fn" = Some "1" then
    dicts := CheckData.Fnames :: !dicts;
  if Util.p_getenv conf.env "d_sn" = Some "1" then
    dicts := CheckData.Snames :: !dicts;
  if Util.p_getenv conf.env "d_pl" = Some "1" then
    dicts := CheckData.Places :: !dicts;
  if Util.p_getenv conf.env "d_pn" = Some "1" then
    dicts := CheckData.PubNames :: !dicts;
  if Util.p_getenv conf.env "d_qu" = Some "1" then
    dicts := CheckData.Qualifiers :: !dicts;
  if Util.p_getenv conf.env "d_al" = Some "1" then
    dicts := CheckData.Aliases :: !dicts;
  if Util.p_getenv conf.env "d_oc" = Some "1" then
    dicts := CheckData.Occupation :: !dicts;
  if Util.p_getenv conf.env "d_ti" = Some "1" then
    dicts := CheckData.Titles :: !dicts;
  if Util.p_getenv conf.env "d_es" = Some "1" then
    dicts := CheckData.Estates :: !dicts;
  if Util.p_getenv conf.env "d_sr" = Some "1" then
    dicts := CheckData.Sources :: !dicts;
  List.rev !dicts

(* Helper to get selected error types from form *)
let get_selected_error_types conf =
  let errors = ref [] in
  if Util.p_getenv conf.env "e_ic" = Some "1" then
    errors := CheckData.InvisibleCharacters :: !errors;
  if Util.p_getenv conf.env "e_bc" = Some "1" then
    errors := CheckData.BadCapitalization :: !errors;
  if Util.p_getenv conf.env "e_ms" = Some "1" then
    errors := CheckData.MultipleSpaces :: !errors;
  if Util.p_getenv conf.env "e_nb" = Some "1" then
    errors := CheckData.NonBreakingSpace :: !errors;
  List.rev !errors

(* Centralized max_results logic *)
let get_max_results conf =
  (* Get configuration value with default 150 *)
  let config_max =
    match List.assoc_opt "chk_data_max_results" conf.base_env with
    | Some "" -> None
    | Some s -> (
        try
          let n = int_of_string s in
          if n > 0 then Some n else Some 150
        with _ -> Some 150)
    | None -> Some 150
  in

  (* Get form value *)
  let form_max =
    match Util.p_getenv conf.env "max" with
    | Some "" -> None (* Empty input = no limit *)
    | Some s -> (
        try
          let n = int_of_string s in
          if n > 0 then Some n else None
        with _ -> None)
    | None -> None
  in

  (* Determine final max_results - CORRECTION ICI *)
  match (config_max, form_max) with
  | Some c, Some f -> Some (min c f) (* Toujours prendre le minimum *)
  | Some c, None -> Some c (* Pas de form, utiliser config *)
  | None, Some f -> Some f (* Pas de config, utiliser form *)
  | None, None -> None (* Aucune limite *)

(* Get dict name for display *)
let dict_name = function
  | CheckData.Fnames -> "first name/first names"
  | CheckData.Snames -> "surname/surnames"
  | CheckData.Places -> "place/places"
  | CheckData.PubNames -> "public name/public names"
  | CheckData.Qualifiers -> "qualifier/qualifiers"
  | CheckData.Aliases -> "alias/aliases"
  | CheckData.Occupation -> "occupation/occupations"
  | CheckData.Titles -> "title/titles"
  | CheckData.Estates -> "domain/domains"
  | CheckData.Sources -> "source/sources"

(* Convert dict type to data parameter for URLs *)
let dict_to_data_param = function
  | CheckData.Fnames -> "fn"
  | CheckData.Snames -> "sn"
  | CheckData.Places -> "place"
  | CheckData.PubNames -> "pubn"
  | CheckData.Qualifiers -> "qual"
  | CheckData.Aliases -> "alias"
  | CheckData.Occupation -> "occu"
  | CheckData.Titles -> "title"
  | CheckData.Estates -> "domain"
  | CheckData.Sources -> "src"

(* Get error type name for display *)
let error_type_name conf = function
  | CheckData.InvisibleCharacters ->
      t conf "chk_data error invisible characters"
  | CheckData.BadCapitalization -> t conf "chk_data error bad capitalization"
  | CheckData.MultipleSpaces -> t conf "chk_data error multiple spaces"
  | CheckData.NonBreakingSpace -> t conf "chk_data error non-breaking spaces"

(* Display results grouped by dictionary and error type *)
(* Dans checkDataDisplay.ml - remplacer display_results *)
let display_results conf base dicts selected_error_types max_results =
  let use_cache = Util.p_getenv conf.env "nocache" <> Some "1" in
  let total_found = ref 0 in
  let hit_limit = ref false in
  let missing_caches = ref [] in

  List.iter
    (fun dict ->
      if not !hit_limit then (
        (* Toujours récupérer TOUT le cache *)
        let entries =
          CheckData.collect_all_errors_with_cache ~max_results:None
            conf base dict
        in

        (* Check if cache file exists *)
        if use_cache && not (CheckData.cache_file_exists conf dict) then
          missing_caches := dict :: !missing_caches;

        (* Filter and limit results *)
        let filtered_entries = ref [] in
        
        List.iter
          (fun (s, errors) ->
            if not !hit_limit then (
              let selected_errors =
                List.filter (fun e -> List.mem e selected_error_types) errors
              in
              if selected_errors <> [] then (
                match max_results with
                | Some max_val when !total_found >= max_val ->
                    hit_limit := true
                | _ ->
                    filtered_entries := (s, selected_errors) :: !filtered_entries;
                    incr total_found)))
          entries;

        let filtered_entries = List.rev !filtered_entries in

        if filtered_entries <> [] then (
          Output.printf conf "<div id=\"cd\" class=\"card mt-3\">";
          Output.printf conf "<div class=\"card-header\">";
          Output.printf conf "<h3 class=\"font-weight bold mb-0\">%s</h3>"
            (tn conf (dict_name dict) 1);
          Output.printf conf "</div>";
          Output.printf conf "<div class=\"card-body\">";

          (* Group by error type *)
          List.iter
            (fun error_type ->
              let entries_for_error =
                List.filter_map
                  (fun (s, errors) ->
                    if List.mem error_type errors then
                      Some
                        (CheckData.make_error_html conf
                           (dict_to_data_param dict) s error_type)
                    else None)
                  filtered_entries
              in

              if entries_for_error <> [] then (
                Output.printf conf "<h4>%s (%d)</h4>"
                  (error_type_name conf error_type)
                  (List.length entries_for_error);
                Output.print_sstring conf "<div class=\"list-group flex-wrap\">";
                List.iter
                  (fun html -> Output.printf conf "%s" html)
                  entries_for_error;
                Output.print_sstring conf "</div>"))
            selected_error_types;

          Output.print_sstring conf "</div></div>")))
    dicts;

  (* Show missing cache warning if any *)
  (if !missing_caches <> [] then
     let missing_names =
       List.rev (List.map CheckData.dict_to_cache_name !missing_caches)
     in
     Output.printf conf
       "<div class=\"alert alert-danger mt-2\"><i class=\"fa \
        fa-exclamation-triangle mr-2\"></i>%s%s %s. %s<br>\
        %s%s <b><code \
        class=\"user-select-all\">.\\gw\\cache_file -bd ..\\bases -all \
        %s</code></b></div>"
       (tn conf "chk_data cache file not found" 0)
       (t conf ":")
       (String.concat ", " missing_names)
       (if List.length !missing_caches = 10 then
          "<b>" ^ tn conf "chk_data use database/cache" 0 ^ ".</b>"
        else "")
       (tn conf "chk_data cache file not found" 1)
       (t conf ":")
       conf.bname);

  (* Summary message *)
  if dicts <> [] then
    if !total_found = 0 then
      Output.printf conf "<div class=\"alert alert-info mt-3\">%s</div>"
        (t conf "no match")
    else
      let errors_msg = Util.ftransl conf "chk_data %d errors found" in
      let limit_msg =
        if !hit_limit then " (" ^ t ~c:0 conf "chk_data limit reached" ^ ")"
        else ""
      in
      Output.printf conf
        "<div class=\"alert alert-success mt-3\"><strong>%s</strong>%s</div>"
        (Printf.sprintf errors_msg !total_found)
        limit_msg

(* Main print function *)
let print conf base =
  let title _ = Output.print_sstring conf (t conf "data typographic checker") in
  Hutil.header conf title;
  Util.print_loading_overlay conf ();

  (* Get parameters *)
  let selected_dicts = get_selected_dicts conf in
  let selected_error_types = get_selected_error_types conf in

  (* Use centralized max_results logic *)
  let max_results = get_max_results conf in

  (* Get form max for display *)
  let form_max =
    match Util.p_getenv conf.env "max" with
    | Some s -> ( try Some (int_of_string s) with _ -> None)
    | None -> None
  in

  (* Get config max for validation *)
  let config_max =
    match List.assoc_opt "chk_data_max_results" conf.base_env with
    | Some "" -> None
    | Some s -> ( try Some (int_of_string s) with _ -> Some 150)
    | None -> Some 150
  in

  Output.print_sstring conf "<div class=\"container mt-3\">";

  (* Display form *)
  Output.print_sstring conf {|<form method="get" action="|};
  Output.print_string conf (Util.commd conf);
  Output.print_sstring conf {|" class="mt-4" id="chk-data-form">|};
  Output.print_sstring conf {|<input type="hidden" name="m" value="CHK_DATA">|};

  (* Dictionaries Section *)
  Output.print_sstring conf {|<div class="d-flex justify-content-center mb-3">|};
  Output.print_sstring conf {|<div class="card">|};
  Output.print_sstring conf {|<div class="card-header">|};
  Output.print_sstring conf {|<h5 class="mb-0">|};
  Output.print_sstring conf (t conf "chk_data books to check");
  Output.print_sstring conf " (";
  Output.print_sstring conf (string_of_int (List.length selected_dicts));
  Output.print_sstring conf {|)</h5></div>|};
  Output.print_sstring conf
    {|<div class="card-body d-flex align-items-start flex-column">
       <div class="d-flex flex-row mb-2">|};

  (* Left column *)
  Output.print_sstring conf {|<div class="w-auto mr-2">|};
  let print_dict_checkbox id code icon label checked =
    Output.printf conf
      {|<div class="form-check">
         <input class="form-check-input" type="checkbox" name="%s" id="%s"
                value="1"%s>
         <label class="form-check-label" for="%s">
           <i class="fa fa-%s fa-fw mr-1"></i>%s
         </label>
       </div>|}
      code id
      (if checked then " checked" else "")
      id icon label
  in

  print_dict_checkbox "dict-fn" "d_fn" "child"
    (tn conf "first name/first names" 1)
    (List.mem CheckData.Fnames selected_dicts);
  print_dict_checkbox "dict-sn" "d_sn" "user"
    (tn conf "surname/surnames" 1)
    (List.mem CheckData.Snames selected_dicts);
  print_dict_checkbox "dict-pl" "d_pl" "map-marker-alt"
    (tn conf "place/places" 1)
    (List.mem CheckData.Places selected_dicts);
  print_dict_checkbox "dict-pn" "d_pn" "bullhorn"
    (tn conf "public name/public names" 1)
    (List.mem CheckData.PubNames selected_dicts);
  print_dict_checkbox "dict-qu" "d_qu" "comment"
    (tn conf "qualifier/qualifiers" 1)
    (List.mem CheckData.Qualifiers selected_dicts);

  Output.print_sstring conf {|</div><div>|};

  (* Right column *)
  print_dict_checkbox "dict-al" "d_al" "mask"
    (tn conf "alias/aliases" 1)
    (List.mem CheckData.Aliases selected_dicts);
  print_dict_checkbox "dict-oc" "d_oc" "briefcase"
    (tn conf "occupation/occupations" 1)
    (List.mem CheckData.Occupation selected_dicts);
  print_dict_checkbox "dict-ti" "d_ti" "crown" (tn conf "title/titles" 1)
    (List.mem CheckData.Titles selected_dicts);
  print_dict_checkbox "dict-es" "d_es" "building"
    (tn conf "domain/domains" 1)
    (List.mem CheckData.Estates selected_dicts);
  print_dict_checkbox "dict-sr" "d_sr" "box-archive"
    (tn conf "source/sources" 1)
    (List.mem CheckData.Sources selected_dicts);

  Output.print_sstring conf {|</div></div>|};
  Output.print_sstring conf
    {|<div class="mt-auto align-self-center">
       <button type="button" class="btn btn-sm btn-outline-primary"
               onclick="toggleAllDicts()">
         <i class="fa fa-check-square mr-1"></i>|};
  Output.print_sstring conf (t conf "toggle all");
  Output.print_sstring conf {|</button></div></div></div>|};

  (* Error Types Section *)
  Output.print_sstring conf {|<div class="card mx-3">|};
  Output.print_sstring conf {|<div class="card-header">|};
  Output.print_sstring conf {|<h5 class="mb-0">|};
  Output.print_sstring conf (t conf "chk_data error types");
  Output.print_sstring conf " (";
  Output.print_sstring conf (string_of_int (List.length selected_error_types));
  Output.print_sstring conf {|)</h5></div>|};
  Output.print_sstring conf
    {|<div class="card-body d-flex align-items-start flex-column">|};

  let print_error_checkbox id code label checked =
    Output.printf conf
      {|<div class="form-check">
         <input class="form-check-input" type="checkbox" name="%s" id="%s"
                value="1"%s>
         <label class="form-check-label" for="%s">%s</label>
       </div>|}
      code id
      (if checked then " checked" else "")
      id label
  in

  print_error_checkbox "err-ic" "e_ic"
    (t conf "chk_data error invisible characters")
    (selected_error_types = []
    || List.mem CheckData.InvisibleCharacters selected_error_types);
  print_error_checkbox "err-bc" "e_bc"
    (t conf "chk_data error bad capitalization")
    (selected_error_types = []
    || List.mem CheckData.BadCapitalization selected_error_types);
  print_error_checkbox "err-ms" "e_ms"
    (t conf "chk_data error multiple spaces")
    (selected_error_types = []
    || List.mem CheckData.MultipleSpaces selected_error_types);
  print_error_checkbox "err-nb" "e_nb"
    (t conf "chk_data error non-breaking spaces")
    (selected_error_types = []
    || List.mem CheckData.NonBreakingSpace selected_error_types);

  Output.print_sstring conf
    {|<div class="mt-auto align-self-center">
       <button type="button" class="btn btn-sm btn-outline-primary"
               onclick="toggleAllErrors()">
         <i class="fa fa-check-square mr-1"></i>|};
  Output.print_sstring conf (t conf "toggle all");
  Output.print_sstring conf {|</button></div></div></div>|};

  (* Options Section *)
  Output.print_sstring conf {|<div class="card">|};
  Output.print_sstring conf {|<div class="card-header">|};
  Output.print_sstring conf {|<h5 class="mb-0">|};
  Output.print_sstring conf "<i class=\"fa fa-cog mr-1\"></i>";
  Output.print_sstring conf (t conf "options");
  Output.print_sstring conf {|</h5></div>|};
  Output.print_sstring conf {|<div class="card-body">|};
  let nocache_checked = Util.p_getenv conf.env "nocache" = Some "1" in
  let is_roglo =
    try List.assoc "roglo" conf.base_env = "yes" with Not_found -> false
  in
  if is_roglo then ()
  else
    Output.printf conf
      {|<div class="form-check">
         <input class="form-check-input" type="checkbox" name="nocache"
                id="use-db" value="1"%s>
         <label class="form-check-label" for="use-db">
           <i class="fa fa-database mr-2"></i>%s
         </label>
       </div>|}
      (if nocache_checked then " checked" else "")
      (tn conf "chk_data use database/cache" 0);

  Output.printf conf
    {|<div class="form-group">
       <label for="max-results">%s%s</label>
       <input type="number" class="form-control" name="max" id="max-results"
              step="10" value="%s" %s>%s</div>|}
    (t conf "chk_data max results")
    (t conf ":")
    (match form_max with Some n -> string_of_int n | None -> "")
    (match config_max with
    | Some n -> Printf.sprintf " max=\"%d\"" n
    | None -> "")
    (match config_max with
    | Some c ->
        Printf.sprintf "<small class=\"mt-1 ml-1 text-muted\">%s</small>"
          (Utf8.capitalize_fst
             (Printf.sprintf
                (Util.ftransl conf "chk_data limited to %d entries")
                c))
    | None -> "");

  Output.print_sstring conf {|</div></div></div>|};

  Output.print_sstring conf
    {|<div class="text-center">
       <button type="submit" class="btn btn-primary btn-lg"
               onclick="return validateAndSubmit()">
         <i class="fa fa-search mr-2"></i>|};
  Output.print_sstring conf (t conf "chk_data check data");
  Output.print_sstring conf {|</button></div></form>|};

  let status_msg =
    if selected_dicts <> [] && selected_error_types <> [] then
      let cache_index = if nocache_checked then 1 else 2 in
      Printf.sprintf
        "<div class=\"alert alert-info mt-3\"><i class=\"fa fa-database \
         mr-2\"></i>%s</div>"
        (tn conf "chk_data use database/cache" cache_index)
    else ""
  in
  Output.print_sstring conf status_msg;

  (* Results *)
  if selected_dicts <> [] && selected_error_types <> [] then
    display_results conf base selected_dicts selected_error_types max_results;

  Output.print_sstring conf {|</div></div>|};

  (* JavaScript for UI interactions *)
  Output.printf conf
    {|<script>
      function toggleAllDicts() {
        const checkboxes = document.querySelectorAll('[name^="d_"]');
        const allChecked = Array.from(checkboxes).every(cb => cb.checked);
        checkboxes.forEach(cb => cb.checked = !allChecked);
      }
      function toggleAllErrors() {
        const checkboxes = document.querySelectorAll('[name^="e_"]');
        const allChecked = Array.from(checkboxes).every(cb => cb.checked);
        checkboxes.forEach(cb => cb.checked = !allChecked);
      }
      function validateAndSubmit() {
        const maxInput = document.getElementById('max-results');
        const maxValue = parseInt(maxInput.value);
        const maxLimit = %s;
        
        // Check if value exceeds limit
        if (maxLimit && maxValue && maxValue > maxLimit) {
          alert("max" + maxLimit);
          maxInput.value = maxLimit;
          return false;
        }
        showOverlay();
        return true;
      }
    </script>|}
    (match config_max with Some n -> string_of_int n | None -> "null");

  Util.print_loading_overlay_js conf;
  Hutil.trailer conf
