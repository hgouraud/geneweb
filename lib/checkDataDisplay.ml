open Config

(* Translation with initial capital by default *)
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
  if Util.p_getenv conf.env "d_do" = Some "1" then
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

(* Get dict name for display *)
let dict_name = function
  | CheckData.Fnames -> "First names"
  | CheckData.Snames -> "Surnames"
  | CheckData.Places -> "Places"
  | CheckData.PubNames -> "Public names"
  | CheckData.Qualifiers -> "Qualifiers"
  | CheckData.Aliases -> "Aliases"
  | CheckData.Occupation -> "Occupations"
  | CheckData.Titles -> "Titles"
  | CheckData.Estates -> "Domains"
  | CheckData.Sources -> "Sources"

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
let display_results conf base dicts selected_error_types max_results =
  (* Check if we're NOT using cache (inverted logic) *)
  let use_cache = Util.p_getenv conf.env "nocache" <> Some "1" in
  
  (* Process each dictionary *)
  let total_found = ref 0 in
  let total_processed = ref 0 in
  let hit_limit = ref false in
  let missing_caches = ref [] in
  
  List.iter
    (fun dict ->
      if not !hit_limit then (
        let remaining =
          match max_results with
          | Some max -> Some (max - !total_found)
          | None -> None
        in
        
        let entries =
          CheckData.collect_all_errors_with_cache ~max_results:remaining conf
            base dict
        in
        
        (* Check if cache file exists *)
        if use_cache && not (CheckData.cache_file_exists conf dict) then
          missing_caches := dict :: !missing_caches;
        
        (* Filter by selected error types *)
        let filtered_entries =
          List.filter_map
            (fun (s, errors) ->
              let selected_errors =
                List.filter (fun e -> List.mem e selected_error_types) errors
              in
              if selected_errors <> [] then Some (s, selected_errors) else None)
            entries
        in
        
        if filtered_entries <> [] then (
          incr total_processed;
          Output.printf conf "<div class=\"card mt-3\">";
          Output.printf conf "<div class=\"card-header\">";
          Output.printf conf "<h3 class=\"mb-0\">%s</h3>" (dict_name dict);
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
                Output.print_sstring conf "<ul class=\"list-group mb-3\">";
                List.iter
                  (fun html ->
                    Output.printf conf "<li class=\"list-group-item\">%s</li>"
                      html)
                  entries_for_error;
                Output.print_sstring conf "</ul>";
                total_found := !total_found + List.length entries_for_error;
                
                (* Check if we hit the limit *)
                match max_results with
                | Some max when !total_found >= max -> hit_limit := true
                | _ -> ()))
            selected_error_types;
          
          Output.print_sstring conf "</div></div>")))
    dicts;
  
  (* Show missing cache warning if any *)
  if !missing_caches <> [] then (
    let missing_names = List.map dict_name !missing_caches in
    Output.printf conf
      "<div class=\"alert alert-warning mt-2\"><i class=\"fa \
       fa-exclamation-triangle mr-1\"></i>%s: %s<br><code>cache_file(.exe) \
       -all %s</code></div>"
      (t conf "chk_data cache file not found")
      (String.concat ", " missing_names)
      conf.bname);
  
  (* Summary with improved message *)
  if !total_processed = 0 then
    Output.printf conf "<div class=\"alert alert-info mt-3\">%s</div>"
      (t conf "no match")
  else
    let errors_msg = Util.ftransl conf "chk_data %d errors found" in
    let limit_msg =
      if !hit_limit then
        Printf.sprintf " (%s)" (t conf ~c:0 "chk_data limit reached")
      else ""
    in
    Output.printf conf
      "<div class=\"alert alert-success mt-3\"><strong>%s</strong>%s</div>"
      (Printf.sprintf errors_msg !total_found) limit_msg

(* Main print function *)
let print conf base =
  let title _ = Output.print_sstring conf (t conf "data typographic checker") in
  Hutil.header conf title;
  Util.print_loading_overlay conf ();
  
  (* Get parameters *)
  let selected_dicts = get_selected_dicts conf in
  let selected_error_types = get_selected_error_types conf in
  
  (* Get configuration value with default 150 *)
  let config_max =
    match List.assoc_opt "chk_data_max_results" conf.base_env with
    | Some "" -> None (* Empty string = no limit *)
    | Some s -> (try Some (int_of_string s) with _ -> Some 150)
    | None -> Some 150 (* Default value *)
  in
  
  (* Get form value *)
  let form_max =
    match Util.p_getenv conf.env "max" with
    | Some "" -> None (* Empty input = no limit *)
    | Some s -> (try Some (int_of_string s) with _ -> None)
    | None -> None
  in
  
  (* Determine final max_results:
     - Form value takes precedence if lower than config
     - Otherwise use config value *)
  let max_results =
    match config_max, form_max with
    | Some c, Some f when f < c -> Some f
    | Some c, _ -> Some c
    | None, Some f -> Some f
    | None, None -> None
  in
  
  Output.print_sstring conf "<div class=\"container mt-3\">";
  
  (* Display form *)
  Output.print_sstring conf {|<form method="get" action="|};
  Output.print_string conf (Util.commd conf);
  Output.print_sstring conf {|" class="mt-4">|};
  Output.print_sstring conf {|<input type="hidden" name="m" value="CHK_DATA">|};
  
  (* Dictionaries Section *)
  Output.print_sstring conf
    {|<div class="d-flex justify-content-center mb-3">|};
  Output.print_sstring conf {|<div class="card">|};
  Output.print_sstring conf {|<div class="card-header">|};
  Output.print_sstring conf {|<h5 class="mb-0">|};
  Output.print_sstring conf {|<i class="fa fa-book mr-2"></i>|};
  Output.print_sstring conf (t conf "chk_data books to check");
  Output.print_sstring conf {|</h5></div>|};
  Output.print_sstring conf
    {|<div class="card-body d-flex align-items-start flex-column">
       <div class="d-flex flex-row">|};
  
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
  print_dict_checkbox "dict-sn" "d_sn" "signature"
    (tn conf "surname/surnames" 1)
    (List.mem CheckData.Snames selected_dicts);
  print_dict_checkbox "dict-pl" "d_pl" "map-location-dot"
    (tn conf "place/places" 1)
    (List.mem CheckData.Places selected_dicts);
  print_dict_checkbox "dict-pn" "d_pn" "pen"
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
  print_dict_checkbox "dict-oc" "d_oc" "user-doctor"
    (tn conf "occupation/occupations" 1)
    (List.mem CheckData.Occupation selected_dicts);
  print_dict_checkbox "dict-ti" "d_ti" "crown" (tn conf "title/titles" 1)
    (List.mem CheckData.Titles selected_dicts);
  print_dict_checkbox "dict-do" "d_do" "chess-rook"
    (tn conf "domain/domains" 1)
    (List.mem CheckData.Estates selected_dicts);
  print_dict_checkbox "dict-sr" "d_sr" "box-archive"
    (tn conf "source/sources" 1)
    (List.mem CheckData.Sources selected_dicts);
  
  Output.print_sstring conf {|</div></div>|};
  Output.print_sstring conf
    {|<div class="mt-auto align-self-center">
       <button type="button" class="btn btn-sm btn-secondary" 
               onclick="toggleAllDicts()">
         <i class="fa fa-check-square mr-1"></i>|};
  Output.print_sstring conf (t conf "toggle all");
  Output.print_sstring conf {|</button></div></div></div>|};
  
  (* Error Types Section *)
  Output.print_sstring conf {|<div class="card mx-3">|};
  Output.print_sstring conf {|<div class="card-header">|};
  Output.print_sstring conf {|<h5 class="mb-0">|};
  Output.print_sstring conf {|<i class="fa fa-exclamation-triangle mr-2"></i>|};
  Output.print_sstring conf (t conf "chk_data error types");
  Output.print_sstring conf {|</h5></div>|};
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
       <button type="button" class="btn btn-sm btn-secondary" 
               onclick="toggleAllErrors()">
         <i class="fa fa-check-square mr-1"></i>|};
  Output.print_sstring conf (t conf "toggle all");
  Output.print_sstring conf {|</button></div></div></div>|};
  
  (* Options Section *)
  Output.print_sstring conf {|<div class="card">|};
  Output.print_sstring conf {|<div class="card-header">|};
  Output.print_sstring conf {|<h5 class="mb-0">|};
  Output.print_sstring conf {|<i class="fa fa-cog mr-2"></i>|};
  Output.print_sstring conf (t conf "options");
  Output.print_sstring conf {|</h5></div>|};
  Output.print_sstring conf {|<div class="card-body">|};
  
  (* Use database checkbox (inverted logic) *)
  let nocache_checked = Util.p_getenv conf.env "nocache" = Some "1" in
  let noverify_checked = Util.p_getenv conf.env "noverify" = Some "1" in
  
  Output.printf conf
    {|<div class="form-check">
       <input class="form-check-input" type="checkbox" name="nocache" id="use-db" 
              value="1"%s onchange="toggleVerifyOption(this)">
       <label class="form-check-label" for="use-db">
         <i class="fa fa-database mr-1"></i>%s
       </label>
     </div>|}
    (if nocache_checked then " checked" else "")
    (t conf "chk_data use database");
  
  (* Skip verification checkbox - disabled when using database *)
  Output.printf conf
    {|<div class="form-check mt-3">
       <input class="form-check-input" type="checkbox" name="noverify" 
              id="no-verify" value="1"%s%s>
       <label class="form-check-label%s" for="no-verify">
         <i class="fa fa-times-circle mr-1"></i>%s
       </label>
     </div>|}
    (if noverify_checked && not nocache_checked then " checked" else "")
    (if nocache_checked then " disabled" else "")
    (if nocache_checked then " text-muted" else "")
    (t conf "chk_data skip cache verification");
  
  (* Max results input - always enabled *)
  Output.printf conf
    {|<div class="form-group mt-3">
       <label for="max-results">%s%s</label>
       <input type="number" class="form-control" name="max" id="max-results"
              value="%s" min="1"%s style="width: 225px;"
              placeholder="%s">
       %s
     </div>|}
    (t conf "chk_data maximum results")
    (t conf ":")
    (match form_max with Some n -> string_of_int n | None -> "")
    (match config_max with Some n -> Printf.sprintf " max=\"%d\"" n | None -> "")
    (Util.transl conf "no limit")
    (match config_max with
    | Some c ->
        Printf.sprintf
          "<small class=\"form-text text-muted\">%s</small>"
          (Printf.sprintf (Util.ftransl conf "chk_data limited to %d first entries") c)
    | None -> "");
  
  Output.print_sstring conf {|</div></div></div>|};
  
  (* Submit Button *)
  Output.print_sstring conf
    {|<div class="text-center">
       <button type="submit" class="btn btn-primary btn-lg" 
               onclick="showOverlay()">
         <i class="fa fa-search mr-2"></i>|};
  Output.print_sstring conf (t conf "chk_data check data");
  Output.print_sstring conf {|</button></div></form>|};
  
  (* Status message AFTER submit button *)
  let status_msg =
    if selected_dicts <> [] && selected_error_types <> [] then
      if nocache_checked then
        Printf.sprintf
          "<div class=\"alert alert-info mt-3\"><i class=\"fa fa-database \
           mr-1\"></i>%s</div>"
          (t conf "chk_data use database")
      else if noverify_checked then
        Printf.sprintf
          "<div class=\"alert alert-warning mt-3\"><i class=\"fa \
           fa-exclamation-triangle mr-1\"></i>%s</div>"
          (t conf "chk_data verification disabled")
      else
        Printf.sprintf
          "<div class=\"alert alert-info mt-3\"><i class=\"fa fa-bolt \
           mr-1\"></i>%s</div>"
          (t conf "chk_data using cached data")
    else ""
  in
  Output.print_sstring conf status_msg;
  
  (* Results *)
  if selected_dicts <> [] && selected_error_types <> [] then
    display_results conf base selected_dicts selected_error_types max_results;
  
  Output.print_sstring conf {|</div></div>|};
  
  (* JavaScript for UI interactions *)
  Output.print_sstring conf
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
      
      function toggleVerifyOption(useDbCheckbox) {
        const verifyCheckbox = document.getElementById('no-verify');
        const verifyLabel = verifyCheckbox.nextElementSibling;
        if (useDbCheckbox.checked) {
          verifyCheckbox.checked = false;
          verifyCheckbox.disabled = true;
          verifyLabel.classList.add('text-muted');
        } else {
          verifyCheckbox.disabled = false;
          verifyLabel.classList.remove('text-muted');
        }
      }
    </script>|};
  
  Util.print_loading_overlay_js conf;
  Hutil.trailer conf