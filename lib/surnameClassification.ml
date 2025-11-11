(* surname_classification.ml *)
(* Shared utilities for classifying surnames into primary and alias groups *)

module Driver = Geneweb_db.Driver
module IperSet = Driver.Iper.Set

type surname_classification = {
  primary_surnames: (string * Driver.person list) list;
  alias_surnames: (string * (Driver.person * string) list) list;
  query_only_aliases: (Driver.person * string) list;
}

(** Classify persons in surname_groups into primary surnames and aliases.
    
    @param base The database
    @param conf The configuration (for visibility checks)
    @param query_string The search query (to identify query-specific aliases)
    @param surname_groups List of (surname, persons) from the search results
    @return A classification structure with primary surnames, aliases, and query-specific aliases
*)
let classify_surnames base conf query_string surname_groups =
  let primary_tbl = Hashtbl.create 100 in
  let alias_tbl = Hashtbl.create 100 in
  let query_aliases = ref [] in
  let query_lower = Name.lower query_string in
  
  (* Helper to add person to a hashtable group *)
  let add_to_primary sn person =
    let current = try Hashtbl.find primary_tbl sn with Not_found -> [] in
    Hashtbl.replace primary_tbl sn (person :: current)
  in
  
  let add_to_alias sn person alias_str =
    let current = try Hashtbl.find alias_tbl sn with Not_found -> [] in
    Hashtbl.replace alias_tbl sn ((person, alias_str) :: current)
  in
  
  (* Process each surname group *)
  List.iter (fun (sn, persons) ->
    let sn_lower = Name.lower sn in
    
    List.iter (fun p ->
      (* Visibility check *)
      let is_visible =
        not (Driver.Istr.is_empty (Driver.get_surname p)) &&
        not (Driver.Istr.is_empty (Driver.get_first_name p)) &&
        not (Util.is_hide_names conf p && not (Util.authorized_age conf base p))
      in
      
      if is_visible then
        let actual_surname = Driver.sou base (Driver.get_surname p) in
        let actual_sn_lower = Name.lower actual_surname in
        
        if actual_sn_lower = sn_lower then
          (* Person's primary surname matches the group *)
          add_to_primary sn p
        else
          (* Person appears here through an alias *)
          if sn_lower = query_lower then
            (* This is an alias for the query string *)
            query_aliases := (p, sn) :: !query_aliases
          else
            (* This is an alias for another surname *)
            add_to_alias sn p sn
    ) persons
  ) surname_groups;
  
  (* Convert hashtables to lists and reverse to maintain order *)
  let primary_surnames =
    Hashtbl.fold (fun sn persons acc -> (sn, List.rev persons) :: acc) primary_tbl []
  in
  
  let alias_surnames =
    Hashtbl.fold (fun sn person_alias_pairs acc ->
      (sn, List.rev person_alias_pairs) :: acc
    ) alias_tbl []
  in
  
  {
    primary_surnames;
    alias_surnames;
    query_only_aliases = List.rev !query_aliases;
  }

(** Get only primary surnames (no aliases) from classification *)
let get_primary classification = classification.primary_surnames

(** Get only alias surnames from classification *)
let get_aliases classification = classification.alias_surnames

(** Get only query-specific aliases from classification *)
let get_query_aliases classification = classification.query_only_aliases

(** Create an IperSet of all persons appearing as aliases *)
let alias_person_set classification =
  let add_pairs set person_alias_pairs =
    List.fold_left (fun acc (p, _) ->
      IperSet.add (Driver.get_iper p) acc
    ) set person_alias_pairs
  in
  
  let set1 = List.fold_left (fun acc (_, pairs) ->
    add_pairs acc pairs
  ) IperSet.empty classification.alias_surnames in
  
  add_pairs set1 classification.query_only_aliases
