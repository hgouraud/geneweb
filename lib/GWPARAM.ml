(** This module allows plugins to modify geneweb configuration.

    This approch is preffered to Functors or library variants
    for simple functions if it does not come with a performance cost.
*)

let nb_errors = ref 0
let errors_undef = ref []
let errors_other = ref []
let set_vars = ref []
let gwd_cmd = ref ""

type syslog_level =
  [ `LOG_ALERT
  | `LOG_CRIT
  | `LOG_DEBUG
  | `LOG_EMERG
  | `LOG_ERR
  | `LOG_INFO
  | `LOG_NOTICE
  | `LOG_WARNING ]

module IperSet = Set.Make (struct
  type t = Gwdb.iper

  let compare = Stdlib.compare
end)

module Default = struct
  let init () = Secure.add_assets Filename.current_dir_name

  let base_path pref bname =
    List.fold_right Filename.concat (Secure.base_dir () :: pref) bname

  let bpath bname = Filename.concat (Secure.base_dir ()) bname

  (** [output_error ?headers ?content conf code]
      Send the http status [code], [headers] and
      [content] if provided, or default content otherwise.
  *)
  let output_error =
    let output_file conf fn =
      let ic = open_in fn in
      try
        in_channel_length ic |> really_input_string ic
        |> Output.print_sstring conf;
        close_in ic
      with _ -> ( try close_in ic with _ -> ())
    in
    fun ?(headers = []) ?(content : Adef.safe_string option) conf code ->
      Output.status conf code;
      List.iter (Output.header conf "%s") headers;
      Output.print_string conf (Adef.encoded "<h1>Incorrect request</h1>");
      match content with
      | Some content -> Output.print_string conf content
      | None -> (
          let code =
            match code with
            | Def.Bad_Request -> "400"
            | Unauthorized -> "401"
            | Forbidden -> "403"
            | Not_Found -> "404"
            | Conflict -> "409"
            | Internal_Server_Error -> "500"
            | Service_Unavailable -> "503"
            | OK | Moved_Temporarily -> assert false
          in
          let fname lang =
            code ^ "-" ^ lang ^ ".html"
            |> Filename.concat "etc" |> Mutil.search_asset_opt
          in
          match fname conf.lang with
          | Some fn -> output_file conf fn
          | None -> (
              match fname "en" with
              | Some fn -> output_file conf fn
              | None -> Output.print_sstring conf ""))

  (* is p2 an ancestor of p1? *)
  (* code copied from MergeInd.ml *)
  let is_ancestor conf base p1 p2 =
    let ip1 = Gwdb.get_iper p1 in
    let ip2 = Gwdb.get_iper p2 in
    if ip1 = ip2 then true
    else
      let rec loop n set tl =
        if n = 0 then false
        else
          match tl with
          | [] -> false
          | ip :: tl -> (
              if IperSet.mem ip set then loop n set tl
              else if ip = ip1 then true
              else
                let set = IperSet.add ip set in
                match Gwdb.get_parents (Gwdb.poi base ip) with
                | Some ifam ->
                    let cpl = Gwdb.foi base ifam in
                    loop (n - 1) set
                      (Gwdb.get_father cpl :: Gwdb.get_mother cpl :: tl)
                | None -> loop n set tl)
      in
      let max =
        try List.assoc "is_semi_public_max" conf.Config.base_env
        with Not_found -> "4" |> String.trim
        (* limit search to n generations *)
      in
      let max = if max = "" then 4 else int_of_string max in
      loop max IperSet.empty [ ip2 ]

  (* is semi public if the user (identified by conf.userkey is semi public
     and p is one of its descendant or ancesstor
  *)
  let is_semi_public conf base p =
    let split_key key =
      let dot = match String.index_opt key '.' with Some i -> i | _ -> -1 in
      let space =
        match String.index_from_opt key dot ' ' with Some i -> i | _ -> -1
      in
      let plus =
        match String.index_from_opt key dot '+' with Some i -> i | _ -> -1
      in
      let sep = if space > 0 then space else if plus > 0 then plus else -1 in
      if
        dot > 0 && sep > 0
        && String.length key > dot + 1
        && String.length key > sep + 1
      then
        ( String.sub key 0 dot,
          String.sub key (dot + 1) (sep - dot - 1),
          String.sub key (sep + 1) (String.length key - sep - 1) )
      else ("?", "", "?")
    in
    if conf.Config.userkey = "" then false
    else if Gwdb.get_access p = SemiPublic then true
    else
      let fn, oc, sn = split_key conf.Config.userkey in
      match
        Gwdb.person_of_key base fn sn (if oc = "" then 0 else int_of_string oc)
      with
      | Some ip1 ->
          (* I (ip1) am semi_public and p is a descendant or ancestor *)
          Gwdb.get_access (Gwdb.poi base ip1) = SemiPublic
          && (is_ancestor conf base p (Gwdb.poi base ip1)
             || is_ancestor conf base (Gwdb.poi base ip1) p)
      | _ -> false

  (* check that p is parent or descendant of conf.key *)

  (** Calcul les droits de visualisation d'une personne en
      fonction de son age.
      Renvoie (dans l'ordre des tests) :
      - Vrai si : magicien ou ami ou la personne est public
      - Vrai si : la personne est en si_titre, si elle a au moins un
                  titre et que public_if_title = yes dans le fichier gwf
      - Faux si : la personne n'est pas décédée et private_years > 0
      - Vrai si : la personne est plus agée (en fonction de la date de
                  naissance ou de la date de baptème) que privates_years
      - Faux si : la personne est plus jeune (en fonction de la date de
                  naissance ou de la date de baptème) que privates_years
      - Vrai si : la personne est décédée depuis plus de privates_years
      - Faux si : la personne est décédée depuis moins de privates_years
      - Vrai si : la personne a entre 80 et 120 ans et qu'elle n'est pas
                  privée et public_if_no_date = yes
      - Vrai si : la personne s'est mariée depuis plus de private_years
      - Faux dans tous les autres cas *)
  let p_auth conf base p =
    conf.Config.wizard
    (* is_semi_public takes into account ancestors and descendants *)
    || (conf.friend && is_semi_public conf base p)
    || Gwdb.get_access p = Public
    || conf.public_if_titles
       && Gwdb.get_access p = IfTitles
       && Gwdb.nobtitles base conf.allowed_titles conf.denied_titles p <> []
    ||
    let death = Gwdb.get_death p in
    if death = NotDead then conf.private_years < 1
    else
      let check_date d lim none =
        match d with
        | None -> none ()
        | Some d ->
            let a = Date.time_elapsed d conf.today in
            if a.Def.year > lim then true
            else if a.year < conf.private_years then false
            else a.month > 0 || a.day > 0
      in
      check_date (Gwdb.get_birth p |> Date.cdate_to_dmy_opt) conf.private_years
      @@ fun () ->
      check_date
        (Gwdb.get_baptism p |> Date.cdate_to_dmy_opt)
        conf.private_years
      @@ fun () ->
      check_date
        (Gwdb.get_death p |> Date.dmy_of_death)
        conf.private_years_death
      @@ fun () ->
      (Gwdb.get_access p <> Def.Private && conf.public_if_no_date)
      ||
      let families = Gwdb.get_family p in
      let len = Array.length families in
      let rec loop i =
        i < len
        && check_date
             (Array.get families i |> Gwdb.foi base |> Gwdb.get_marriage
            |> Date.cdate_to_dmy_opt)
             conf.private_years_marriage
             (fun () -> loop (i + 1))
      in
      loop 0

  let syslog (level : syslog_level) msg =
    let tm = Unix.(time () |> localtime) in
    let level =
      match level with
      | `LOG_EMERG -> "EMERGENCY"
      | `LOG_ALERT -> "ALERT"
      | `LOG_CRIT -> "CRITICAL"
      | `LOG_ERR -> "ERROR"
      | `LOG_WARNING -> "WARNING"
      | `LOG_NOTICE -> "NOTICE"
      | `LOG_INFO -> "INFO"
      | `LOG_DEBUG -> "DEBUG"
    in
    Printf.eprintf "[%s]: %s %s\n"
      (Mutil.sprintf_date tm : Adef.safe_string :> string)
      level msg

  let wrap_output (conf : Config.config) (title : Adef.safe_string)
      (content : unit -> unit) =
    let robot = List.assoc_opt "robot_index" conf.base_env = Some "yes" in
    Output.print_sstring conf {|<!DOCTYPE html><head><title>|};
    Output.print_string conf title;
    Output.print_sstring conf {|</title>|};
    Output.print_sstring conf
      (if robot then {|<meta name="robots" content="index,follow">|}
      else {|<meta name="robots" content="none">|});
    Output.print_sstring conf {|<meta charset="|};
    Output.print_sstring conf conf.charset;
    Output.print_sstring conf {|">|};
    Output.print_sstring conf
      {|<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">|};
    Output.print_sstring conf {|</head>|};
    Output.print_sstring conf "<body>";
    content ();
    Output.print_sstring conf {|</body></html>|}
end

let init = ref Default.init
let base_path = ref Default.base_path
let bpath = ref Default.bpath
let output_error = ref Default.output_error
let p_auth = ref Default.p_auth
let is_semi_public = ref Default.is_semi_public
let syslog = ref Default.syslog

(** [wrap_output conf title content]
    Plugins defining a page content but not a complete UI
    may want to wrap their page using [wrap_output].
*)
let wrap_output = ref Default.wrap_output
