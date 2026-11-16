open Config
module Driver = Geneweb_db.Driver

let commands =
  [|
    "MOD_IND_OK";
    "MOD_FAM_OK";
    "ADD_FAM_OK";
    "ADD_PAR_OK";
    "DEL_IND_OK";
    "DEL_FAM_OK";
    "CHG_CHN_OK";
    "CHG_EVT_IND_ORD_OK";
    "CHG_EVT_FAM_ORD_OK";
    "CHG_FAM_ORD_OK";
    "MOD_NOTES_OK";
    "MOD_WIZNOTES_OK";
    "CHG_TIT_ORD_OK";
  |]

let command_label conf m =
  let rec loop i =
    if i = Array.length commands then m
    else if commands.(i) = m then
      Utf8.capitalize_fst (Util.transl_nth conf "write queue command" i)
    else loop (i + 1)
  in
  loop 0

let state_label conf (state : WriteQueue.state) =
  Util.transl_nth conf "write queue state"
    (match state with
    | Pending -> 0
    | Running -> 1
    | Applied -> 2
    | Unchanged -> 3
    | Review -> 4
    | Failed -> 5)

let state_class (state : WriteQueue.state) =
  match state with
  | Pending | Running -> "text-body-secondary"
  | Applied | Unchanged -> "text-success"
  | Review | Failed -> "text-danger"

let time_label conf t =
  let tm = Unix.localtime t in
  let today =
    tm.Unix.tm_mday = conf.today.Adef.day
    && tm.Unix.tm_mon + 1 = conf.today.Adef.month
    && tm.Unix.tm_year + 1900 = conf.today.Adef.year
  in
  if today then Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min
  else
    Printf.sprintf "%02d/%02d %02d:%02d" tm.Unix.tm_mday (tm.Unix.tm_mon + 1)
      tm.Unix.tm_hour tm.Unix.tm_min

(* The person concerned by a request: [ip] for the family forms, [i] for the
   person forms. *)
let person_of base (item : WriteQueue.item) =
  let key =
    match List.assoc_opt "ip" item.env with
    | Some ip -> Some ip
    | None ->
        if
          List.mem item.command
            [ "MOD_IND_OK"; "DEL_IND_OK"; "CHG_EVT_IND_ORD_OK"; "CHG_CHN_OK" ]
        then List.assoc_opt "i" item.env
        else None
  in
  match key with
  | None -> None
  | Some s -> (
      match Driver.poi base (Driver.Iper.of_string (Mutil.decode s)) with
      | p -> if Util.is_empty_name p then None else Some p
      | exception _ -> None)

let queue_url conf = (Util.commd conf :> string) ^ "m=QUEUE"

let print_queued conf bfile (item : WriteQueue.item) =
  let title _ =
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl conf "write queue"))
  in
  Hutil.header conf title;
  Output.print_sstring conf {|<div class="alert alert-info mt-3"><p>|};
  let position = Option.value ~default:1 (WriteQueue.position bfile item.id) in
  Output.print_sstring conf
    (Utf8.capitalize_fst
       (Printf.sprintf
          (Util.ftransl conf
             "the base is busy: your modification is queued (%d)")
          position));
  Output.print_sstring conf ".</p><p>";
  Output.print_sstring conf
    (Utf8.capitalize_fst (Util.transl conf "you can leave this page"));
  Output.print_sstring conf ".</p>";
  (match BulkStatus.read bfile with
  | Some st ->
      let tm = Unix.localtime st.started in
      Output.print_sstring conf {|<p class="text-body-secondary">|};
      Output.print_string conf
        (Printf.sprintf
           (Util.ftransl conf
              "a large update started by %s at %s is in progress")
           (Util.escape_html (if st.user = "" then "?" else st.user) :> string)
           (Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min)
        |> Utf8.capitalize_fst |> Adef.safe);
      Output.print_sstring conf ".</p>"
  | None -> ());
  Output.print_sstring conf "</div>";
  Output.printf conf {|<p><a href="%s">%s</a></p>|} (queue_url conf)
    (Utf8.capitalize_fst (Util.transl conf "write queue"));
  Hutil.trailer conf

(* A form posting the request [item] again, in the session of the wizard. *)
let print_resume_form conf (item : WriteQueue.item) =
  Output.printf conf {|<form method="post" action="%s" class="d-inline">|}
    conf.command;
  Util.hidden_env_aux conf conf.henv;
  List.iter
    (fun (k, v) ->
      if k = "notes" || Update.is_label_note k then
        Util.hidden_textarea conf k v
      else Util.hidden_input conf k v)
    item.env;
  Output.printf conf
    {|<button type="submit" class="btn btn-sm btn-outline-primary ms-2">%s</button></form>|}
    (Utf8.capitalize_fst (Util.transl conf "resume"));
  ()

let print_list conf base =
  let bfile = !GWPARAM.bpath conf.bname in
  let title _ =
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl conf "write queue"))
  in
  Hutil.header conf title;
  match WriteQueue.list_user bfile ~user:conf.user with
  | [] ->
      Output.printf conf {|<p class="mt-3">%s.</p>|}
        (Utf8.capitalize_fst (Util.transl conf "no queued modification"));
      Hutil.trailer conf
  | items ->
      Output.print_sstring conf {|<ul class="list-unstyled mt-3">|};
      List.iter
        (fun (item : WriteQueue.item) ->
          Output.printf conf {|<li class="mt-2">%s · %s|}
            (time_label conf item.created)
            (command_label conf item.command);
          (match person_of base item with
          | Some p ->
              Output.print_sstring conf " · ";
              Output.print_string conf (Util.referenced_person_text conf base p)
          | None -> (
              match List.assoc_opt "f" item.env with
              | Some f when f <> Adef.encoded "" ->
                  Output.print_sstring conf " · ";
                  Output.print_string conf
                    (Util.escape_html (Mutil.decode f) :> Adef.safe_string)
              | Some _ | None -> ()));
          Output.printf conf {| · <span class="%s">%s</span>|}
            (state_class item.state)
            (state_label conf item.state);
          (match item.state with
          | Pending -> (
              match WriteQueue.position bfile item.id with
              | Some n -> Output.printf conf " (%d)" n
              | None -> ())
          | Review | Failed -> print_resume_form conf item
          | Running | Applied | Unchanged -> ());
          Output.print_sstring conf "</li>")
        items;
      Output.print_sstring conf "</ul>";
      Hutil.trailer conf

let notify conf =
  if (conf.wizard || conf.friend) && Option.is_none conf.write_log then
    let bfile = !GWPARAM.bpath conf.bname in
    if Sys.file_exists (WriteQueue.dir bfile) then
      List.iter
        (fun (item : WriteQueue.item) ->
          let msg key =
            Printf.sprintf (Util.ftransl conf key)
              (command_label conf item.command)
              (time_label conf item.created)
            |> Utf8.capitalize_fst
          in
          let title = Utf8.capitalize_fst (Util.transl conf "write queue") in
          match item.state with
          | Applied | Unchanged ->
              Notif.success ~title (msg "write queue applied %s %s")
          | Review | Failed ->
              Notif.warning ~mode:Notif.Dismissible ~title
                (msg "write queue to check %s %s")
          | Pending | Running -> ())
        (WriteQueue.take_unseen bfile ~user:conf.user)
