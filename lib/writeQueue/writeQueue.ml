type state = Pending | Running | Applied | Unchanged | Review | Failed

type identity = {
  wizard : bool;
  friend : bool;
  user : string;
  username : string;
  userkey : string;
}

type item = {
  id : int;
  created : float;
  identity : identity;
  command : string;
  from : string;
  lang : string;
  request : string list;
  env : (string * Adef.encoded_string) list;
  state : state;
  finished : float;
}

(* Changed with the type [item]: a file of another version is ignored. *)
let magic = "GWQUEUE1"
let ( // ) = Filename.concat
let dir bfile = Filename.remove_extension bfile ^ ".queue"
let pending_dir bfile = dir bfile // "pending"
let done_dir bfile = dir bfile // "done"

let unseen_dir bfile ~user =
  dir bfile // "unseen" // Digest.to_hex (Digest.string user)

let file_name id = Printf.sprintf "%010d" id

let mkdir d =
  try Filesystem.create_dir ~parent:true ~required_perm:0o700 d
  with Filesystem.File_error _ -> ()

let ids d =
  match Sys.readdir d with
  | exception Sys_error _ -> []
  | a ->
      Array.fold_left
        (fun acc s ->
          if String.length s = 10 then
            match int_of_string_opt s with Some i -> i :: acc | None -> acc
          else acc)
        [] a
      |> List.sort compare

let has_pending bfile = ids (pending_dir bfile) <> []

let read fname =
  match
    Secure.with_open_in_bin fname (fun ic ->
        if really_input_string ic (String.length magic) = magic then
          Some (input_value ic : item)
        else None)
  with
  | x -> x
  | exception (Sys_error _ | End_of_file | Failure _) -> None

let write d (item : item) =
  mkdir d;
  let fname = d // file_name item.id in
  let tmp = fname ^ ".tmp" in
  Secure.with_open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
    0o600 tmp (fun oc ->
      output_string oc magic;
      output_value oc item);
  Sys.rename tmp fname

let remove fname = try Sys.remove fname with Sys_error _ -> ()

let strip_credentials request =
  List.filter
    (fun l ->
      let l = String.lowercase_ascii l in
      not
        (String.starts_with ~prefix:"authorization:" l
        || String.starts_with ~prefix:"cookie:" l))
    request

(* The counter of the requests is guarded by a lock of its own: queuing a
   request never waits for the lock of the base. *)
let with_seq_lock bfile k =
  mkdir (dir bfile);
  Lock.control
    ~on_exn:(fun exn _bt -> raise exn)
    ~wait:true
    ~lock_file:(dir bfile // "seq.lck")
    k

let next_id bfile =
  let fname = dir bfile // "seq" in
  let n =
    match Secure.with_open_in_text fname In_channel.input_line with
    | Some s -> Option.value ~default:0 (int_of_string_opt (String.trim s))
    | None -> 0
    | exception Sys_error _ -> 0
  in
  let n = n + 1 in
  let tmp = fname ^ ".tmp" in
  Secure.with_open_out_text tmp (fun oc -> Printf.fprintf oc "%d\n" n);
  Sys.rename tmp fname;
  n

let pending_items bfile =
  let d = pending_dir bfile in
  List.filter_map (fun id -> read (d // file_name id)) (ids d)

let enqueue bfile ~identity ~command ~from ~lang ~request ~env =
  with_seq_lock bfile @@ fun () ->
  let same (i : item) =
    i.state = Pending
    && i.identity.user = identity.user
    && i.command = command && i.env = env
  in
  match List.find_opt same (pending_items bfile) with
  | Some i -> i
  | None ->
      let item =
        {
          id = next_id bfile;
          created = Unix.time ();
          identity;
          command;
          from;
          lang;
          request = strip_credentials request;
          env;
          state = Pending;
          finished = 0.;
        }
      in
      write (pending_dir bfile) item;
      item

let position bfile id =
  let rec loop n l =
    match l with
    | [] -> None
    | x :: l -> if x = id then Some n else loop (n + 1) l
  in
  loop 1 (ids (pending_dir bfile))

let finish bfile (item : item) state =
  write (done_dir bfile) { item with state; finished = Unix.time () };
  remove (pending_dir bfile // file_name item.id);
  let d = unseen_dir bfile ~user:item.identity.user in
  mkdir d;
  Secure.with_open_out_bin (d // file_name item.id) ignore

let rec next bfile =
  match ids (pending_dir bfile) with
  | [] -> None
  | id :: _ -> (
      let fname = pending_dir bfile // file_name id in
      match read fname with
      | None ->
          (* unreadable: written by another version, or truncated *)
          Sys.rename fname (fname ^ ".bad");
          next bfile
      | Some _ when Sys.file_exists (done_dir bfile // file_name id) ->
          (* finished, the crash happening before its removal *)
          remove fname;
          next bfile
      | Some item -> (
          match item.state with
          | Running ->
              finish bfile item Review;
              next bfile
          | Pending | Applied | Unchanged | Review | Failed ->
              let item = { item with state = Running } in
              write (pending_dir bfile) item;
              Some item))

let find bfile id =
  match read (pending_dir bfile // file_name id) with
  | Some _ as x -> x
  | None -> read (done_dir bfile // file_name id)

let list_user bfile ~user =
  let items d =
    List.filter_map
      (fun id ->
        match read (d // file_name id) with
        | Some i when i.identity.user = user -> Some i
        | Some _ | None -> None)
      (ids d)
  in
  List.rev_append (items (done_dir bfile)) (items (pending_dir bfile))
  |> List.sort (fun (a : item) b -> compare b.id a.id)

let take_unseen bfile ~user =
  let d = unseen_dir bfile ~user in
  List.filter_map
    (fun id ->
      remove (d // file_name id);
      read (done_dir bfile // file_name id))
    (ids d)

let purge bfile ~max_age =
  let now = Unix.time () in
  let d = done_dir bfile in
  List.iter
    (fun id ->
      match read (d // file_name id) with
      | Some i when now -. i.finished > max_age ->
          remove (unseen_dir bfile ~user:i.identity.user // file_name id);
          remove (d // file_name id)
      | Some _ | None -> ())
    (ids d)
