(* Per-source log levels for gwd, and per-request timing.

   The level of each [Logs] source can be overridden with a spec of the
   form [SRC=LEVEL,SRC=LEVEL,...], where [SRC] is a source name as shown
   in the log header (GWD, SRCH, GC, BENCH, ...; case-insensitive) or [*]
   for every source, and [LEVEL] is one of quiet, app, error, warning,
   info, debug. Sources not mentioned stay at the global level (debug
   with -debug, info otherwise).

   The spec is read from the file [gwd_log] in the counter directory if
   it exists, otherwise from the environment variable [GWD_LOG]. The
   file is checked at every request, so levels can be changed on a
   running server without restarting it. *)

let bench_src = Logs.Src.create ~doc:"Request timing" "BENCH"
let gc_src = Logs.Src.create ~doc:"GC statistics per request" "GC"

module Bench_log = (val Logs.src_log bench_src : Logs.LOG)
module Gc_log = (val Logs.src_log gc_src : Logs.LOG)

let env_var = "GWD_LOG"
let ctl_file_name = "gwd_log"

let parse_spec spec =
  let items =
    String.split_on_char ',' spec
    |> List.concat_map (String.split_on_char ' ')
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  List.fold_left
    (fun (ok, errors) item ->
      match String.index_opt item '=' with
      | None -> (ok, item :: errors)
      | Some i -> (
          let name = String.trim (String.sub item 0 i) in
          let lvl =
            String.sub item (i + 1) (String.length item - i - 1)
            |> String.trim |> String.lowercase_ascii
          in
          match Logs.level_of_string lvl with
          | Ok level -> ((String.uppercase_ascii name, level) :: ok, errors)
          | Error _ -> (ok, item :: errors)))
    ([], []) items
  |> fun (ok, errors) -> (List.rev ok, List.rev errors)

let source_key src = String.uppercase_ascii (String.trim (Logs.Src.name src))

(* Reset every source to the global level, then apply [rules] in order.
   Returns the rule names that match no source. *)
let apply_rules rules =
  let default = Logs.level () in
  let srcs = Logs.Src.list () in
  List.iter (fun src -> Logs.Src.set_level src default) srcs;
  List.fold_left
    (fun unknown (name, level) ->
      let matching =
        List.filter (fun src -> name = "*" || source_key src = name) srcs
      in
      List.iter (fun src -> Logs.Src.set_level src level) matching;
      if matching = [] then name :: unknown else unknown)
    [] rules
  |> List.rev

let read_ctl_file path =
  match In_channel.with_open_text path In_channel.input_all with
  | s -> Some s
  | exception Sys_error _ -> None

let current_spec ~ctl_dir =
  match read_ctl_file (Filename.concat ctl_dir ctl_file_name) with
  | Some s -> Some s
  | None -> Sys.getenv_opt env_var

(* Called once at startup: reports malformed items and unknown sources. *)
let init ~ctl_dir =
  match current_spec ~ctl_dir with
  | None -> ()
  | Some spec ->
      let rules, errors = parse_spec spec in
      let unknown = apply_rules rules in
      List.iter
        (fun e ->
          Logs.warn (fun k -> k "%s: ignoring malformed item %S" env_var e))
        errors;
      List.iter
        (fun n -> Logs.warn (fun k -> k "%s: no log source named %S" env_var n))
        unknown

(* Called at every request: silent and stateless, so that removing the
   control file restores the environment (or default) levels. *)
let refresh ~ctl_dir =
  let rules =
    match current_spec ~ctl_dir with
    | None -> []
    | Some spec -> fst (parse_spec spec)
  in
  ignore (apply_rules rules : string list)

let enabled src = Logs.Src.level src = Some Logs.Debug

let bench name fn =
  let with_gc = enabled gc_src in
  (* [Gc.stat] walks the heap, which is costly on a large base: only
     pay for it when the GC source is enabled. *)
  let gc1 = if with_gc then Some (Gc.stat ()) else None in
  let p1 = Sys.time () in
  let t1 = Unix.gettimeofday () in
  let res = fn () in
  let t2 = Unix.gettimeofday () in
  let p2 = Sys.time () in
  Bench_log.debug (fun k -> k "[%s]: %fs (~%fs CPU)" name (t2 -. t1) (p2 -. p1));
  Option.iter
    (fun gc1 ->
      let diff = Mutil.gc_stat_diff gc1 (Gc.stat ()) in
      Gc_log.debug (fun k ->
          k "%s" (String.trim (Mutil.string_of_gc_stat diff))))
    gc1;
  res
