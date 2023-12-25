(* $Id: phonygwd.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

let port_selected = ref 2317
let fname = ref ""

let log addr request s =
  let referer = Wserver.extract_param "referer: " '\n' request in
  let user_agent = Wserver.extract_param "user-agent: " '\n' request in
  let tm = Unix.localtime (Unix.time ()) in
  Printf.eprintf "%02d/%02d/%02d %02d:%02d" tm.Unix.tm_mday
    (succ tm.Unix.tm_mon) tm.Unix.tm_year tm.Unix.tm_hour tm.Unix.tm_min;
  Printf.eprintf " %s\n" s;
  begin match addr with
    Unix.ADDR_UNIX x -> ()
  | Unix.ADDR_INET (iaddr, port) ->
      Printf.eprintf "  From: %s\n"
        (try (Unix.gethostbyaddr iaddr).Unix.h_name with
           _ -> Unix.string_of_inet_addr iaddr)
  end;
  Printf.eprintf "  Agent: %s\n" user_agent;
  if referer <> "" then Printf.eprintf "  Referer: %s\n" referer;
  flush stderr

let print_text fname =
  let ic = Secure.open_in fname in
  (try while true do print_char (input_char ic) done with End_of_file -> ());
  close_in ic;
  Wserver.wflush ()

let connection (addr, request) script_name contents =
  let str = script_name ^ "?" ^ contents in
  log addr request str; Wserver.html ""; print_text !fname; Wserver.wflush ()

let main () =
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [-p #] <file>" in
  let speclist =
    ["-p", Arg.Int (fun x -> port_selected := x), "#: port number"]
  in
  Argl.parse speclist (fun s -> fname := s) usage;
  if !fname = "" then
    begin
      Printf.eprintf "Missing file\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 1
    end;
  close_in (Secure.open_in !fname);
  Wserver.f None !port_selected 0 (Some 4) connection

let _ =
  try main () with
    Unix.Unix_error (err, fun_name, arg) ->
      Printf.eprintf "Error: \"%s\", %s\n" fun_name (Unix.error_message err);
      flush stderr;
      exit 1
  | exc -> Printexc.catch raise exc
