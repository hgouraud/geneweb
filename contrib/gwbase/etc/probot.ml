(* camlp4r *)
(* $Id: probot.ml,v 1.7 2006-10-05 13:37:58 deraugla Exp $ *)

open Printf

let magic_robot = "GWRB0007"

module W = Map.Make (struct type t = string let compare = compare end)

type norfriwiz =
    Normal
  | Friend of string
  | Wizard of string

type who =
  { acc_times : float list;
    oldest_time : float;
    nb_connect : int;
    nbase : string;
    utype : norfriwiz }

type excl =
  { mutable excl : (string * int ref) list;
    mutable who : who W.t;
    mutable max_conn : int * string }

let input_excl =
  let b = Bytes.create (String.length magic_robot) in
  fun ic ->
    really_input ic b 0 (String.length b);
    if b <> magic_robot then raise Not_found else (input_value ic : excl)

let main () =
  let xcl = input_excl (open_in_bin "cnt/robot") in
  let tm_now = Unix.time () in
  if xcl.excl <> [] then printf "excl\n";
  List.iter (fun (s, ri) -> printf "  \"%s\" %d\n" s !ri) xcl.excl;
  printf "who\n";
  let sorted_who =
    List.sort
      (fun (k1, w1) (k2, w2) ->
         compare (List.hd w1.acc_times) (List.hd w2.acc_times))
      (W.fold (fun k x l -> (k, x) :: l) xcl.who [])
  in
  List.iter
    (fun (k, w) ->
       let tml = w.acc_times in
       let tm0 = w.oldest_time in
       let cnt = w.nb_connect in
       let bn = w.nbase in
       let nfw = w.utype in
       printf "  %s (%d)" k cnt;
       begin match nfw with
         Wizard n ->
           printf " (\027[31mwiz %s%s\027[30m)" bn
             (if n = "" then "" else " " ^ n)
       | Friend n ->
           printf " (\027[34mfri %s%s\027[30m)" bn
             (if n = "" then "" else " " ^ n)
       | Normal -> ()
       end;
       printf "\n";
       printf "    ";
       let cnt = ref 0 in
       let siz = List.length tml in
       List.iter
         (fun tm ->
            incr cnt;
            if true || !cnt <= 8 || !cnt > siz - 8 then
              printf "-%g" (tm_now -. tm)
            else if !cnt = 9 then printf "-...")
         tml;
       printf "\n")
    sorted_who;
  printf "max_conn\n";
  printf "  %d \"%s\"\n" (fst xcl.max_conn) (snd xcl.max_conn);
  flush stdout

let _ = main ()
