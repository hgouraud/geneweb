(* $Id: check_base.ml,v 5.5 2007-03-19 10:59:31 ddr Exp $ *)

open Printf

let print_error base x =
  printf "\nError: "; Check.print_base_error stdout base x

let print_warning base =
  function
    Def.UndefinedSex _ -> ()
  | x -> printf "\nWarning: "; Check.print_base_warning stdout base x

let set_list l v = l := v :: !l

let check_base bname =
  Secure.set_base_dir (Filename.dirname bname);
  let base = Gwdb.open_base bname in
  let changed_p (ip, p, o_sex, o_rpar) =
    let fn = Gwdb.p_first_name base p in
    let sn = Gwdb.p_surname base p in
    printf "%s.%d %s not changed" fn (Gwdb.get_occ p) sn
  in
  let errors = ref [] in
  let warnings = ref [] in
  Check.check_base base (set_list errors) (set_list warnings) (fun _ -> true)
    changed_p false;
  List.iter (print_error base) (List.rev !errors);
  (* On rend la liste unique, parce qu'il se peut qu'un warning soit *)
  (* levé par plusieurs fonctions différents selon le context.       *)
  let wl =
    let ht = Hashtbl.create 1 in
    let rec loop wl accu =
      match wl with
        [] -> accu
      | x :: wl ->
          if Hashtbl.mem ht (Hashtbl.hash x) then loop wl accu
          else
            begin
              Hashtbl.add ht (Hashtbl.hash x) true;
              loop wl (x :: accu)
            end
    in
    loop !warnings []
  in
  List.iter (print_warning base) wl; flush stdout

let main () = check_base Sys.argv.(1)

let _ = main ()
