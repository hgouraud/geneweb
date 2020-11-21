(* $Id: sendImage.ml,v 5.7 2019-03-12 09:58:44 ddr Exp $ *)

open Geneweb
open Config
open Def
open Gwdb
open Util
open TemplAst

(* Mutil ************************************ *)
let rn fname s =
  try if Sys.file_exists fname then Sys.rename fname s
  with Failure _ ->
    begin
      Printf.eprintf "Rn failed: %s to %s\n" fname s; flush stderr
    end

(* Util ************************************ *)
let keydir conf base p =
  let k = default_image_name base p in
  let f = String.concat Filename.dir_sep
    [(base_path ["src"] conf.bname); "images"; k]
  in
  try if Sys.is_directory f then Some f else None
  with Sys_error _ -> None

let keydir_old conf base p =
  let k = default_image_name base p in
  let f = String.concat Filename.dir_sep
    [(base_path ["src"] conf.bname); "images"; k ; "saved" ]
  in
  try if Sys.is_directory f then Some f else None
  with Sys_error _ -> None

let get_keydir_img_notes conf base p fname =
  let k = default_image_name base p in
  let fname =
    String.concat Filename.dir_sep
      [(base_path ["src"] conf.bname); "images"; k ; fname ^ ".txt" ]
  in
  let s = if Sys.file_exists fname then
    let ic = Secure.open_in fname in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic; s
    else ""
  in s

let out_keydir_img_notes conf base p fname s =
  let k = default_image_name base p in
  let fname =
    String.concat Filename.dir_sep
      [(base_path ["src"] conf.bname); "images";  k ; fname ^ ".txt" ]
  in
  try
    let oc = Secure.open_out fname in
    output_string oc s;
    close_out oc;
  with Sys_error _ -> ()

let get_keydir_old conf base p =
  match keydir_old conf base p with
    Some f ->
      Array.fold_right (fun f1 l ->
        if f1.[0] <> '.' && Filename.extension f1 <> ".txt" &&
          ( Filename.extension f1 = ".jpg" ||
            Filename.extension f1 = ".gif" ||
            Filename.extension f1 = ".png" )
        then
          (* vérifier ici le type des images autorisées  *)
          ( f1 :: l ) else l)
          (Sys.readdir f) []
  | None -> []

let get_keydir conf base p =
  match keydir conf base p with
    Some f ->
      Array.fold_right (fun f1 l ->
        if f1.[0] <> '.' && Filename.extension f1 <> ".txt" &&
          ( Filename.extension f1 = ".jpg" ||
            Filename.extension f1 = ".gif" ||
            Filename.extension f1 = ".png" )
        then
          (* vérifier ici le type des images autorisées  *)
          ( f1 :: l ) else l)
          (Sys.readdir f) []
  | None -> []

let has_keydir conf base p =
  if not conf.no_image && authorized_age conf base p then
    keydir conf base p <> None
  else false

let auto_image_file ?bak:(b=false) conf base p =
  let s = default_image_name base p in
  let dir =
      if b then Filename.concat (base_path ["images"] conf.bname) "saved"
      else (base_path ["images"] conf.bname)
  in
  let f = Filename.concat dir s in
  if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".png") then Some (f ^ ".png")
  else None

(* ************************************ *)

(* Type pour ne pas créer à chaque fois un tableau tstab et mark *)
type sosa_t =
  { tstab : (iper, int) Gwdb.Marker.t;
    mark : (iper, bool) Gwdb.Marker.t;
    mutable last_zil : (iper * Sosa.t) list;
    sosa_ht : (iper, (Sosa.t * Gwdb.person) option) Hashtbl.t }

type generation_person =
    GP_person of Sosa.t * iper * ifam option
  | GP_same of Sosa.t * Sosa.t * iper
  | GP_interv of (Sosa.t * Sosa.t * (Sosa.t * Sosa.t) option) option
  | GP_missing of Sosa.t * iper

type pos = Left | Right | Center | Alone
type cell =
    Cell of person * ifam option * pos * bool * int * string
  | Empty

(* Interpretation of template file *)

let rec compare_ls sl1 sl2 =
  match sl1, sl2 with
    s1 :: sl1, s2 :: sl2 ->
      (* Je ne sais pas s'il y a des effets de bords, mais on  *)
      (* essaie de convertir s1 s2 en int pour éviter que "10" *)
      (* soit plus petit que "2". J'espère qu'on ne casse pas  *)
      (* les performances à cause du try..with.                *)
      let c =
        try Stdlib.compare (int_of_string s1) (int_of_string s2) with
          Failure _ -> Gutil.alphabetic_order s1 s2
      in
      if c = 0 then compare_ls sl1 sl2 else c
  | _ :: _, [] -> 1
  | [], _ :: _ -> -1
  | [], [] -> 0

module SortedList =
  Set.Make (struct type t = string list let compare = compare_ls end)

(*
   Type pour représenté soit :
     - la liste des branches patronymique
       (surname * date begin * date end * place * person * list sosa * loc)
     - la liste éclair
       (surname * place * date begin * date end * person * list iper * loc)
*)
type ancestor_surname_info =
    Branch of
      (string * date option * date option * string * person * Sosa.t list *
         loc)
  | Eclair of
      (string * string * date option * date option * person * iper list * loc)

type 'a env =
    Vallgp of generation_person list
  | Vanc of generation_person
  | Vanc_surn of ancestor_surname_info
  | Vcell of cell
  | Vcelll of cell list
  | Vcnt of int ref
  | Vdesclevtab of ((iper, int) Marker.t * (ifam, int) Marker.t) lazy_t
  | Vdmark of (iper, bool) Marker.t ref
  | Vslist of SortedList.t ref
  | Vslistlm of string list list
  | Vind of person
  | Vfam of ifam * family * (iper * iper * iper) * bool
  | Vrel of relation * person option
  | Vbool of bool
  | Vint of int
  | Vgpl of generation_person list
  | Vnldb of (Gwdb.iper, Gwdb.ifam) Def.NLDB.t
  | Vstring of string
  | Vsosa_ref of person option Lazy.t
  | Vsosa of (iper * (Sosa.t * person) option) list ref
  | Vt_sosa of sosa_t option
  | Vtitle of person * title_item
  | Vevent of person * event_item
  | Vlazyp of string option ref
  | Vlazy of 'a env Lazy.t
  | Vother of 'a
  | Vnone
and title_item =
  int * istr gen_title_name * istr * istr list *
    (date option * date option) list
and event_item =
  event_name * cdate * istr * istr * istr * (iper * witness_kind) array *
    iper option
and event_name =
    Pevent of istr gen_pers_event_name
  | Fevent of istr gen_fam_event_name

let get_env v env =
  try
    match List.assoc v env with
      Vlazy l -> Lazy.force l
    | x -> x
  with Not_found -> Vnone

let get_vother =
  function
    Vother x -> Some x
  | _ -> None

let set_vother x = Vother x

let extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""

let bool_val x = VVbool x
let str_val x = VVstring x

let make_ep conf base ip =
  let p = pget conf base ip in
  let p_auth = authorized_age conf base p in p, p_auth

let make_efam conf base ip ifam =
  let fam = foi base ifam in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let ispouse = if ip = ifath then imoth else ifath in
  let cpl = ifath, imoth, ispouse in
  let m_auth =
    authorized_age conf base (pget conf base ifath) &&
    authorized_age conf base (pget conf base imoth)
  in
  fam, cpl, m_auth

let mode_local env =
  match get_env "fam_link" env with
  | Vfam _ -> false
  | _ -> true

(*
  | ["evar_cur"; v; i] ->
      let n = int_of_string i in
      let rec loop n =
        match Util.p_getenv (conf.env @ conf.henv) (v ^ string_of_int n) with
        | Some vv -> vv
        | None -> if n > 0 then loop (n - 1) else ""
      in VVstring (loop n)
  | ["substr_start"; n; v] ->
          let n = int_of_string n in
          (* Attention aux caractères utf-8 !! *)
          let sub =
            let len = String.length v in
            let rec loop i n str =
              if n = 0 || i >= len then str
              else
                let nbc = Utf8.nbc v.[i] in
                let car = String.sub v i nbc in
                loop (i+nbc) (n-1) (str ^ car)
            in
            loop 0 n ""
          in VVstring sub
  | [s] -> eval_simple_var conf s
  | _ -> raise Not_found

*)

let rec eval_var conf base env ep loc sl =
  try eval_simple_var conf base env ep sl with
    Not_found -> eval_compound_var conf base env ep loc sl
and eval_simple_var conf base env (p, p_auth as ep) =
  function
  | [s] ->
      begin try bool_val (eval_simple_bool_var conf base env s) with
        Not_found -> str_val (eval_simple_str_var conf base env ep s)
      end
  | _ -> raise Not_found
and eval_simple_bool_var conf base env =
  function
  | "has_old_image" ->
      begin match find_person_in_env conf base "" with
      | Some p ->
          begin match auto_image_file ~bak:true conf base p with
            Some _s -> true
          | _ -> false
          end
      | _ -> false
      end
  | "has_keydir" ->
      begin match find_person_in_env conf base "" with
      | Some p ->
          begin match auto_image_file ~bak:true conf base p with
            Some _s -> has_keydir conf base p
          | _ -> false
          end
      | _ -> false
      end
  | s ->
      let v = extract_var "file_exists_" s in
      if v <> "" then
        let v = code_varenv v in
        let s = SrcfileDisplay.source_file_name conf v in Sys.file_exists s
      else raise Not_found
and eval_simple_str_var conf base env (p, p_auth) =
  function
  | "count" ->
      begin match get_env "count" env with
        Vcnt c -> string_of_int !c
      | _ -> ""
      end
  | "idigest" -> default_image_name base p
  | "incr_count" ->
      begin match get_env "count" env with
        Vcnt c -> incr c; ""
      | _ -> ""
      end
  | "keydir_img" ->
      begin match get_env "keydir_img" env with
        Vstring s -> s
      | _ -> ""
      end
  | "keydir_img_key" ->
      begin match get_env "keydir_img" env with
        Vstring s -> (Mutil.tr '+' ' ' (code_varenv s))
      | _ -> ""
      end
  | "keydir_img_old" ->
      begin match get_env "keydir_img_old" env with
        Vstring s -> s
      | _ -> ""
      end
  | "keydir_img_old_key" ->
      begin match get_env "keydir_img_old" env with
        Vstring s -> (Mutil.tr '+' ' ' (code_varenv s))
      | _ -> ""
      end
  | "keydir_notes" ->
      begin match get_env "keydir_img" env with
        Vstring s -> (Filename.remove_extension s) ^ ".txt"
      | _ -> ""
      end
  | "reset_count" ->
      begin match get_env "count" env with
        Vcnt c -> c := 0; ""
      | _ -> ""
      end
  | "X" -> Filename.dir_sep
  | s ->
      let rec loop =
        function
          (pfx, f) :: pfx_list ->
            let v = extract_var pfx s in
            if v <> "" then f v else loop pfx_list
        | [] -> raise Not_found
      in
      loop
        ["evar_",
         (fun v ->
            match p_getenv (conf.env @ conf.henv) v with
              Some vv -> Util.escape_html vv
            | None -> "");
         (* warning: "cvar_" deprecated since 5.00; use "bvar." *)
         "cvar_",
         (fun v -> try List.assoc v conf.base_env with Not_found -> "")]
and eval_compound_var conf base env (a, _ as ep) loc =
  function
  | ["evar_cur"; v; i] ->
      let n = int_of_string i in
      let rec loop n =
        match Util.p_getenv (conf.env @ conf.henv) (v ^ string_of_int n) with
        | Some vv -> vv
        | None -> if n > 0 then loop (n - 1) else ""
      in VVstring (loop n)
  | ["substr_start"; n; v] ->
          let n = int_of_string n in
          (* Attention aux caractères utf-8 !! *)
          let sub =
            let len = String.length v in
            let rec loop i n str =
              if n = 0 || i >= len then str
              else
                let nbc = Utf8.nbc v.[i] in
                let car = String.sub v i nbc in
                loop (i+nbc) (n-1) (str ^ car)
            in
            loop 0 n ""
          in VVstring sub
  | ["base"; "name"] -> VVstring conf.bname
  | "family" :: sl ->
      (* TODO ???
      let mode_local =
        match get_env "fam_link" env with
        [ Vfam ifam _ (_, _, ip) _ -> False
        | _ -> True ]
      in *)
      begin match get_env "fam" env with
        Vfam (i, f, c, m) ->
          eval_family_field_var conf base env (i, f, c, m) loc sl
      | _ ->
          match get_env "fam_link" env with
            Vfam (i, f, c, m) ->
              eval_family_field_var conf base env (i, f, c, m) loc sl
          | _ -> raise Not_found
      end
  | "spouse" :: sl ->
      begin match get_env "fam" env with
        Vfam (_, _, (_, _, ip), _) when mode_local env ->
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ ->
        raise Not_found
      end
  | sl -> eval_person_field_var conf base env ep loc sl
and eval_item_field_var ell =
  function
    [s] ->
      begin try
        match ell with
          el :: _ ->
            let v = int_of_string s in
            let r = try List.nth el (v - 1) with Failure _ -> "" in VVstring r
        | [] -> VVstring ""
      with Failure _ -> raise Not_found
      end
  | _ -> raise Not_found
and eval_num conf n =
  function
    ["hexa"] -> Printf.sprintf "0x%X" @@ int_of_string (Sosa.to_string n)
  | ["octal"] -> Printf.sprintf "0x%o" @@ int_of_string (Sosa.to_string n)
  | ["lvl"] -> string_of_int @@ Sosa.gen n
  | ["v"] -> Sosa.to_string n
  | [] -> Sosa.to_string_sep (transl conf "(thousand separator)") n
  | _ -> raise Not_found
and eval_person_field_var conf base env (p, p_auth as ep) loc =
  function
  | "spouse" :: sl ->
      begin match get_env "fam" env with
        Vfam (ifam, _, _, _) ->
          let cpl = foi base ifam in
          let ip = Gutil.spouse (get_iper p) cpl in
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found
      end
  | [s] ->
      begin try bool_val (eval_bool_person_field conf base env ep s) with
        Not_found ->
          begin try str_val (eval_str_person_field conf base env ep s) with
            Not_found -> raise Not_found
          end
      end
  | [] -> str_val (simple_person_text conf base p p_auth)
  | _ -> raise Not_found
and eval_bool_person_field conf base env (p, p_auth) =
  function
  | "has_image" -> Util.has_image conf base p
  | "has_keydir" -> has_keydir conf base p
  | "is_female" -> get_sex p = Female
  | "is_male" -> get_sex p = Male
  | _ -> raise Not_found
and eval_str_person_field conf base env (p, p_auth as _ep) =
  function
  | "access" -> acces conf base p
  | "auto_image_file_name" | "portrait" ->
      begin match auto_image_file conf base p with
        Some s when p_auth -> s
      | _ -> ""
      end
  | "portrait_base" ->
      begin match auto_image_file conf base p with
        Some s when p_auth -> Filename.basename s
      | _ -> ""
      end
  | "portrait_saved" ->
      begin match auto_image_file ~bak:true conf base p with
        Some s -> Filename.basename s
      | _ -> ""
      end
  | "first_name" ->
      if not p_auth && is_hide_names conf p then "x" else p_first_name base p
  | "first_name_key" ->
      if is_hide_names conf p && not p_auth then ""
      else code_varenv (Name.lower (p_first_name base p))
  | "first_name_key_val" ->
      if is_hide_names conf p && not p_auth then ""
      else Name.lower (p_first_name base p)
  | "first_name_key_strip" ->
      if is_hide_names conf p && not p_auth then ""
      else Name.strip_c (p_first_name base p) '"'
  | "index" ->
      begin match get_env "p_link" env with
        Vbool _ -> ""
      | _ -> string_of_iper (get_iper p)
      end
  | "keydir" -> default_image_name base p
  | "keydir_img_nbr" ->
    string_of_int (List.length (get_keydir conf base p))
  | "keydir_old_img_nbr" ->
      string_of_int
        (List.length (get_keydir_old conf base p))
  | "keydir_img_notes" ->
      begin match get_env "keydir_img" env with
        Vstring f ->
          let ext = Filename.extension f in
          let fname = Filename.chop_suffix f ext in
          get_keydir_img_notes conf base p fname
      | _ -> raise Not_found
      end
  | "keydir_img_src" ->
      begin match get_env "keydir_img" env with
        Vstring f ->
          begin
            let ext = Filename.extension f in
            let fname = Filename.chop_suffix f ext in
            let n = get_keydir_img_notes conf base p fname in
            match (String.index_opt n '\n') with
              Some i ->
                let s1 = if (String.length n) > i then (String.sub n (i + 1)
                  (String.length n - i - 1)) else ""
                in
                begin
                  match (String.index_opt s1 '\n') with
                    Some j -> String.sub s1 0 j
                  | None -> ""
                end
            | None -> ""
          end
      | _ -> raise Not_found
      end
  | "keydir_img_title" ->
      begin match get_env "keydir_img" env with
        Vstring f ->
          begin
            let ext = Filename.extension f in
            let fname = Filename.chop_suffix f ext in
            let n = get_keydir_img_notes conf base p fname in
            match (String.index_opt n '\n') with
              Some i ->
                String.sub n 0 i
            | None -> ""
          end
      | _ -> raise Not_found
      end
  | "occ" ->
      if is_hide_names conf p && not p_auth then ""
      else string_of_int (get_occ p)
  | "sex" ->
      (* Pour éviter les traductions bizarre, on ne teste pas p_auth. *)
      string_of_int (index_of_sex (get_sex p))
  | "surname" ->
      if not p_auth && is_hide_names conf p then "x" else p_surname base p
  | "surname_begin" ->
      if not p_auth && is_hide_names conf p then ""
      else surname_particle base (p_surname base p)
  | "surname_end" ->
      if not p_auth && is_hide_names conf p then "x"
      else surname_without_particle base (p_surname base p)
  | "surname_key" ->
      if is_hide_names conf p && not p_auth then ""
      else code_varenv (Name.lower (p_surname base p))
  | "surname_key_val" ->
      if is_hide_names conf p && not p_auth then ""
      else Name.lower (p_surname base p)
  | "surname_key_strip" ->
      if is_hide_names conf p && not p_auth then ""
      else Name.strip_c (p_surname base p) '"'
  | _ -> raise Not_found
and eval_family_field_var conf base env
    (_, fam, (ifath, imoth, _), m_auth as fcd) loc =
  function
  | [s] -> str_val (eval_str_family_field env fcd s)
  | _ -> raise Not_found
and eval_str_family_field env (ifam, _, _, _) =
  function
  | "index" -> string_of_ifam ifam
  | _ -> raise Not_found
and simple_person_text conf base p p_auth =
  if p_auth then
    match main_title conf base p with
      Some t -> titled_person_text conf base p t
    | None -> person_text conf base p
  else if is_hide_names conf p then "x x"
  else person_text conf base p
and string_of_int_env var env =
  match get_env var env with
    Vint x -> string_of_int x
  | _ -> raise Not_found

let eval_transl conf base env upp s c =
  match c with
    "n" | "s" | "w" | "f" | "c" ->
      let n =
        match c with
          "n" ->
            (* replaced by %apply;nth([...],sex) *)
            begin match get_env "p" env with
              Vind p -> 1 - index_of_sex (get_sex p)
            | _ -> 2
            end
        | "s" ->
            begin match get_env "child" env with
              Vind p -> index_of_sex (get_sex p)
            | _ ->
                match get_env "p" env with
                  Vind p -> index_of_sex (get_sex p)
                | _ -> 2
            end
        | "w" ->
            begin match get_env "fam" env with
              Vfam (_, fam, _, _) ->
                if Array.length (get_witnesses fam) <= 1 then 0 else 1
            | _ -> 0
            end
        | "f" ->
            begin match get_env "p" env with
              Vind p -> if Array.length (get_family p) <= 1 then 0 else 1
            | _ -> 0
            end
        | "c" ->
            begin match get_env "fam" env with
              Vfam (_, fam, _, _) ->
                if Array.length (get_children fam) <= 1 then 0 else 1
            | _ ->
                match get_env "p" env with
                  Vind p ->
                    let n =
                      Array.fold_left
                        (fun n ifam ->
                           n + Array.length (get_children (foi base ifam)))
                        0 (get_family p)
                    in
                    if n <= 1 then 0 else 1
                | _ -> 0
            end
        | _ -> assert false
      in
      let r = Util.translate_eval (Util.transl_nth conf s n) in
      if upp then Utf8.capitalize r else r
  | _ -> Templ.eval_transl conf upp s c

let print_foreach conf base print_ast eval_expr =
  let rec print_foreach env ini_ep loc s sl ell al =
    let rec loop env (a, _ as ep) efam =
      function
        [s] -> print_simple_foreach env ell al ini_ep ep efam loc s
      | "self" :: sl -> loop env ep efam sl
      | "spouse" :: sl ->
          begin match efam with
            Vfam (_, _, (_, _, ip), _) ->
              let ep = make_ep conf base ip in loop env ep efam sl
          | _ ->
              raise Not_found
          end
      | _ -> raise Not_found
    in
    let efam =
      match get_env "is_link" env with
        Vbool _ -> get_env "fam_link" env
      | _ -> get_env "fam" env
    in
    loop env ini_ep efam (s :: sl)
  and print_simple_foreach env el al ini_ep ep efam loc =
    function
    | "family" -> print_foreach_family env al ini_ep ep
    | "img_in_keydir" -> print_foreach_img_in_keydir env al ep
    | "img_in_keydir_old" ->
        begin match ep with
        | (p, _) ->
            print_foreach_img_in_keydir_old env ep al
              (get_keydir_old conf base p)
        end
    | _ -> raise Not_found
  and print_foreach_family env al ini_ep (p, _) =
    match get_env "p_link" env with
      Vbool _ ->
        ()
    | _ ->
        if Array.length (get_family p) > 0 then
          begin let rec loop prev i =
            if i = Array.length (get_family p) then ()
            else
              let ifam = (get_family p).(i) in
              let fam = foi base ifam in
              let ifath = get_father fam in
              let imoth = get_mother fam in
              let ispouse = Gutil.spouse (get_iper p) fam in
              let cpl = ifath, imoth, ispouse in
              let m_auth =
                authorized_age conf base (pget conf base ifath) &&
                authorized_age conf base (pget conf base imoth)
              in
              let vfam = Vfam (ifam, fam, cpl, m_auth) in
              let env = ("#loop", Vint 0) :: env in
              let env = ("fam", vfam) :: env in
              let env = ("family_cnt", Vint (i + 1)) :: env in
              let env =
                match prev with
                  Some vfam -> ("prev_fam", vfam) :: env
                | None -> env
              in
              List.iter (print_ast env ini_ep) al; loop (Some vfam) (i + 1)
          in
            loop None 0
          end;
        ()
  and print_foreach_img_in_keydir env al (p, p_auth as ep) =
    if not p_auth && is_hide_names conf p then ()
    else
      let list = get_keydir conf base p in
      let rec loop first cnt =
        function
          a :: l ->
          let env =
            ("keydir_img", Vstring a) ::
            ("first", Vbool first) ::
            ("last", Vbool (l = [])) ::
            ("cnt", Vint cnt) :: env
          in
            List.iter (print_ast env ep) al; loop false (cnt + 1) l
        | [] -> ()
      in
      loop true 1 list
  and print_foreach_img_in_keydir_old env (p, p_auth as ep) al list =
    let rec loop cnt =
      function
        a :: l ->
          let env =
            ("keydir_img_old", Vstring a) ::
            ("cnt", Vint cnt) :: env
          in
          List.iter (print_ast env ep) al; loop (cnt + 1) l
      | [] -> ()
    in
    loop 1 list
  in
  print_foreach

let eval_predefined_apply conf env f vl =
  let vl =
    List.map
      (function
         VVstring s -> s
       | _ -> raise Not_found)
      vl
  in
  match f, vl with
    "a_of_b", [s1; s2] -> Util.translate_eval (transl_a_of_b conf s1 s2 s2)
  | "a_of_b2", [s1; s2; s3] -> Util.translate_eval (transl_a_of_b conf s1 s2 s3)
  | "a_of_b_gr_eq_lev", [s1; s2] ->
      Util.translate_eval (transl_a_of_gr_eq_gen_lev conf s1 s2 s2)
  | "add_in_sorted_list", sl ->
      begin match get_env "list" env with
        Vslist l -> l := SortedList.add sl !l; ""
      | _ -> raise Not_found
      end
  | "add_in_sorted_listb", sl ->
      begin match get_env "listb" env with
      | Vslist l -> l := SortedList.add sl !l; ""
      | _ -> raise Not_found
      end
  | "add_in_sorted_listc", sl ->
      begin match get_env "listc" env with
      | Vslist l -> l := SortedList.add sl !l; ""
      | _ -> raise Not_found
      end
  | "hexa", [s] -> Util.hexa_string s
  | "initial", [s] ->
      if String.length s = 0 then ""
      else String.sub s 0 (Utf8.next s 0)
  | "lazy_print", [v] ->
      begin match get_env "lazy_print" env with
        Vlazyp r -> r := Some v; ""
      | _ -> raise Not_found
      end
  | "min", s :: sl ->
      begin try
        let m =
          List.fold_right (fun s -> min (int_of_string s)) sl
            (int_of_string s)
        in
        string_of_int m
      with Failure _ -> raise Not_found
      end
  | "clean_html_tags", [s] ->
      (* On supprime surtout les balises qui peuvent casser la mise en page. *)
      Util.clean_html_tags s
        ["<br */?>"; "</?p>"; "</?div>"; "</?span>"; "</?pre>"]
  | _ -> raise Not_found

(* ********************************** *)

type image_type = JPEG | GIF | PNG

let image_types = [JPEG; GIF; PNG]

let extension_of_type =
  function
    JPEG -> ".jpg"
  | GIF -> ".gif"
  | PNG -> ".png"

let incorrect conf = Hutil.incorrect_request conf; raise @@ Update.ModErr __LOC__

let incorrect_content_type conf base p s =
  let title _ = Wserver.print_string (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<p>\n<em style=\"font-size:smaller\">";
  Wserver.printf "Error: incorrect image content type: %s" s;
  Wserver.printf "</em>\n</p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (referenced_person_title_text conf base p);
  Hutil.trailer conf;
  raise @@ Update.ModErr __LOC__

let error_too_big_image conf base p len max_len =
  let title _ = Wserver.print_string (Utf8.capitalize (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<p><em style=\"font-size:smaller\">";
  Wserver.printf "Error: this image is too big: %d bytes<br%s>\n" len
    conf.xhs;
  Wserver.printf "Maximum authorized in this database: %d bytes<br%s>\n"
    max_len conf.xhs;
  Wserver.printf "</em></p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (referenced_person_title_text conf base p);
  Hutil.trailer conf;
  raise @@ Update.ModErr __LOC__

let raw_get conf key =
  try List.assoc key conf.env with Not_found -> incorrect conf

(* Send image form validated *)

let print_sent conf base p =
  let title _ =
    Wserver.print_string (Utf8.capitalize (transl conf "image received"))
  in
  Hutil.header conf title;
  Wserver.printf "<ul><li>\n%s\n</li></ul>\n"
    (referenced_person_text conf base p);
  Hutil.trailer conf

let write_file fname content =
  let oc = Secure.open_out_bin fname in
  output_string oc content; flush oc; close_out oc

let move_file_to_old dir file  =
  try
    begin
      let save_dir = Filename.concat dir "saved" in
      let fname = Filename.basename file in
      if not (Sys.file_exists save_dir) then Mutil.mkdir_p save_dir;
      let orig_file = Filename.concat dir fname in
      let saved_file = Filename.concat save_dir fname in
      (* TODO handle rn errors *)
      rn orig_file saved_file;
      let orig_file_t = (Filename.remove_extension orig_file) ^ ".txt" in
      let saved_file_t = (Filename.remove_extension saved_file) ^ ".txt" in
      if Sys.file_exists orig_file_t then
        rn orig_file_t saved_file_t;
      1
    end
  with _ -> 0

let normal_image_type s =
  if String.length s > 10 && Char.code s.[0] = 0xff && Char.code s.[1] = 0xd8
  then
    Some JPEG
  else if String.length s > 4 && String.sub s 0 4 = "\137PNG" then Some PNG
  else if String.length s > 4 && String.sub s 0 4 = "GIF8" then Some GIF
  else None

let string_search s v =
  let rec loop i j =
    if j = String.length v then Some (i - String.length v)
    else if i = String.length s then None
    else if s.[i] = v.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

(* get the image type, possibly removing spurious header *)

let image_type s =
  match normal_image_type s with
    Some t -> Some (t, s)
  | None ->
      match string_search s "JFIF" with
        Some i when i > 6 ->
          let s = String.sub s (i - 6) (String.length s - i + 6) in
          Some (JPEG, s)
      | _ ->
          match string_search s "\137PNG" with
            Some i ->
              let s = String.sub s i (String.length s - i) in Some (PNG, s)
          | _ ->
              match string_search s "GIF8" with
                Some i ->
                  let s = String.sub s i (String.length s - i) in
                  Some (GIF, s)
              | None -> None

let dump_bad_image conf s =
  match p_getenv conf.base_env "dump_bad_images" with
    Some "yes" ->
      begin try
        let oc = Secure.open_out_bin "bad-image" in
        output_string oc s; flush oc; close_out oc
      with Sys_error _ -> ()
      end
  | _ -> ()

let effective_send_ok conf base p file =
  let strm = Stream.of_string file in
  let (request, content) = Wserver.get_request_and_content strm in
  let content =
    let s =
      let rec loop len (strm__ : _ Stream.t) =
        match Stream.peek strm__ with
          Some x -> Stream.junk strm__; loop (Buff.store len x) strm
        | _ -> Buff.get len
      in
      loop 0 strm
    in
    content ^ s
  in
  let (typ, content) =
    match image_type content with
      None ->
        let ct = Wserver.extract_param "content-type: " '\n' request in
        dump_bad_image conf content; incorrect_content_type conf base p ct
    | Some (typ, content) ->
        match p_getint conf.base_env "max_images_size" with
          Some len when String.length content > len ->
            error_too_big_image conf base p (String.length content) len
        | _ -> typ, content
  in
  let bfname = default_image_name base p in
  let bfdir =
    let bfdir = Util.base_path ["images"] conf.bname in
    if Sys.file_exists bfdir then bfdir
    else
      let d = Filename.concat (Secure.base_dir ()) "images" in
      let d1 = Filename.concat d conf.bname in
      (try Unix.mkdir d 0o777 with Unix.Unix_error (_, _, _) -> ());
      (try Unix.mkdir d1 0o777 with Unix.Unix_error (_, _, _) -> ());
      d1
  in
  let fname = bfname ^ extension_of_type typ in
  let _moved = move_file_to_old bfdir bfname in
  write_file (Filename.concat bfdir fname) content;
  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed "si";
  print_sent conf base p

let print_send_ok conf base =
  let ip =
    let s = raw_get conf "i" in
    try iper_of_string s with Failure _ -> incorrect conf
  in
  let p = poi base ip in
  let digest = Update.digest_person (UpdateInd.string_person_of base p) in
  if digest = raw_get conf "digest" then
    let file = raw_get conf "file" in effective_send_ok conf base p file
  else Update.error_digest conf

(* Delete image form validated *)

let print_deleted conf base p =
  let title _ =
    Wserver.print_string (Utf8.capitalize (transl conf "image deleted"))
  in
  Hutil.header conf title;
  Wserver.printf "<ul><li>%s</li></ul>" (referenced_person_text conf base p);
  Hutil.trailer conf

let effective_delete_ok conf base p =
  let file =
    match auto_image_file conf base p with
    | Some f -> f
    | None -> incorrect conf;
  in
  let dir = Util.base_path ["images"] conf.bname in
  if (move_file_to_old dir file = 0) then incorrect conf;
  let changed =
    U_Delete_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed "di"; print_deleted conf base p

let print_del_ok conf base =
  match p_getenv conf.env "i" with
    Some ip ->
    let p = poi base (iper_of_string ip) in
    effective_delete_ok conf base p
  | None -> incorrect conf

(* ************************************************************************ *)
(* Code for carousel                                                        *)

let clean_old_portrait dir bfname =
  let file = Filename.remove_extension
    (String.concat Filename.dir_sep [dir; bfname])
  in
  Mutil.rm (file ^ ".jpg") ;
  Mutil.rm (file ^ ".png") ;
  Mutil.rm (file ^ ".gif")

let space_to_unders = Mutil.tr ' ' '_'

let get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get_extension conf keydir =
  let f = String.concat
      Filename.dir_sep
        [(Util.base_path ["images"] conf.bname); keydir]
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else "."

let get_extension_old conf keydir =
  let f =
    String.concat
      Filename.dir_sep
        [(Util.base_path ["images"] conf.bname); "saved"; keydir]
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else "."

let print_confirm_c conf base save_t report =
  match p_getint conf.env "i" with
  | Some ip ->
      let p = poi base (Gwdb.iper_of_string (string_of_int ip)) in
      let ep = p, authorized_age conf base p in
      let digest = default_image_name base p in
      let new_env =
        List.fold_left
          (fun accu (k, v) ->
             if k = "t" then ("t", "REFRESH") :: accu
             else if k = "idigest" || k = "" then accu
             else (k, v) :: accu)
          [] conf.env
      in
      let new_env =
        if save_t = "REFRESH" then new_env
        else ("em", save_t) :: new_env
      in
      let new_env = ("idigest", digest) :: new_env in
      let new_env = ("report", report) :: new_env in
      let conf = { (conf) with env = new_env } in
      Hutil.interp conf "carrousel"
        {Templ.eval_var = eval_var conf base;
         Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
         Templ.eval_predefined_apply = (fun _ -> raise Not_found);
         Templ.get_vother = get_vother;
         Templ.set_vother = set_vother;
         Templ.print_foreach = print_foreach conf base}
        [] ep
  | None ->
      Hutil.incorrect_request conf

let print_image conf base = print_confirm_c conf base ""

(* ************************************************************************ *)
(*  send, delete and reset functions                                        *)
(*                                                                          *)
(* ************************************************************************ *)

let effective_send_c_ok conf base p file file_name mode =
  let notes = match Util.p_getenv conf.env "notes" with
    Some v ->
      Util.safe_html
        (only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
    | None -> ""
  in
  let strm = Stream.of_string file in
  let (request, content) = Wserver.get_request_and_content strm in
  let content =
    if mode = "comment" then ""
    else
      let s =
        let rec loop len (strm__ : _ Stream.t) =
          match Stream.peek strm__ with
          | Some x -> Stream.junk strm__; loop (Buff.store len x) strm
          | _ -> Buff.get len
        in
        loop 0 strm
      in
      content ^ s
  in
  let (typ, content) =
    if content <> "" then
      match image_type content with
      | None ->
          let ct = Wserver.extract_param "content-type: " '\n' request in
          dump_bad_image conf content; incorrect_content_type conf base p ct
      | Some (typ, content) ->
          match p_getint conf.base_env "max_images_size" with
          | Some len when String.length content > len ->
              error_too_big_image conf base p (String.length content) len
          | _ -> typ, content
    else GIF, content (* we dont care which type, content = "" *)
  in
  let keydir = default_image_name base p in
  let full_dir =
    if mode = "portraits" then
      String.concat Filename.dir_sep
        [(Util.base_path ["images"] conf.bname);]
    else
      String.concat Filename.dir_sep
        [(Util.base_path ["src"] conf.bname); "images"; keydir]
  in
  let _ =
    if not (Sys.file_exists full_dir) then
      let d1 = String.concat
        Filename.dir_sep [(Util.base_path ["images"] conf.bname);]
      in
      let d2 = String.concat
        Filename.dir_sep
          [(Util.base_path ["src"] conf.bname); "images"; keydir]
      in
      Mutil.mkdir_p d1;
      Mutil.mkdir_p d2;
  in
  let full_name = Filename.concat full_dir
     (if mode = "portraits" then
      keydir ^ (extension_of_type typ)
     else file_name)
  in
  if mode = "portraits" then
    begin match Util.auto_image_file conf base p with
    | Some f ->
        let dir = Util.base_path ["images"] conf.bname in
        let old_dir = Filename.concat dir "saved" in
        let fname = Filename.basename f in
        clean_old_portrait old_dir fname;
        if (move_file_to_old dir fname) = 0 then incorrect conf;
    | None -> ()
    end
  else
    begin
    if (Sys.file_exists full_name) && content <> "" then
      let dir = String.concat Filename.dir_sep
        [(Util.base_path ["src"] conf.bname); "images"; keydir]
      in (* attention au full name *)
      if (move_file_to_old dir full_name) = 0 then incorrect conf
    end;
  if content <> "" then write_file full_name content;
  if notes <> "" then
    write_file ((Filename.remove_extension full_name) ^ ".txt") notes;
  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed
    (if mode = "portraits" then "si" else
      if file_name <> "" && notes <> "" then "sb"
      else if file_name <> "" then "so"
      else if notes <> "" then "sc" else "sn");
  file_name

(* removes portrait or other image and saves it into old folder *)
(* if delete=on permanently deletes the file in old folder *)

let effective_delete_c_ok conf base p =
  let keydir = default_image_name base p in
  let file_name = try List.assoc "file_name" conf.env with Not_found -> "" in
  let file_name = Wserver.decode file_name in
  let mode = try List.assoc "mode" conf.env with Not_found -> "portraits" in
  let delete =
    try (List.assoc "delete" conf.env = "on") with Not_found -> false
  in
  let ext = if delete then get_extension_old conf keydir
    else get_extension conf keydir
  in
  let file = if file_name = "" then keydir ^ ext else file_name in
  let full_dir =
    if mode = "portraits" then (Util.base_path ["images"] conf.bname)
    else String.concat Filename.dir_sep
      [(Util.base_path ["src"] conf.bname); "images"; keydir]
  in
    (* TODO verify we dont destroy a saved image
        having the same name as portrait! *)
  if delete then Mutil.rm
    (String.concat Filename.dir_sep [full_dir; "saved"; file])
  else
    if (move_file_to_old full_dir file) = 0 then incorrect conf;
  let changed =
    U_Delete_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed
    (if mode = "portraits" then "di" else "do");
  file_name

(* reset portrait or image from old folder to portrait or others *)

let swap_files conf file1 file2 txt =
  let tmp_file =
    String.concat Filename.dir_sep
      [(Util.base_path ["images"] conf.bname); "tempfile.tmp"]
  in
  let ext_1 = Filename.extension file1 in
  let ext_2 = Filename.extension file2 in
  rn file1 tmp_file;
  rn file2 ((Filename.remove_extension file1) ^ ext_2);
  rn tmp_file ((Filename.remove_extension file2) ^ ext_1);
  if txt then
    let tmp_file_t =
      String.concat Filename.dir_sep
        [(Util.base_path ["images"] conf.bname); "tempfile.tmp"]
    in
    let file1_t = (Filename.remove_extension file1) ^ ".txt" in
    let file2_t = (Filename.remove_extension file2) ^ ".txt" in
    rn file1_t tmp_file_t;
    rn file2_t file1_t;
    rn tmp_file_t file2_t

let rename_files file1 file2 txt =
  rn file1 file2;
  if txt then
    let file1_t = (Filename.remove_extension file1) ^ ".txt" in
    let file2_t = (Filename.remove_extension file2) ^ ".txt" in
    rn file1_t file2_t

let effective_reset_c_ok conf base p =
  let mode = try List.assoc "mode" conf.env with Not_found -> "portraits" in
  let keydir = default_image_name base p in
  if mode = "portraits" then
    begin
      let file_name = keydir in
      let ext_o = get_extension_old conf keydir in
      let ext = get_extension conf keydir in
      let ext = if ext = "." then ext_o else ext in
      let file_in_old =
        String.concat Filename.dir_sep
          [(Util.base_path ["images"] conf.bname); "saved";
            (file_name ^ ext_o)]
      in
      let file_in_portraits =
        String.concat Filename.dir_sep
          [(Util.base_path ["images"] conf.bname);
            (file_name ^ ext)]
      in
      if Sys.file_exists file_in_portraits then
        swap_files conf file_in_old file_in_portraits false
      else
        rename_files file_in_old file_in_portraits false
    end
  else
    begin
      let file_name =
        try List.assoc "file_name" conf.env with Not_found -> ""
      in
      let file_name = Wserver.decode file_name in
      let file_in_old =
        String.concat Filename.dir_sep
          [(Util.base_path ["src"] conf.bname); "images";
            keydir; "saved"; file_name]
      in
      let new_file =
        String.concat Filename.dir_sep
          [(Util.base_path ["src"] conf.bname); "images"; keydir; file_name]
      in
      if Sys.file_exists new_file then
        swap_files conf file_in_old new_file true
      else
        rename_files file_in_old new_file true
    end;
  let file_name =
    try List.assoc "file_name" conf.env with Not_found -> ""
  in
  file_name

let print_c conf base =
  (* if em="" this is the first pass, do it *)
  begin match p_getenv conf.env "em" with
  | None ->
    begin match p_getenv conf.env "t" with
    | Some t ->
      let save_t = t in
      begin match p_getenv conf.env "i" with
      | Some ip ->
          let p = poi base (Gwdb.iper_of_string ip) in
          let digest = default_image_name base p in
          let (conf, report) =
            begin match p_getenv conf.env "t" with
            | Some "SND_IMAGE_C_OK" ->
                let mode =
                  try List.assoc "mode" conf.env with Not_found -> "portraits"
                in
                let file_name =
                    (try List.assoc "file_name" conf.env with Not_found -> "")
                in
                let file_name =
                  if file_name = "" then
                    (try List.assoc "file_name_2" conf.env with Not_found -> "")
                  else file_name
                in
                let file_name_2 = Filename.remove_extension file_name in
                let new_env =
                  List.fold_left
                    (fun accu (k, v) ->
                       if k = "file_name_2" then (k, file_name_2) :: accu
                       else (k, v) :: accu)
                    [] conf.env
                in
                let conf = { (conf) with env = new_env } in
                let file_name = Wserver.decode file_name in
                let file =
                  if mode <> "comment" then raw_get conf "file"
                  else "file_name"
                in
                let idigest = try List.assoc "idigest" conf.env
                  with Not_found -> ""
                in
                if digest = idigest then
                  conf, effective_send_c_ok conf base p file file_name mode
                else
                  conf, "digest error"
            | Some "DEL_IMAGE_C_OK" ->
                let idigest = try List.assoc "idigest" conf.env
                  with Not_found -> ""
                in
                if digest = idigest then
                  conf, effective_delete_c_ok conf base p
                else conf, "digest error"
            | Some "RESET_IMAGE_C_OK" ->
                let idigest = try List.assoc "idigest" conf.env
                  with Not_found -> ""
                in
                if digest = idigest then
                  conf, effective_reset_c_ok conf base p
                else conf, "digest error"
            | Some "IMAGE_C" -> conf, "image"
            | _ -> conf, "incorrect request"
            end
          in
          begin match report with
          | "digest error" -> Update.error_digest conf
          | "incorrect request" -> Hutil.incorrect_request conf
          | _ -> print_confirm_c conf base save_t report
          end
      | None -> Hutil.incorrect_request conf
      end
    | None -> Hutil.incorrect_request conf
    end
  (* em!="" second pass, ignore *)
  | Some _ -> print_confirm_c conf base "REFRESH" ""
  end

let auto_image_file ?bak:(b=false) conf base p =
  let s = default_image_name base p in
  let dir =
      if b then Filename.concat (base_path ["images"] conf.bname) "saved"
      else (base_path ["images"] conf.bname)
  in
  let f = Filename.concat dir s in
  if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".png") then Some (f ^ ".png")
  else None

(* ************************************************************************** *)
(*  [Fonc] print : Config.config -> Gwdb.base -> unit                         *)
(* ************************************************************************** *)
let print ?(bak=false) conf base =
  match (Util.p_getenv conf.env "s", Util.find_person_in_env conf base "") with
  | (Some f, Some p) ->
      let k = Util.default_image_name base p in
      let f =
          if bak then String.concat Filename.dir_sep [k; "saved"; f]
          else String.concat Filename.dir_sep [k; f]
      in
      ImageDisplay.print_source_image conf f
  | (Some f, _) ->
      ImageDisplay.print_source_image conf f
  | (_, Some p) ->
      begin match auto_image_file ~bak:bak conf base p with
      | Some f ->
          if ImageDisplay.print_image_file f then () else Hutil.incorrect_request conf
      | _ -> Hutil.incorrect_request conf
      end
  | (_, _) -> Hutil.incorrect_request conf

let () =
  Gwdlib.GwdPlugin.register ~ns:"carrousel" [ "SND_IMAGE", fun assets conf base ->
      match Util.find_person_in_env conf base "" with
      | None -> false
      | Some p ->
          let ep = p, authorized_age conf base p in
          let env =
            [("p", Vind p);
             ("p_auth", Vbool (authorized_age conf base p));
             ("count", Vcnt (ref 0));
            ]
          in
          match p_getenv conf.env "t" with
          | Some "IM" -> print ~bak:false conf base; true
          | Some "IMS" -> print ~bak:true conf base; true
          | Some "SND_IMAGE_C" | Some "REFRESH" ->
              Hutil.interp conf "carrousel"
                  {Templ.eval_var = eval_var conf base;
                   Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
                   Templ.eval_predefined_apply = eval_predefined_apply conf;
                   Templ.get_vother = get_vother; Templ.set_vother = set_vother;
                   Templ.print_foreach = print_foreach conf base}
                  env ep; true
          | Some "SND_IMAGE_C_OK" -> print_c conf base; true
          | Some "DEL_IMAGE_C_OK" -> print_c conf base; true
          | Some "IMAGE_C" -> print_c conf base; true
          | Some "RESET_IMAGE_C_OK" -> print_c conf base; true
          | _ -> false
    ]
