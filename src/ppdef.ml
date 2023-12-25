(* camlp5r *)
(* $Id: ppdef.ml,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)

(* #load "pa_extend.cmo" *)
(* #load "q_MLast.cmo" *)

open Pcaml

let _ = Stdpp.loc_name := "loc"

type 'a item_or_def =
    SdStr of 'a
  | SdDef of string * (string list * MLast.expr) option
  | SdUnd of string
  | SdNop

let rec list_remove x =
  function
    (y, _) :: l when y = x -> l
  | d :: l -> d :: list_remove x l
  | [] -> []

let defined = ref ["CAMLP5", None]

let is_defined i = List.mem_assoc i !defined

let loc = Grammar.loc_of_token_interval 0 0
let _loc = loc

let subst mloc env =
  let rec loop =
    function
      MLast.ExLet (_, Ploc.VaVal rf, Ploc.VaVal pel, e) ->
        let pel = List.map (fun (p, e) -> p, loop e) pel in
        MLast.ExLet (loc, Ploc.VaVal rf, Ploc.VaVal pel, loop e)
    | MLast.ExIfe (_, e1, e2, e3) ->
        MLast.ExIfe (loc, loop e1, loop e2, loop e3)
    | MLast.ExApp (_, e1, e2) -> MLast.ExApp (loc, loop e1, loop e2)
    | MLast.ExLid (_, Ploc.VaVal x) | MLast.ExUid (_, Ploc.VaVal x) as e ->
        (try MLast.ExAnt (loc, List.assoc x env) with Not_found -> e)
    | MLast.ExTup (_, Ploc.VaVal x) ->
        MLast.ExTup (loc, Ploc.VaVal (List.map loop x))
    | MLast.ExRec (_, Ploc.VaVal pel, None) ->
        let pel = List.map (fun (p, e) -> p, loop e) pel in
        MLast.ExRec (loc, Ploc.VaVal pel, None)
    | e -> e
  in
  loop

let substp mloc env =
  let rec loop =
    function
      MLast.ExApp (_, e1, e2) -> MLast.PaApp (loc, loop e1, loop e2)
    | MLast.ExChr (_, Ploc.VaVal c) -> MLast.PaChr (loc, Ploc.VaVal c)
    | MLast.ExLid (_, Ploc.VaVal x) ->
        begin try MLast.PaAnt (loc, List.assoc x env) with
          Not_found -> MLast.PaLid (loc, Ploc.VaVal x)
        end
    | MLast.ExUid (_, Ploc.VaVal x) ->
        begin try MLast.PaAnt (loc, List.assoc x env) with
          Not_found -> MLast.PaUid (loc, Ploc.VaVal x)
        end
    | MLast.ExInt (_, Ploc.VaVal x, "") -> MLast.PaInt (loc, Ploc.VaVal x, "")
    | MLast.ExTup (_, Ploc.VaVal x) ->
        MLast.PaTup (loc, Ploc.VaVal (List.map loop x))
    | MLast.ExRec (_, Ploc.VaVal pel, None) ->
        let ppl = List.map (fun (p, e) -> p, loop e) pel in
        MLast.PaRec (loc, Ploc.VaVal ppl)
    | x ->
        Stdpp.raise_with_loc mloc
          (Failure
             "this macro cannot be used in a pattern (see its definition)")
  in
  loop

let cannot_eval e =
  let loc = MLast.loc_of_expr e in
  Stdpp.raise_with_loc loc (Stream.Error "can't eval")

let rec eval =
  function
    MLast.ExApp
      (_,
       MLast.ExAcc
         (_, MLast.ExUid (_, Ploc.VaVal "Char"),
          MLast.ExLid (_, Ploc.VaVal "chr")),
       e) ->
      begin match eval e with
        MLast.ExInt (_, Ploc.VaVal i, "") ->
          let c = Char.escaped (Char.chr (int_of_string i)) in
          MLast.ExChr (loc, Ploc.VaVal c)
      | e -> cannot_eval e
      end
  | MLast.ExApp
      (_,
       MLast.ExAcc
         (_, MLast.ExUid (_, Ploc.VaVal "Char"),
          MLast.ExLid (_, Ploc.VaVal "code")),
       e) ->
      begin match eval e with
        MLast.ExChr (_, Ploc.VaVal c) ->
          let i = string_of_int (Char.code (Token.eval_char c)) in
          MLast.ExInt (loc, Ploc.VaVal i, "")
      | e -> cannot_eval e
      end
  | MLast.ExApp (_, MLast.ExApp (_, op, x), y) ->
      let f = eval op in
      let x = eval x in
      let y = eval y in
      begin match x, y with
        MLast.ExInt (_, Ploc.VaVal x, ""),
        MLast.ExInt (_, Ploc.VaVal y, "") ->
          let x = int_of_string x in
          let y = int_of_string y in
          begin match f with
            MLast.ExLid (_, Ploc.VaVal "+") ->
              MLast.ExInt (loc, Ploc.VaVal (string_of_int (x + y)), "")
          | MLast.ExLid (_, Ploc.VaVal "-") ->
              MLast.ExInt (loc, Ploc.VaVal (string_of_int (x - y)), "")
          | MLast.ExLid (_, Ploc.VaVal "lor") ->
              let s = Printf.sprintf "0o%o" (x lor y) in
              MLast.ExInt (loc, Ploc.VaVal s, "")
          | _ -> cannot_eval op
          end
      | _ -> cannot_eval op
      end
  | MLast.ExUid (_, Ploc.VaVal x) as e ->
      (try (match List.assoc x !defined with _ -> e) with Not_found -> e)
  | MLast.ExLid (_, Ploc.VaVal _) | MLast.ExChr (_, Ploc.VaVal _) |
    MLast.ExInt (_, Ploc.VaVal _, "") as e ->
      e
  | e -> cannot_eval e

let may_eval =
  function
    MLast.ExApp (_, MLast.ExUid (_, Ploc.VaVal "EVAL"), e) -> eval e
  | e -> e

let incorrect_number loc l1 l2 =
  Stdpp.raise_with_loc loc
    (Failure
       (Printf.sprintf "expected %d parameters; found %d" (List.length l2)
          (List.length l1)))

let first_pos = Stdpp.first_pos

let define eo x =
  let gloc = loc in
  begin match eo with
    Some ([], e) ->
      Grammar.safe_extend
        [Grammar.extension (expr : 'expr Grammar.Entry.e)
           (Some (Gramext.Level "simple"))
           [None, None,
            [Grammar.production
               (Grammar.r_next Grammar.r_stop (Grammar.s_token ("UIDENT", x)),
                (fun _ (loc : Ploc.t) ->
                   (may_eval
                      (Pcaml.expr_reloc (fun _ -> loc) (first_pos gloc) e) :
                    'expr)))]];
         Grammar.extension (patt : 'patt Grammar.Entry.e)
           (Some (Gramext.Level "simple"))
           [None, None,
            [Grammar.production
               (Grammar.r_next Grammar.r_stop (Grammar.s_token ("UIDENT", x)),
                (fun _ (loc : Ploc.t) ->
                   (let p = substp loc [] e in
                    Pcaml.patt_reloc (fun _ -> loc) (first_pos gloc) p :
                    'patt)))]]]
  | Some (sl, e) ->
      Grammar.safe_extend
        [Grammar.extension (expr : 'expr Grammar.Entry.e)
           (Some (Gramext.Level "apply"))
           [None, None,
            [Grammar.production
               (Grammar.r_next
                  (Grammar.r_next Grammar.r_stop
                     (Grammar.s_token ("UIDENT", x)))
                  Grammar.s_self,
                (fun (param : 'expr) _ (loc : Ploc.t) ->
                   (let el =
                      match param with
                        MLast.ExTup (_, Ploc.VaVal el) -> el
                      | e -> [e]
                    in
                    if List.length el = List.length sl then
                      let env = List.combine sl el in
                      let e = subst loc env e in
                      may_eval
                        (Pcaml.expr_reloc (fun _ -> loc) (first_pos gloc) e)
                    else incorrect_number loc el sl :
                    'expr)))]];
         Grammar.extension (patt : 'patt Grammar.Entry.e)
           (Some (Gramext.Level "simple"))
           [None, None,
            [Grammar.production
               (Grammar.r_next
                  (Grammar.r_next Grammar.r_stop
                     (Grammar.s_token ("UIDENT", x)))
                  Grammar.s_self,
                (fun (param : 'patt) _ (loc : Ploc.t) ->
                   (let pl =
                      match param with
                        MLast.PaTup (_, Ploc.VaVal pl) -> pl
                      | p -> [p]
                    in
                    if List.length pl = List.length sl then
                      let env = List.combine sl pl in
                      let p = substp loc env e in
                      Pcaml.patt_reloc (fun _ -> loc) (first_pos gloc) p
                    else incorrect_number loc pl sl :
                    'patt)))]]]
  | None -> ()
  end;
  defined := (x, eo) :: !defined

let undef x =
  try
    let eo = List.assoc x !defined in
    begin match eo with
      Some ([], _) ->
        Grammar.safe_delete_rule expr
          (Grammar.r_next Grammar.r_stop (Grammar.s_token ("UIDENT", x)));
        Grammar.safe_delete_rule patt
          (Grammar.r_next Grammar.r_stop (Grammar.s_token ("UIDENT", x)))
    | Some (_, _) ->
        Grammar.safe_delete_rule expr
          (Grammar.r_next
             (Grammar.r_next Grammar.r_stop (Grammar.s_token ("UIDENT", x)))
             Grammar.s_self);
        Grammar.safe_delete_rule patt
          (Grammar.r_next
             (Grammar.r_next Grammar.r_stop (Grammar.s_token ("UIDENT", x)))
             Grammar.s_self)
    | None -> ()
    end;
    defined := list_remove x !defined
  with Not_found -> ()

let _ =
  Grammar.safe_extend
    (let _ = (expr : 'expr Grammar.Entry.e)
     and _ = (patt : 'patt Grammar.Entry.e)
     and _ = (str_item : 'str_item Grammar.Entry.e)
     and _ = (sig_item : 'sig_item Grammar.Entry.e) in
     let grammar_entry_create s =
       Grammar.create_local_entry (Grammar.of_entry expr) s
     in
     let macro_def : 'macro_def Grammar.Entry.e =
       grammar_entry_create "macro_def"
     and str_item_or_macro : 'str_item_or_macro Grammar.Entry.e =
       grammar_entry_create "str_item_or_macro"
     and opt_macro_value : 'opt_macro_value Grammar.Entry.e =
       grammar_entry_create "opt_macro_value"
     and id_then_expr : 'id_then_expr Grammar.Entry.e =
       grammar_entry_create "id_then_expr"
     and uident : 'uident Grammar.Entry.e = grammar_entry_create "uident" in
     [Grammar.extension (str_item : 'str_item Grammar.Entry.e)
        (Some Gramext.First)
        [None, None,
         [Grammar.production
            (Grammar.r_next Grammar.r_stop
               (Grammar.s_nterm (macro_def : 'macro_def Grammar.Entry.e)),
             (fun (x : 'macro_def) (loc : Ploc.t) ->
                (match x with
                   SdStr [si] -> si
                 | SdStr sil -> MLast.StDcl (loc, Ploc.VaVal sil)
                 | SdDef (x, eo) ->
                     define eo x; MLast.StDcl (loc, Ploc.VaVal [])
                 | SdUnd x -> undef x; MLast.StDcl (loc, Ploc.VaVal [])
                 | SdNop -> MLast.StDcl (loc, Ploc.VaVal []) :
                 'str_item)))]];
      Grammar.extension (macro_def : 'macro_def Grammar.Entry.e) None
        [None, None,
         [Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next
                           (Grammar.r_next
                              (Grammar.r_next Grammar.r_stop
                                 (Grammar.s_token ("", "IFNDEF")))
                              (Grammar.s_nterm
                                 (uident : 'uident Grammar.Entry.e)))
                           (Grammar.s_token ("", "THEN")))
                        (Grammar.s_nterm
                           (str_item_or_macro :
                            'str_item_or_macro Grammar.Entry.e)))
                     (Grammar.s_token ("", "ELSE")))
                  (Grammar.s_nterm
                     (str_item_or_macro :
                      'str_item_or_macro Grammar.Entry.e)))
               (Grammar.s_token ("", "END")),
             (fun _ (d2 : 'str_item_or_macro) _ (d1 : 'str_item_or_macro) _
                  (i : 'uident) _ (loc : Ploc.t) ->
                (if is_defined i then d2 else d1 : 'macro_def)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next Grammar.r_stop
                           (Grammar.s_token ("", "IFNDEF")))
                        (Grammar.s_nterm (uident : 'uident Grammar.Entry.e)))
                     (Grammar.s_token ("", "THEN")))
                  (Grammar.s_nterm
                     (str_item_or_macro :
                      'str_item_or_macro Grammar.Entry.e)))
               (Grammar.s_token ("", "END")),
             (fun _ (d : 'str_item_or_macro) _ (i : 'uident) _
                  (loc : Ploc.t) ->
                (if is_defined i then SdNop else d : 'macro_def)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next
                           (Grammar.r_next
                              (Grammar.r_next Grammar.r_stop
                                 (Grammar.s_token ("", "IFDEF")))
                              (Grammar.s_nterm
                                 (uident : 'uident Grammar.Entry.e)))
                           (Grammar.s_token ("", "THEN")))
                        (Grammar.s_nterm
                           (str_item_or_macro :
                            'str_item_or_macro Grammar.Entry.e)))
                     (Grammar.s_token ("", "ELSE")))
                  (Grammar.s_nterm
                     (str_item_or_macro :
                      'str_item_or_macro Grammar.Entry.e)))
               (Grammar.s_token ("", "END")),
             (fun _ (d2 : 'str_item_or_macro) _ (d1 : 'str_item_or_macro) _
                  (i : 'uident) _ (loc : Ploc.t) ->
                (if is_defined i then d1 else d2 : 'macro_def)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next Grammar.r_stop
                           (Grammar.s_token ("", "IFDEF")))
                        (Grammar.s_nterm (uident : 'uident Grammar.Entry.e)))
                     (Grammar.s_token ("", "THEN")))
                  (Grammar.s_nterm
                     (str_item_or_macro :
                      'str_item_or_macro Grammar.Entry.e)))
               (Grammar.s_token ("", "END")),
             (fun _ (d : 'str_item_or_macro) _ (i : 'uident) _
                  (loc : Ploc.t) ->
                (if is_defined i then d else SdNop : 'macro_def)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next Grammar.r_stop (Grammar.s_token ("", "UNDEF")))
               (Grammar.s_nterm (uident : 'uident Grammar.Entry.e)),
             (fun (i : 'uident) _ (loc : Ploc.t) -> (SdUnd i : 'macro_def)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next Grammar.r_stop
                     (Grammar.s_token ("", "DEFINE")))
                  (Grammar.s_nterm (uident : 'uident Grammar.Entry.e)))
               (Grammar.s_nterm
                  (opt_macro_value : 'opt_macro_value Grammar.Entry.e)),
             (fun (def : 'opt_macro_value) (i : 'uident) _ (loc : Ploc.t) ->
                (SdDef (i, def) : 'macro_def)))]];
      Grammar.extension
        (str_item_or_macro : 'str_item_or_macro Grammar.Entry.e) None
        [None, None,
         [Grammar.production
            (Grammar.r_next Grammar.r_stop
               (Grammar.s_list1
                  (Grammar.s_nterm (str_item : 'str_item Grammar.Entry.e))),
             (fun (si : 'str_item list) (loc : Ploc.t) ->
                (SdStr si : 'str_item_or_macro)));
          Grammar.production
            (Grammar.r_next Grammar.r_stop
               (Grammar.s_nterm (macro_def : 'macro_def Grammar.Entry.e)),
             (fun (d : 'macro_def) (loc : Ploc.t) ->
                (d : 'str_item_or_macro)))]];
      Grammar.extension (opt_macro_value : 'opt_macro_value Grammar.Entry.e)
        None
        [None, None,
         [Grammar.production
            (Grammar.r_stop,
             (fun (loc : Ploc.t) -> (None : 'opt_macro_value)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next Grammar.r_stop (Grammar.s_token ("", "=")))
               (Grammar.s_nterm (expr : 'expr Grammar.Entry.e)),
             (fun (e : 'expr) _ (loc : Ploc.t) ->
                (Some ([], e) : 'opt_macro_value)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next Grammar.r_stop
                           (Grammar.s_token ("", "(")))
                        (Grammar.s_list1sep (Grammar.s_token ("LIDENT", ""))
                           (Grammar.s_token ("", ",")) false))
                     (Grammar.s_token ("", ")")))
                  (Grammar.s_token ("", "=")))
               (Grammar.s_nterm (expr : 'expr Grammar.Entry.e)),
             (fun (e : 'expr) _ _ (pl : string list) _ (loc : Ploc.t) ->
                (Some (pl, e) : 'opt_macro_value)))]];
      Grammar.extension (expr : 'expr Grammar.Entry.e)
        (Some (Gramext.Level "top"))
        [None, None,
         [Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next
                           (Grammar.r_next
                              (Grammar.r_next Grammar.r_stop
                                 (Grammar.s_token ("", "IFNDEF")))
                              (Grammar.s_nterm
                                 (uident : 'uident Grammar.Entry.e)))
                           (Grammar.s_token ("", "THEN")))
                        Grammar.s_self)
                     (Grammar.s_token ("", "ELSE")))
                  Grammar.s_self)
               (Grammar.s_token ("", "END")),
             (fun _ (e2 : 'expr) _ (e1 : 'expr) _ (i : 'uident) _
                  (loc : Ploc.t) ->
                (if is_defined i then e2 else e1 : 'expr)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next Grammar.r_stop
                           (Grammar.s_token ("", "IFDEF")))
                        (Grammar.s_list1sep
                           (Grammar.s_nterm
                              (id_then_expr : 'id_then_expr Grammar.Entry.e))
                           (Grammar.s_token ("", "ELSIFDEF")) false))
                     (Grammar.s_token ("", "ELSE")))
                  Grammar.s_self)
               (Grammar.s_token ("", "END")),
             (fun _ (e2 : 'expr) _ (idl : 'id_then_expr list) _
                  (loc : Ploc.t) ->
                (let rec loop =
                   function
                     (i, e) :: idl -> if is_defined i then e else loop idl
                   | [] -> e2
                 in
                 loop idl :
                 'expr)))]];
      Grammar.extension (id_then_expr : 'id_then_expr Grammar.Entry.e) None
        [None, None,
         [Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next Grammar.r_stop
                     (Grammar.s_nterm (uident : 'uident Grammar.Entry.e)))
                  (Grammar.s_token ("", "THEN")))
               (Grammar.s_nterm (expr : 'expr Grammar.Entry.e)),
             (fun (e : 'expr) _ (i : 'uident) (loc : Ploc.t) ->
                (i, e : 'id_then_expr)))]];
      Grammar.extension (expr : 'expr Grammar.Entry.e)
        (Some (Gramext.Level "simple"))
        [None, None,
         [Grammar.production
            (Grammar.r_next Grammar.r_stop
               (Grammar.s_token ("LIDENT", "__FILE__")),
             (fun _ (loc : Ploc.t) ->
                (MLast.ExStr (loc, Ploc.VaVal !(Pcaml.input_file)) :
                 'expr)))]];
      Grammar.extension (patt : 'patt Grammar.Entry.e) None
        [None, None,
         [Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next
                           (Grammar.r_next
                              (Grammar.r_next Grammar.r_stop
                                 (Grammar.s_token ("", "IFNDEF")))
                              (Grammar.s_nterm
                                 (uident : 'uident Grammar.Entry.e)))
                           (Grammar.s_token ("", "THEN")))
                        Grammar.s_self)
                     (Grammar.s_token ("", "ELSE")))
                  Grammar.s_self)
               (Grammar.s_token ("", "END")),
             (fun _ (p2 : 'patt) _ (p1 : 'patt) _ (i : 'uident) _
                  (loc : Ploc.t) ->
                (if is_defined i then p2 else p1 : 'patt)));
          Grammar.production
            (Grammar.r_next
               (Grammar.r_next
                  (Grammar.r_next
                     (Grammar.r_next
                        (Grammar.r_next
                           (Grammar.r_next
                              (Grammar.r_next Grammar.r_stop
                                 (Grammar.s_token ("", "IFDEF")))
                              (Grammar.s_nterm
                                 (uident : 'uident Grammar.Entry.e)))
                           (Grammar.s_token ("", "THEN")))
                        Grammar.s_self)
                     (Grammar.s_token ("", "ELSE")))
                  Grammar.s_self)
               (Grammar.s_token ("", "END")),
             (fun _ (p2 : 'patt) _ (p1 : 'patt) _ (i : 'uident) _
                  (loc : Ploc.t) ->
                (if is_defined i then p1 else p2 : 'patt)))]];
      Grammar.extension (uident : 'uident Grammar.Entry.e) None
        [None, None,
         [Grammar.production
            (Grammar.r_next Grammar.r_stop (Grammar.s_token ("UIDENT", "")),
             (fun (i : string) (loc : Ploc.t) -> (i : 'uident)))]]])

let _ =
  Pcaml.add_option "-D" (Arg.String (define None))
    "<string> Define for IFDEF instruction."
let _ =
  Pcaml.add_option "-U" (Arg.String undef)
    "<string> Undefine for IFDEF instruction."

let _ =
  if Sys.ocaml_version >= "3.07" then
    defined := ("OCAML_307", None) :: !defined

let _ =
  if Sys.ocaml_version >= "3.08" then
    defined := ("OCAML_308", None) :: !defined

let _ =
  if Sys.ocaml_version >= "3.09" then
    defined := ("OCAML_309", None) :: !defined
