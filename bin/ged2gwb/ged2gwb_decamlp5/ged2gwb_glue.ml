(* ------------------------------------------------------------------ *)
(* Modifications à appliquer dans ged2gwb.ml                          *)
(* ------------------------------------------------------------------ *)

(* 1. SUPPRIMER de ged2gwb.ml :
      - les stream-parsers de dates : lexing_date, number, ident, text (l. 389-412)
      - make_date_lexing, tparse, using_token, date_lexer (l. 414-439)
      - date_g, date_value, date_interval, date_value_recover (l. 446-449)
      - roman_int via Grammar.Entry.of_parser (l. 458-462)
      - tout le bloc [@@@ocaml.warning "-27"] EXTEND ... END [@@@..."+27"] (l. 522-697)
      - make_date / recover_date / is_roman_int / type range / check_month /
        month_number_dates / no_negative_dates / date_str
        -> déplacés dans GedDateAux (cf. gedDateAux.ml)

   2. Remplacer les usages restants dans ged2gwb par GedDateAux.xxx :
        !no_negative_dates      -> !GedDateAux.no_negative_dates
        !month_number_dates     -> !GedDateAux.month_number_dates   (et l'affectation)
        MonthDayDates/...       -> GedDateAux.MonthDayDates/...
      (warning_month_number_dates, qui lit month_number_dates, suit le même préfixe.) *)

(* 3. NOUVELLE version de date_of_field (remplace l. 704-714).
   Plus de Stream ni de Ploc : on lexe avec ocamllex et on parse avec menhir.
   L'ancien filet Ploc.Exc(_, Stream.Error _) devient GedDateParser.Error
   (échec de parse) + GedDateAux.Not_a_date (échec sémantique : bad date,
   int overflow, romain invalide). *)
let date_of_field d =
  if d = "" then None
  else if preg_match "^[0-9]+$" d && String.length d > 8 then Some (Adef.Dtext d)
  else begin
    GedDateAux.date_str := d;
    let run entry =
      let lb = Lexing.from_string (String.uppercase_ascii d) in
      entry GedDateLexer.token lb
    in
    try Some (run GedDateParser.date_value)
    with GedDateParser.Error | GedDateAux.Not_a_date -> (
      try Some (run GedDateParser.date_value_recover)
      with GedDateParser.Error | GedDateAux.Not_a_date -> Some (Dtext d))
  end

(* 4. decode_date_interval (l. ~1141-1144) : même substitution.
   Ancien :
       let strm = Stream.of_string (String.uppercase_ascii s) in
       match Grammar.Entry.parse date_interval strm with ...
   Nouveau : *)
let _decode_date_interval_body s =
  let lb = Lexing.from_string (String.uppercase_ascii s) in
  match GedDateParser.date_interval GedDateLexer.token lb with
  | exception (GedDateParser.Error | GedDateAux.Not_a_date) -> None
  | r -> Some r   (* r : Def.date GedDateAux.range — adapter à l'appelant *)
