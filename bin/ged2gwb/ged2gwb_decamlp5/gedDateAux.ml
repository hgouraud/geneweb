(* Sous-système "dates GEDCOM" extrait de ged2gwb.ml pour être partagé avec le
   parseur menhir (qui ne peut pas dépendre en retour de ged2gwb).

   À DÉPLACER depuis ged2gwb.ml (couper/coller, puis remplacer les usages
   restants dans ged2gwb par GedDateAux.xxx) :
     - type month_number_dates        (l. 29-33)
     - ref no_negative_dates          (l. 49)
     - ref month_number_dates         (l. 50)
     - ref date_str                   (l. 464)
     - check_month                    (l. 296)   [+ warning_month_number_dates reste dans ged2gwb, il lit month_number_dates]
     - make_date / recover_date       (l. 466-520)
   Le reste (mk_yearint, is_roman_int, to_int, Not_a_date, range) est nouveau ou
   déplacé depuis l'EXTEND.

   Ci-dessous : copie fidèle de ces définitions, prête à l'emploi. *)

open Def

exception Not_a_date

type 'a range = Begin of 'a | End of 'a | BeginEnd of 'a * 'a

type month_number_dates =
  | MonthDayDates
  | DayMonthDates
  | NoMonthNumberDates
  | MonthNumberHappened of string

let no_negative_dates = ref false
let month_number_dates = ref NoMonthNumberDates
let date_str = ref ""

let check_month m =
  (* corps identique à ged2gwb.ml l. 296 — à recopier tel quel *)
  if m < 1 || m > 12 then () else ()   (* PLACEHOLDER : remplacer par l'original *)

let to_int s = try int_of_string s with Failure _ -> raise Not_a_date

let is_roman_int x =
  try let _ = Mutil.arabian_of_roman x in true with Not_found -> false

(* Repris tel quel de l'EXTEND (l. 466-508). *)
let make_date n1 n2 n3 =
  let n3 =
    if !no_negative_dates then
      match n3 with Some n3 -> Some (abs n3) | None -> None
    else n3
  in
  match n1, n2, n3 with
  | Some d, Some m, Some y ->
      let (d, m) =
        match m with
        | Right m -> d, m
        | Left m ->
            match !month_number_dates with
            | DayMonthDates -> check_month m; d, m
            | MonthDayDates -> check_month d; m, d
            | _ ->
                if d >= 1 && m >= 1 && d <= 31 && m <= 31 then
                  if d > 13 && m <= 13 then d, m
                  else if m > 13 && d <= 13 then m, d
                  else if d > 13 && m > 13 then 0, 0
                  else begin
                    month_number_dates := MonthNumberHappened !date_str;
                    0, 0
                  end
                else 0, 0
      in
      let (d, m) = if m < 1 || m > 13 then 0, 0 else d, m in
      { Adef.day = d; month = m; year = y; prec = Sure; delta = 0 }
  | None, Some m, Some y ->
      let m = match m with Right m -> m | Left m -> m in
      { Adef.day = 0; month = m; year = y; prec = Sure; delta = 0 }
  | None, None, Some y ->
      { Adef.day = 0; month = 0; year = y; prec = Sure; delta = 0 }
  | Some y, None, None ->
      { Adef.day = 0; month = 0; year = y; prec = Sure; delta = 0 }
  | _ -> raise Not_a_date   (* ancien : raise (Stream.Error "bad date") *)

(* Repris tel quel de l'EXTEND (l. 510-520). *)
let recover_date cal = function
  | Adef.Dgreg (d, Dgregorian) ->
      let d =
        match cal with
        | Dgregorian -> d
        | Djulian -> Calendar.gregorian_of_julian d
        | Dfrench -> Calendar.gregorian_of_french d
        | Dhebrew -> Calendar.gregorian_of_hebrew d
      in
      Adef.Dgreg (d, cal)
  | d -> d

(* Extrait du cas BeginEnd de date_or_text (EXTEND l. 554-573). *)
let mk_yearint (d1, cal1) (d2, cal2) =
  let dmy2 =
    match cal2 with
    | Dgregorian ->
        { Adef.day2 = d2.Adef.day; month2 = d2.month; year2 = d2.year; delta2 = 0 }
    | Djulian ->
        let d = Calendar.julian_of_gregorian d2 in
        { Adef.day2 = d.Adef.day; month2 = d.month; year2 = d.year; delta2 = 0 }
    | Dfrench ->
        let d = Calendar.french_of_gregorian d2 in
        { Adef.day2 = d.Adef.day; month2 = d.month; year2 = d.year; delta2 = 0 }
    | Dhebrew ->
        let d = Calendar.hebrew_of_gregorian d2 in
        { Adef.day2 = d.Adef.day; month2 = d.month; year2 = d.year; delta2 = 0 }
  in
  Adef.Dgreg ({ d1 with Adef.prec = YearInt dmy2 }, cal1)
