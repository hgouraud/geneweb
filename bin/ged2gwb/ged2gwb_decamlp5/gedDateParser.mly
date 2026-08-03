/* Grammaire menhir remplaçant le bloc EXTEND ... END (lignes 523-696) et les
   entrées Grammar.Entry date_value / date_value_recover / date_interval.

   Les actions sont reprises telles quelles de l'EXTEND.  Les fonctions
   auxiliaires (make_date, recover_date, mk_yearint, is_roman_int, to_int,
   exception Not_a_date, type 'a range) sont extraites de ged2gwb dans le
   module GedDateAux (voir gedDateAux.ml) pour casser le cycle de dépendance
   parseur <-> ged2gwb.  Left/Right viennent de Def (déjà utilisés par
   make_date). */

%{
open Def          /* Dtext, About, Maybe, Before, After, YearInt, calendriers, Left/Right */
open GedDateAux   /* range, make_date, recover_date, mk_yearint, is_roman_int, to_int, Not_a_date */
%}

%token <string> INT
%token <string> TEXT
%token <string> ID            /* chiffres romains + mots inconnus */
%token DOT SLASH MINUS AT HASH EOI OTHER
%token BEF AFT BET AND TO FROM
%token ABT ENV EST
%token DGREGORIAN DJULIAN DFRENCH DHEBREW
%token R AN BCE
%token JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
%token VEND BRUM FRIM NIVO PLUV VENT GERM FLOR PRAI MESS THER FRUC COMP
%token TSH CSH KSL TVT SHV ADR ADS NSN IYR SVN TMZ AAV ELL

%start <Def.date> date_value
%start <Def.date> date_value_recover
%start <Def.date GedDateAux.range> date_interval

%%

date_value:
  | d = date_or_text EOI { d }
;

date_value_recover:
  | AT HASH DGREGORIAN AT d = date_value { recover_date Dgregorian d }
  | AT HASH DJULIAN    AT d = date_value { recover_date Djulian d }
  | AT HASH DFRENCH R  AT d = date_value { recover_date Dfrench d }
  | AT HASH DHEBREW    AT d = date_value { recover_date Dhebrew d }
;

/* Décision 2 : date_range et date_or_text de l'EXTEND sont fusionnés ici.
   Dans l'EXTEND, date_or_text essayait date_range PUIS date ; les deux
   commençaient par AFT/BEF, ce qui est un choix ordonné intraduisible en LR.
   Comme date_range gagnait toujours pour AFT/BEF/BET/TO/FROM, on inline sa
   transformation (Begin/End -> prec After/Before, BeginEnd -> YearInt) et on
   n'ouvre la branche "date simple" que sur les têtes NON-intervalle
   (date_top, sans AFT/BEF). */
date_or_text:
  | BEF  d = date              { let (dd, cal) = d in Adef.Dgreg ({ dd with Adef.prec = Before }, cal) }
  | AFT  d = date              { let (dd, cal) = d in Adef.Dgreg ({ dd with Adef.prec = After }, cal) }
  | BET  d = date AND d1 = date { mk_yearint d d1 }
  | TO   d = date              { let (dd, cal) = d in Adef.Dgreg ({ dd with Adef.prec = Before }, cal) }
  | FROM d = date              { let (dd, cal) = d in Adef.Dgreg ({ dd with Adef.prec = After }, cal) }
  | FROM d = date TO d1 = date { mk_yearint d d1 }
  | dt = date_top              { let (dd, cal) = dt in Adef.Dgreg (dd, cal) }
  | s = TEXT                   { Dtext s }
;

/* Repli non-intervalle, utilisé seulement par date_interval (évite le conflit
   BEF-vs-BEF au niveau de date_interval). */
date_plain:
  | dt = date_top { let (dd, cal) = dt in Adef.Dgreg (dd, cal) }
  | s = TEXT      { Dtext s }
;

date_interval:
  | BEF  dt = date_or_text EOI                    { End dt }
  | AFT  dt = date_or_text EOI                    { Begin dt }
  | BET  dt = date_or_text AND dt1 = date_or_text EOI { BeginEnd (dt, dt1) }
  | TO   dt = date_or_text EOI                    { End dt }
  | FROM dt = date_or_text EOI                    { Begin dt }
  | FROM dt = date_or_text TO dt1 = date_or_text EOI  { BeginEnd (dt, dt1) }
  | dt = date_plain EOI                           { Begin dt }
;

/* date "opérande" complet (avec AFT/BEF), utilisé après un mot d'intervalle. */
date:
  | ABT d = date_calendar { let (dd, cal) = d in ({ dd with Adef.prec = About }, cal) }
  | ENV d = date_calendar { let (dd, cal) = d in ({ dd with Adef.prec = About }, cal) }
  | EST d = date_calendar { let (dd, cal) = d in ({ dd with Adef.prec = Maybe }, cal) }
  | AFT d = date_calendar { let (dd, cal) = d in ({ dd with Adef.prec = Before }, cal) }
  | BEF d = date_calendar { let (dd, cal) = d in ({ dd with Adef.prec = After }, cal) }
  | d = date_calendar     { d }
;

/* date "de tête" (sans AFT/BEF) : les têtes AFT/BEF sont captées comme
   intervalles au niveau de date_or_text. */
date_top:
  | ABT d = date_calendar { let (dd, cal) = d in ({ dd with Adef.prec = About }, cal) }
  | ENV d = date_calendar { let (dd, cal) = d in ({ dd with Adef.prec = About }, cal) }
  | EST d = date_calendar { let (dd, cal) = d in ({ dd with Adef.prec = Maybe }, cal) }
  | d = date_calendar     { d }
;

date_calendar:
  | AT HASH DGREGORIAN AT d = date_greg { (d, Dgregorian) }
  | AT HASH DJULIAN    AT d = date_greg { (Calendar.gregorian_of_julian d, Djulian) }
  | AT HASH DFRENCH R  AT d = date_fren { (Calendar.gregorian_of_french d, Dfrench) }
  | AT HASH DHEBREW    AT d = date_hebr { (Calendar.gregorian_of_hebrew d, Dhebrew) }
  | d = date_greg                       { (d, Dgregorian) }
;

date_greg:
  | list(DOT) n1 = ioption(int) list(dot_slash) n2 = ioption(gen_month)
    list(dot_slash) n3 = ioption(int) list(DOT)
    { make_date n1 n2 n3 }
;

/* Décision 3 : date_fren simplifié.  L'EXTEND avait 3 règles dont deux
   partageaient le préfixe [int]/[year_fren] (year_fren commence par int) :
   choix ordonné à retour arrière.  On garde 2 règles ; date_fren_kont couvre
   déjà mois + année (int | AN roman | roman), donc les cas "année seule" et
   "AN <roman>" y retombent avec un résultat make_date identique. */
date_fren:
  | list(DOT) n1 = int rest = date_fren_kont { make_date (Some n1) (fst rest) (snd rest) }
  | list(DOT) rest = date_fren_kont          { make_date None (fst rest) (snd rest) }
;

date_fren_kont:
  | list(dot_slash) n2 = ioption(gen_french) list(dot_slash)
    n3 = ioption(year_fren) list(DOT)
    { (n2, n3) }
;

date_hebr:
  | list(DOT) n1 = ioption(int) list(dot_slash) n2 = ioption(gen_hebr)
    list(dot_slash) n3 = ioption(int) list(DOT)
    { make_date n1 n2 n3 }
;

dot_slash: DOT { () } | SLASH { () } ;

gen_month:
  | i = int   { Left (abs i) }
  | m = month { Right m }
;

month:
  | JAN {1} | FEB {2} | MAR {3} | APR {4} | MAY {5} | JUN {6}
  | JUL {7} | AUG {8} | SEP {9} | OCT {10} | NOV {11} | DEC {12}
;

gen_french: m = french { Right m } ;

french:
  | VEND {1} | BRUM {2} | FRIM {3} | NIVO {4} | PLUV {5} | VENT {6}
  | GERM {7} | FLOR {8} | PRAI {9} | MESS {10} | THER {11} | FRUC {12} | COMP {13}
;

gen_hebr: m = hebr { Right m } ;

hebr:
  | TSH {1} | CSH {2} | KSL {3} | TVT {4} | SHV {5} | ADR {6} | ADS {7}
  | NSN {8} | IYR {9} | SVN {10} | TMZ {11} | AAV {12} | ELL {13}
;

year_fren:
  | i = int          { i }
  | AN i = roman_int { i }
  | i = roman_int    { i }
;

int:
  | i = INT       { to_int i }
  | MINUS i = INT { - (to_int i) }
  | i = INT BCE   { - (to_int i) }
;

/* Décision 4 : roman_int reste un ID générique validé par prédicat, exactement
   comme l'ancien Grammar.Entry.of_parser "roman int" (parser [<'("ID",x) when
   is_roman_int x>]).  Si l'ID n'est pas un romain valide, on lève Not_a_date,
   ce que date_of_field rattrape pour retomber sur Dtext. */
roman_int:
  | x = ID { if is_roman_int x then Mutil.arabian_of_roman x else raise Not_a_date }
;
