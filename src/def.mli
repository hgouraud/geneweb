(* $Id: def.mli,v 5.22 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type ('a, 'b) choice =
    Left of 'a
  | Right of 'b

type iper = Adef.iper
type ifam = Adef.ifam

type cdate = Adef.cdate
type codate = Adef.codate

type date =
  Adef.date =
      Dgreg of dmy * calendar
    | Dtext of string
and calendar = Adef.calendar = Dgregorian | Djulian | Dfrench | Dhebrew
and dmy =
  Adef.dmy =
    { day : int; month : int; year : int; prec : precision; delta : int }
and precision =
  Adef.precision =
      Sure
    | About
    | Maybe
    | Before
    | After
    | OrYear of int
    | YearInt of int

type relation_kind =
    Married
  | NotMarried
  | Engaged
  | NoSexesCheckNotMarried
  | NoMention
  | NoSexesCheckMarried

type divorce =
    NotDivorced
  | Divorced of codate
  | Separated

type death_reason = Killed | Murdered | Executed | Disappeared | Unspecified
type death =
    NotDead
  | Death of death_reason * cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead

type burial =
    UnknownBurial
  | Buried of codate
  | Cremated of codate

type access = IfTitles | Public | Private | Friend | Friend_m

type 'string gen_title_name =
    Tmain
  | Tname of 'string
  | Tnone
type 'string gen_title =
  { t_name : 'string gen_title_name;
    t_ident : 'string;
    t_place : 'string;
    t_date_start : codate;
    t_date_end : codate;
    t_nth : int }

type relation_type =
  Adoption | Recognition | CandidateParent | GodParent | FosterParent

type ('person, 'string) gen_relation =
  { r_type : relation_type;
    r_fath : 'person option;
    r_moth : 'person option;
    r_sources : 'string }

type sex = Male | Female | Neuter

(* person *)

type ('person, 'string) gen_person =
  { first_name : 'string;
    surname : 'string;
    occ : int;
    image : 'string;
    public_name : 'string;
    qualifiers : 'string list;
    aliases : 'string list;
    first_names_aliases : 'string list;
    surnames_aliases : 'string list;
    titles : 'string gen_title list;
    rparents : ('person, 'string) gen_relation list;
    related : iper list;
    occupation : 'string;
    sex : sex;
    access : access;
    birth : codate;
    birth_place : 'string;
    birth_src : 'string;
    baptism : codate;
    baptism_place : 'string;
    baptism_src : 'string;
    death : death;
    death_place : 'string;
    death_src : 'string;
    burial : burial;
    burial_place : 'string;
    burial_src : 'string;
    notes : 'string;
    psources : 'string;
    key_index : iper }

type 'family gen_ascend = { parents : 'family option; consang : Adef.fix }

type 'family gen_union = { family : 'family array }

(* family *)

type ('person, 'string) gen_family =
  { marriage : codate;
    marriage_place : 'string;
    marriage_src : 'string;
    witnesses : 'person array;
    relation : relation_kind;
    divorce : divorce;
    comment : 'string;
    origin_file : 'string;
    fsources : 'string;
    fam_index : ifam }

type 'person gen_couple = 'person Adef.gen_couple

type 'person gen_descend = { children : 'person array }

type 'person error =
    AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person

type ('person, 'descend, 'title) warning =
    BigAgeBetweenSpouses of 'person * 'person * dmy
  | BirthAfterDeath of 'person
  | BirthAfterBaptism of 'person
  | IncoherentSex of 'person * int * int
  | ChangedOrderOfChildren of ifam * 'descend * iper array * iper array
  | ChangedOrderOfMarriages of 'person * ifam array * ifam array
  | ChildrenNotInOrder of ifam * 'descend * 'person * 'person
  | CloseChildren of ifam * 'descend * 'person * 'person
  | DeadOld of 'person * dmy
  | DeadTooEarlyToBeFather of 'person * 'person
  | IncoherentAncestorDate of 'person * 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person * 'person
  | OldForMarriage of 'person * dmy
  | ParentBornAfterChild of 'person * 'person
  | ParentTooOld of 'person * dmy
  | ParentTooYoung of 'person * dmy
  | TitleDatesError of 'person * 'title
  | UndefinedSex of 'person
  | WitnessDateAfterDeath of 'person
  | WitnessDateBeforeBirth of 'person
  | YoungForMarriage of 'person * dmy

type ('person, 'descend, 'title) misc = MissingSources

type rn_mode = RnAll | Rn1Ln | RnDeg


(* Historique des modifications *)

type ('person, 'string) base_changed =
    U_Add_person of ('person, 'string) gen_person
  | U_Modify_person of
      ('person, 'string) gen_person * ('person, 'string) gen_person
  | U_Delete_person of ('person, 'string) gen_person
  | U_Merge_person of
      ('person, 'string) gen_person * ('person, 'string) gen_person *
        ('person, 'string) gen_person
  | U_Send_image of ('person, 'string) gen_person
  | U_Delete_image of ('person, 'string) gen_person
  | U_Add_family of
      ('person, 'string) gen_person * ('person, 'string) gen_family
  | U_Modify_family of
      ('person, 'string) gen_person * ('person, 'string) gen_family *
        ('person, 'string) gen_family
  | U_Delete_family of
      ('person, 'string) gen_person * ('person, 'string) gen_family
  | U_Invert_family of ('person, 'string) gen_person * ifam
  | U_Merge_family of
      ('person, 'string) gen_person * ('person, 'string) gen_family *
        ('person, 'string) gen_family * ('person, 'string) gen_family
  | U_Change_children_name of
      ('person, 'string) gen_person *
        ((string * string * int * iper) * (string * string * int * iper)) list
  | U_Add_parent of
      ('person, 'string) gen_person * ('person, 'string) gen_family
  | U_Kill_ancestors of ('person, 'string) gen_person
  | U_Multi of ('person, 'string) gen_person
  | U_Notes of int option * string
