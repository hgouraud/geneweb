(* $Id: util.mli,v 5.36 2007-07-26 01:57:42 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

val start_time : float ref
val add_lang_path : string -> unit
val add_doc_path : string -> unit
val set_base_dir : string -> unit
val cnt_dir : string ref
val images_url : string ref
val image_prefix : config -> string
val base_path : string list -> string -> string

val find_misc_file : string -> string

val search_in_lang_path : string -> string
val search_in_doc_path : string -> string

val etc_file_name : config -> string -> string

val escache_value : base -> string
val commit_patches : config -> base -> unit
val update_wf_trace : config -> string -> unit

val get_referer : config -> string

val no_html_tags : string -> string
val clean_html_tags : string -> string list -> string

val nl : unit -> unit
val html : config -> unit
val html_br : config -> unit
val html_p : config -> unit
val html_li : config -> unit
val unauthorized : config -> string -> unit
val string_of_ctime : config -> string

val redirect_HTML : config -> string -> string -> unit

val commd : config -> string
val code_varenv : string -> string
val decode_varenv : string -> string
val hidden_env : config -> unit
val scramble : string -> string
val unscramble : string -> string

val nobtit : config -> base -> person -> title list

val strictly_after_private_years : config -> int -> dmy -> bool
val authorized_age : config -> base -> person -> bool
val is_old_person : config -> (iper, istr) gen_person -> bool
val fast_auth_age : config -> person -> bool

val start_with_vowel : string -> bool
val know : base -> person -> bool
val acces_n : config -> base -> string -> person -> string
val acces : config -> base -> person -> string
val wprint_hidden_person : config -> base -> string -> person -> unit
val accessible_by_key : config -> base -> person -> string -> string -> bool

val geneweb_link : config -> string -> string -> string
val wprint_geneweb_link : config -> string -> string -> unit

val is_restricted : config -> base -> iper -> bool
val is_hidden : person -> bool

val pget : config -> base -> iper -> person
val string_gen_person :
  base -> (iper, istr) gen_person -> (iper, string) gen_person
val string_gen_family :
  base -> (iper, istr) gen_family -> (iper, string) gen_family

type p_access = (base -> person -> string) * (base -> person -> string)
val std_access : p_access
val raw_access : p_access

(* Fonctions d'écriture du nom et prénom d'un individu en fonction de : *)
(*   - son/ses titre de noblesse                                        *)
(*   - son/ses nom public                                               *)
(*   - son/ses sobriquets ...                                           *)
val gen_person_text : p_access -> config -> base -> person -> string
val gen_person_text_no_html : p_access -> config -> base -> person -> string
val gen_person_text_without_title :
  p_access -> config -> base -> person -> string
val gen_person_title_text :
  (config -> base -> person -> string -> string) -> p_access -> config ->
    base -> person -> string
val person_text : config -> base -> person -> string
val person_text_no_html : config -> base -> person -> string
val person_text_without_surname : config -> base -> person -> string
val person_text_no_surn_no_acc_chk : config -> base -> person -> string
val person_text_without_title : config -> base -> person -> string
val main_title : config -> base -> person -> title option
val titled_person_text : config -> base -> person -> title -> string
val one_title_text : config -> base -> person -> title -> string
val person_title_text : config -> base -> person -> string
val person_title : config -> base -> person -> string

val reference : config -> base -> person -> string -> string
val no_reference : config -> base -> person -> string -> string
val referenced_person_title_text : config -> base -> person -> string
val referenced_person_text : config -> base -> person -> string
val referenced_person_text_without_surname :
  config -> base -> person -> string

val update_family_loop : config -> base -> person -> string -> string

val p_getenv : (string * string) list -> string -> string option
val p_getint : (string * string) list -> string -> int option
val create_env : string -> (string * string) list
val capitale : string -> string
val index_of_next_char : string -> int -> int

val open_etc_file : string -> in_channel option
val open_hed_trl : config -> string -> in_channel option
val open_templ : config -> string -> in_channel option
val copy_from_etc :
  (char * (unit -> string)) list -> string -> string -> in_channel -> unit
val string_with_macros :
  config -> (char * (unit -> string)) list -> string -> string
val string_of_place : config -> string -> string
val filter_html_tags : string -> string
val allowed_tags_file : string ref
val body_prop : config -> string
val url_no_index : config -> base -> bool -> string
val message_to_wizard : config -> unit
val check_xhtml : string -> string

val print_alphab_list :
  config -> ('a -> string) -> ('a -> unit) -> 'a list -> unit
val of_course_died : config -> person -> bool
val hexa_string : string -> string

val surname_begin : base -> string -> string
val surname_end : base -> string -> string
val get_particle : base -> string -> string
val old_surname_begin : string -> string
val old_surname_end : string -> string

val specify_homonymous : config -> base -> person -> bool -> unit

type ('a, 'b) format2 = ('a, unit, string, 'b) format4

val check_format : ('a, 'b) format2 -> string -> ('a, 'b) format2 option
val valid_format : ('a, 'b) format2 -> string -> ('a, 'b) format2

val transl : config -> string -> string
val transl_nth : config -> string -> int -> string
val transl_decline : config -> string -> string -> string
val transl_a_of_b : config -> string -> string -> string
val transl_a_of_gr_eq_gen_lev : config -> string -> string -> string
val ftransl : config -> ('a, 'b) format2 -> ('a, 'b) format2
val ftransl_nth : config -> ('a, 'b) format2 -> int -> ('a, 'b) format2
val fdecline : config -> ('a, 'b) format2 -> string -> ('a, 'b) format2
val fcapitale : ('a, 'b) format2 -> ('a, 'b) format2
val nth_field : string -> int -> string

val cftransl : config -> string -> string list -> string
val translate_eval : string -> string

val std_color : config -> string -> string

val index_of_sex : sex -> int

val relation_txt :
  config -> sex -> family -> (('a -> 'b) -> 'b, 'a, 'b) format

val string_of_decimal_num : config -> float -> string

val person_exists : config -> base -> string * string * int -> bool
val mark_if_not_public : config -> base -> string * string * int -> bool

val child_of_parent : config -> base -> person -> string

val find_person_in_env : config -> base -> string -> person option
(* Recherche le sosa uniquement dans le fichier gwf *)
val default_sosa_ref : config -> base -> person option
val find_sosa_ref : config -> base -> person option
val update_gwf_sosa : config -> base -> iper * (string * string * int) -> unit

val quote_escaped : string -> string

val get_server_string : config -> string
val get_request_string : config -> string

val get_server_string_aux : bool -> string list -> string
val get_request_string_aux : bool -> string list -> string

val create_topological_sort : config -> base -> int array

val branch_of_sosa :
  config -> base -> iper -> Num.t -> (iper * sex) list option
val sosa_of_branch : (iper * sex) list -> Num.t

val has_image : config -> base -> person -> bool
val image_file_name : string -> string
val source_image_file_name : string -> string -> string

val image_size : string -> (int * int) option
val limited_image_size :
  int -> int -> string -> (int * int) option -> (int * int) option
val image_and_size :
  config -> base -> person ->
    (string -> (int * int) option -> (int * int) option) ->
    (bool * string * (int * int) option) option

val default_image_name_of_key : string -> string -> int -> string
val default_image_name : base -> person -> string
val auto_image_file : config -> base -> person -> string option

val only_printable : string -> string
val only_printable_or_nl : string -> string

val relation_type_text : config -> relation_type -> int -> string
val rchild_type_text : config -> relation_type -> int -> string

val has_nephews_or_nieces : config -> base -> person -> bool

val browser_doesnt_have_tables : config -> bool

val start_with : string -> int -> string -> bool

val doctype : config -> string

val begin_centered : config -> unit
val end_centered : config -> unit

(* Printing for browsers without tables *)

val pre_text_size : string -> int
val print_pre_center : int -> string -> unit
val print_pre_left : int -> string -> unit
val print_pre_right : int -> string -> unit

val short_f_month : int -> string

val compilation_time_hook : (config -> string) ref
val compilation_time : config -> string

(* Reading password file *)

type auth_user = { au_user : string; au_passwd : string; au_info : string }

val read_gen_auth_file : string -> auth_user list

val is_that_user_and_password : auth_scheme_kind -> string -> string -> bool

(* Searching *)

val in_text : bool -> string -> string -> bool
val html_highlight : bool -> string -> string -> string

(* Pretty print XHTML wrapper for Wserver.wrap_string *)

val xml_pretty_print : string -> string

(* Print list in columns with alphabetic order *)

val wprint_in_columns :
  config -> ('a -> string) -> ('a -> unit) -> 'a list -> unit

(* Variable that use also private flag of person *)
val is_hide_names : config -> person -> bool
val is_hide_names_full : config -> base -> person -> bool

val reduce_list : int -> 'a list -> 'a list

val print_reference : config -> string -> int -> string -> unit

val gen_print_tips : config -> string -> unit
val print_tips_relationship : config -> unit

val print_image_sex : config -> person -> int -> unit

val display_options : config -> string
