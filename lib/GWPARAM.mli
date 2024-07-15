val nb_errors : int ref
val errors_undef : string list ref
val errors_other : string list ref
val set_vars : string list ref
val gwd_cmd : string ref
val cnt_dir : string ref
val sock_dir : string ref
val bases : string ref
val reorg : bool ref
val force : bool ref

type my_fun_2 = string -> string
type my_fun_3 = string -> string -> string

val config : my_fun_2 ref
val cnt_d : my_fun_2 ref
val adm_file : my_fun_2 ref
val src_d : my_fun_2 ref
val etc_d : my_fun_2 ref
val lang_d : my_fun_3 ref
val bpath : my_fun_2 ref
val portraits_d : my_fun_2 ref
val images_d : my_fun_2 ref

type syslog_level =
  [ `LOG_EMERG  (** A panic condition. *)
  | `LOG_ALERT
    (** A condition that should be corrected immediately,
                                    such as a corrupted system database. *)
  | `LOG_CRIT  (** Critical conditions, such as hard device errors. *)
  | `LOG_ERR  (** Errors. *)
  | `LOG_WARNING  (** Warning messages.  *)
  | `LOG_DEBUG
    (** Conditions that are not error conditions,
                                    but that may require special handling. *)
  | `LOG_INFO  (** Informational messages. *)
  | `LOG_NOTICE
    (** Messages that contain information
                                    normally of use only when debugging a program.  *)
  ]
(** The level of log gravity. See SYSLOG(3) *)

(* S: Move it to gwd_lib?  *)

val init : string -> unit
(** Function called before gwd starts
    e.g. inititialise assets folders in Secure module. *)

val init_etc : string -> unit
(** Function called before gwd starts
    e.g. inititialise etc folders in reorg mode. *)

val output_error :
  (?headers:string list ->
  ?content:Adef.safe_string ->
  Config.config ->
  Def.httpStatus ->
  unit)
  ref
(** [!output_error ?headers ?content conf status] default function that send the http status [status].
    Also send [headers] and use [content] (typically a HTML string describing the error) if provided.
*)

val p_auth : (Config.config -> Gwdb.base -> Gwdb.person -> bool) ref
(** Check if a person should be displayed or not *)

val syslog : (syslog_level -> string -> unit) ref
(** [!syslog level log] log message [log] with gravity level [level] on stderr. *)

val wrap_output :
  (Config.config -> Adef.safe_string -> (unit -> unit) -> unit) ref
(** [wrap_output conf title content]
    Wrap the display of [title] and [content] in a defined template.
*)

val is_reorg_base : string -> bool
val test_reorg : string -> unit

module Reorg : sig
  val config : string -> string
  val cnt_d : string -> string
  val adm_file : string -> string
  val portraits_d : string -> string
  val src_d : string -> string
  val etc_d : string -> string
  val lang_d : string -> string -> string
  val images_d : string -> string

  val bpath : string -> string
  (** [Filename.concat (Secure.base_dir ())] *)
end

module Default : sig
  val config : string -> string
  val cnt_d : string -> string
  val adm_file : string -> string
  val portraits_d : string -> string
  val src_d : string -> string
  val etc_d : string -> string
  val lang_d : string -> string -> string
  val images_d : string -> string

  val bpath : string -> string
  (** [Filename.concat (Secure.base_dir ())] *)

  val output_error :
    ?headers:string list ->
    ?content:Adef.safe_string ->
    Config.config ->
    Def.httpStatus ->
    unit
  (** If [?content] is not set, sends page content from {/etc/<status-code>-<lang>.html}.
      If the current lang is not available, use `en` *)

  val p_auth : Config.config -> Gwdb.base -> Gwdb.person -> bool
  (** Calculate the access rights to the person's information in
      according to his age.
      Returns (in the order of the tests) :
      - `true` if requester is wizard or friend or person is public
      - `true` if person has at least one title and {i public_if_title}
        is set to {i yes} in gwf config file
      - `false` if person is alive and {i private_years} > 0
      - `true` if person is older (depending on the date of
        birth or baptism date) then {i privates_years}
      - `false` if person is younger (depending on the date of
        birth or baptism date) then {i privates_years}
      - `true` if person has been deceased for more than {i privates_years}
      - `false` if person has been deceased for less than {i privates_years}
      - `true` if person is between 80 and 120 years old and he is not beeing
        private and  {i public_if_no_date} is set to {i yes} in gwf config file
      - `true` if person has been married for more than {i private_years}
      - `false` otherwise
  *)

  val syslog : syslog_level -> string -> unit
  (** Prints on stderr using `"[date]: level message"` format. *)

  val wrap_output : Config.config -> Adef.safe_string -> (unit -> unit) -> unit
  (** Display in a very basic HTML doc, with no CSS or JavaScript. *)
end
