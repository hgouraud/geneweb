open Def
open Gwdb

(** Authentication scheme data type *)
type auth_scheme_kind =
  | NoAuth
  | TokenAuth of token_auth_scheme
  | HttpAuth of http_auth_scheme

and token_auth_scheme = { ts_user : string; ts_pass : string }
(** Authentication via security token *)

(** Authentication via HTTP *)
and http_auth_scheme =
  | Basic of basic_auth_scheme
  | Digest of digest_auth_scheme

and basic_auth_scheme = {
  bs_realm : string;
  bs_user : string;
  bs_pass : string;
}
(** Basic authentication scheme inside {i Autorization} HTTP header *)

and digest_auth_scheme = {
  ds_username : string;
  ds_realm : string;
  ds_nonce : string;
  ds_meth : string;
  ds_uri : string;
  ds_qop : string;
  ds_nc : string;
  ds_cnonce : string;
  ds_response : string;
}
(** Digest authentication scheme inside {i Autorization} HTTP header *)

type output_conf = {
  status : Def.httpStatus -> unit;
  header : string -> unit;
  body : string -> unit;
  flush : unit -> unit;
}
(** HTTP printer, that prints and sends requests on the user's socket *)

type env = (string * Adef.encoded_string) list

type config = {
  access_by_key : bool;
  allowed_titles : string list Lazy.t;
  api_mode : bool;
  auth_file : string;
  auth_scheme : auth_scheme_kind;
  authorized_wizards_notes : bool;
  base_env : (string * string) list (* content of .gwf file *);
  bname : string;
  border : int;
  cgi : bool;
  cgi_passwd : string;
  command : string;
  ctime : float; (* TODO verify usefulness *)
  debug : bool;
  default_lang : string;
  default_sosa_ref : iper * Gwdb.person option;
  denied_titles : string list Lazy.t;
  env : env;
  forced_plugins : string list;
  friend : bool;
  from : string;
  hide_names : bool;
  highlight : string;
  icon_prefix : string;
  (* prefix for image urls:
     the value of argument -images_url if specified, otherwise
     command ^ "?m=IM&v=" in CGI mode
     "images" otherwise *)
  image_prefix : string;
  indep_command : string;
  is_printed_by_template : bool;
  is_rtl : bool;
  just_friend_wizard : bool;
  lang : string;
  left : string;
  lexicon : (string, string) Hashtbl.t;
  manitou : bool;
  multi_parents : bool;
  mutable charset : string;
  mutable henv : env;
  mutable n_connect : (int * int * int * (string * float) list) option;
  mutable output_conf : output_conf;
  (* HTTP printer *)
  mutable senv : env;
  mutable setup_link : bool;
  no_image : bool;
  no_note : bool;
  path : Path.t;
  plugins : string list;
  private_years : int;
  public_if_no_date : bool;
  public_if_titles : bool;
  query_start : float;
  request : string list;
  right : string;
  static_path : string;
  (* in CGI mode, provides location of etc files to Apache for direct loading *)
  (* in CGI, the base name is in the b argument of the query string: ?b=BASE&...
     if not, the base name is the last element of the uri path: .../base?... *)
  supervisor : bool;
  time : int * int * int;
  today : dmy;
  today_wd : int;
  use_restrict : bool;
  user : string;
  username : string;
  wizard : bool;
}
(** Geneweb configuration data type *)

val empty : config
(** A dummy {!type:config} value, with uninitialized fields.
    Used for testing purpose *)
