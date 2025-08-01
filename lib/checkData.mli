type dict_type =
  | Fnames
  | Snames
  | Places
  | PubNames
  | Qualifiers
  | Aliases
  | Occupation
  | Estates
  | Titles
  | Sources

type error_type =
  | InvisibleCharacters
  | BadCapitalization
  | MultipleSpaces
  | NonBreakingSpace

val find_error_positions : error_type -> string -> int list
val fix_error : error_type -> string -> string
val make_error_html : Config.config -> string -> string -> error_type -> string

val collect_all_errors :
  ?max_results:int option ->
  ?selected_error_types:error_type list ->
  Geneweb_db.Driver.base ->
  dict_type ->
  (string * error_type list) list

val collect_all_errors_with_cache :
  ?max_results:int option ->
  ?selected_error_types:error_type list ->
  Config.config ->
  Geneweb_db.Driver.base ->
  dict_type ->
  (string * error_type list) list

val cache_file_exists : Config.config -> dict_type -> bool
val dict_to_cache_name : dict_type -> string