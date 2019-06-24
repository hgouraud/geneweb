(* Some global path, independant from base used. *)
let etc = ref "etc"
let lang = ref "lang"
let cnt = ref "cnt"
let reorg = ref true
(* all three paths are reset by gwDaemon with the -hd parameter
   (if present) at start time
*)
let sharelib =
  Array.fold_left Filename.concat
    (try Sys.getenv "GWPREFIX" with Not_found -> "/usr") (* FIXME *)
    [| "share" ; "geneweb" |]
let direct = ref false

(* Lazy so that we can set [cnt] before actually accessing the file. *)
let gwd_lock_file = lazy (Filename.concat !cnt "gwd.lck")

type t =
  { dir_documents : string
  ; dir_portraits : string
  ; dir_portraits_bak : string
  ; file_conf : string
  ; dir_notes : string
  ; dir_wiznotes : string
  ; dir_root : string
  ; dir_icons : string
  ; dir_images : string
  ; dir_password : string
  ; dir_bases : string
  ; dir_my_base : string
  ; dir_bin : string
  ; dir_cnt : string
  ; dir_lang : string
  ; dir_etc_d : string
  ; dir_etc_b : string
  ; dir_history : string
  ; file_ts : string
  ; file_ts_visitor : string
  ; file_restrict : string
  ; file_synchro_patches : string
  ; file_cmd : string
  ; file_base : string
  ; file_particles : string
  ; file_base_acc : string
  ; file_strings_inx : string
  ; file_names_inx : string
  ; file_names_acc : string
  ; file_patches : string
  ; file_notes_aliases : string
  ; file_forum : string
  ; file_history : string
  ; file_notes : string
  ; notes_name : string
  ; file_notes_links : string
  ; file_snames_dat : string
  ; file_snames_inx : string
  ; file_fnames_dat : string
  ; file_fnames_inx : string
  ; file_wizard_log: string
  ; file_friend_log : string
  ; file_counts : string
  ; file_lock : string
  ; file_update_log : string
  ; file_cache_info : string
  }

(* some paths are relative to the current base dir (dir_my_base) *)

let path_from_bname s =
  let bname =
    if Filename.check_suffix s ".gwb" then Filename.chop_suffix s ".gwb"
    else if Filename.check_suffix s ".gwb/" then Filename.chop_suffix s ".gwb/"
    else s
  in
  let bdir = bname ^ ".gwb" in
  let dir_bin = Filename.current_dir_name in (* TODO where gw sits -hd??*)
  let dir_bases = Secure.base_dir () in (* -bd argument *)
  let dir_my_base = Filename.concat dir_bases bdir in
  let dir_icons = Filename.concat dir_bases "images" in
  let dir_etc_dist = Filename.concat dir_bin "etc" in
  let dir_etc_base = Filename.concat dir_my_base "etc" in
  let dir_cnt = Filename.concat dir_etc_base "cnt" in
  let dir_lang = Filename.concat dir_etc_base "lang" in
  let dir_documents = Filename.concat dir_my_base "documents" in
  let dir_portraits = Filename.concat dir_documents "portraits" in
  let dir_images = Filename.concat dir_documents "images" in
  let config_name = "config.txt" in
  let dir_notes = Filename.concat dir_my_base "notes" in
  let notes_name = "notes.txt" in
  let dir_history = Filename.concat dir_my_base "history" in
  { file_conf = Filename.concat dir_etc_base config_name
  ; dir_root = dir_my_base
  ; dir_bin = dir_bin
  ; dir_bases = dir_bases
  ; dir_my_base = dir_my_base
  ; dir_password = dir_bases
  ; dir_documents = dir_documents
  ; dir_portraits = dir_portraits
  ; dir_icons = dir_icons
  ; dir_images = dir_images
  ; dir_notes = dir_notes
  ; notes_name = notes_name
  ; dir_cnt = dir_cnt
  ; dir_lang = dir_lang
  ; dir_etc_d = dir_etc_dist
  ; dir_etc_b = dir_etc_base
  ; dir_history = dir_history
  ; dir_wiznotes = Filename.concat bdir "wiznotes"
  ; dir_portraits_bak = Filename.concat dir_portraits "saved"
  ; file_ts = Filename.concat bdir "tstab"
  ; file_ts_visitor = Filename.concat bdir "tstab_visitor"
  ; file_snames_dat = Filename.concat bdir "snames.dat"
  ; file_snames_inx = Filename.concat bdir "snames.inx"
  ; file_fnames_dat = Filename.concat bdir "fnames.dat"
  ; file_fnames_inx = Filename.concat bdir "fnames.inx"
  ; file_restrict = Filename.concat bdir "restrict"
  ; file_synchro_patches = Filename.concat bdir "synchro_patches"
  ; file_cmd = Filename.concat dir_my_base "command.txt"
  ; file_base = Filename.concat bdir "base"
  ; file_particles = Filename.concat bdir "particles.txt"
  ; file_base_acc = Filename.concat bdir "base.acc"
  ; file_strings_inx = Filename.concat bdir "strings.inx"
  ; file_names_inx = Filename.concat bdir "names.inx"
  ; file_names_acc = Filename.concat bdir "names.acc"
  ; file_patches = Filename.concat bdir "patches"
  ; file_notes = Filename.concat bdir notes_name
  ; file_notes_aliases = Filename.concat dir_my_base "notes.alias"
  ; file_forum = Filename.concat dir_my_base "forum"
  ; file_history = Filename.concat dir_history "history.txt"
  ; file_notes_links = Filename.concat dir_my_base "notes_links"
  ; file_wizard_log = Filename.concat dir_cnt "wizard.log"
  ; file_friend_log = Filename.concat dir_cnt "friends.log"
  ; file_counts = Filename.concat dir_cnt "counts.txt"
  ; file_lock = Filename.concat dir_cnt "lock"
  ; file_update_log = Filename.concat dir_cnt "update.log"
  ; file_cache_info = Filename.concat bdir "cache_info"
  }
