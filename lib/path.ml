(* Some global path, independant from base used. *)
let dir_etc_r = ref "etc"
let dir_lang_r = ref "lang"
let dir_gwd_k_r = ref "gwd_k"
let dir_cnt_r = ref "cnt"
let dir_cnt_base_r = ref "cnt"
let direct = ref false
let reorg = ref false

let adm_file_gwd f = Filename.concat !dir_cnt_r f
let adm_file_base f = Filename.concat !dir_cnt_base_r f

(* executed at start time by gwd *)
let init bases_dir =
  if !reorg then (* TODO better reorg *)
    dir_cnt_r := (String.concat Filename.dir_sep
      [bases_dir; "cnt"])
  else
    dir_cnt_r := (String.concat Filename.dir_sep
      [bases_dir; "cnt"])

let init_base bases_dir bname =
  if !reorg then
    dir_cnt_base_r := (String.concat Filename.dir_sep
      [bases_dir; bname ^ ".gwb"; "cnt"])
  else
    dir_cnt_base_r := (String.concat Filename.dir_sep
      [bases_dir; "cnt"])

let sharelib =
  Array.fold_left Filename.concat
    (try Sys.getenv "GWPREFIX" with Not_found -> "/usr") (* FIXME *)
    [| "share" ; "geneweb" |]

(* Lazy so that we can set [cnt] before actually accessing the file. *)
(* let gwd_lock_file = lazy (Filename.concat !dir_cnt "gwd.lck") *)

type t =
  { dir_root : string
  ; dir_bases : string
  ; dir_my_base : string
  ; dir_cnt : string
  ; dir_lang_d : string
  ; dir_lang_b : string
  ; dir_etc_d : string
  ; dir_etc_b : string
  ; dir_history : string
  ; dir_notes : string
  ; dir_wiznotes : string
  ; dir_icons : string
  ; dir_password : string
  ; dir_documents : string
  ; dir_portraits : string
  ; dir_portraits_bak : string
  ; dir_images : string
  ; file_base : string
  ; file_base_acc : string
  ; file_cache_info : string
  ; file_cache_visited : string
  ; file_cmd : string
  ; file_config : string
  ; file_counts : string
  ; file_fnames_dat : string
  ; file_fnames_inx : string
  ; file_forum : string
  ; file_friend_log : string
  ; file_history : string
  ; file_lock : string
  ; file_names_acc : string
  ; file_names_inx : string
  ; file_notes : string
  ; file_notes_aliases : string
  ; file_notes_links : string
  ; file_particles : string
  ; file_patches : string
  ; file_restrict : string
  ; file_snames_dat : string
  ; file_snames_inx : string
  ; file_strings_inx : string
  ; file_synchro_patches : string
  ; file_ts : string
  ; file_ts_visitor : string
  ; file_update_log : string
  ; file_wizard_log: string
  }

(* some paths are relative to the current base dir (dir_my_base) *)

let path_from_bname s =
  let fname = Filename.basename s in
  let bname =
    if Filename.check_suffix fname ".gwb" then Filename.chop_suffix fname ".gwb"
    else if 
      Filename.check_suffix fname ".gwb/" then Filename.chop_suffix fname ".gwb/"
    else fname
  in
  if !reorg then
  begin
    Printf.eprintf "**** Running in mode REORG\n";
    let bdir = bname ^ ".gwb" in
    let config_name = "config.txt" in
    let dir_bases = Secure.bases_dir () in (* -bd argument *)
    let dir_my_base = Filename.concat dir_bases bdir in
    let dir_cnt = !dir_cnt_r in
    let dir_etc_dist = !dir_etc_r in
    let dir_lang_dist = !dir_lang_r in
    let dir_etc_base = Filename.concat dir_my_base "etc" in
    let dir_icons = Filename.concat dir_etc_dist "images" in (* icons *)
    let dir_cnt_base = !dir_cnt_base_r in
    let file_particles = Filename.concat dir_etc_base "particles.txt" in
    let dir_lang_base = Filename.concat dir_etc_base "lang" in
    let dir_documents = Filename.concat dir_my_base "documents" in
    let dir_portraits = Filename.concat dir_documents "portraits" in
    let dir_portraits_bak = Filename.concat dir_portraits "saved" in
    let dir_images = Filename.concat dir_documents "images" in
    let dir_notes = Filename.concat dir_my_base "notes" in
    let dir_wiznotes = Filename.concat dir_my_base "wiznotes" in
    let dir_history = Filename.concat dir_my_base "history" in
    let file_config = Filename.concat dir_etc_base config_name in
    let file_cache_info = Filename.concat bdir "cache_info" in
    let file_cache_visited = Filename.concat bdir "cache_visited" in
    let file_wizard_log = Filename.concat dir_cnt_base "wizard.log" in
    let file_friend_log = Filename.concat dir_cnt_base "friends.log" in
    let file_counts = Filename.concat dir_cnt_base "counts.txt" in
    let file_lock = Filename.concat dir_cnt_base "lock" in
    let file_update_log = Filename.concat dir_cnt_base "update.log" in
    let file_notes = Filename.concat dir_notes "notes.txt" in
    let file_notes_links = Filename.concat dir_my_base "notes_links" in
    let file_notes_aliases = Filename.concat dir_my_base "notes.alias" in
    let file_history = Filename.concat dir_history "history.txt" in
    let file_forum = Filename.concat dir_my_base "forum.txt" in
    { dir_root = dir_my_base
    ; dir_bases = dir_bases
    ; dir_my_base = dir_my_base
    ; dir_password = dir_bases
    ; dir_documents = dir_documents
    ; dir_portraits = dir_portraits
    ; dir_portraits_bak = dir_portraits_bak
    ; dir_icons = dir_icons
    ; dir_images = dir_images
    ; dir_notes = dir_notes
    ; dir_wiznotes = dir_wiznotes
    ; dir_cnt = dir_cnt
    ; dir_lang_d = dir_lang_dist
    ; dir_lang_b = dir_lang_base
    ; dir_etc_d = dir_etc_dist
    ; dir_etc_b = dir_etc_base
    ; dir_history = dir_history
    ; file_notes = file_notes
    ; file_notes_aliases = file_notes_aliases
    ; file_notes_links = file_notes_links
    ; file_counts = file_counts
    ; file_lock = file_lock
    ; file_wizard_log = file_wizard_log
    ; file_friend_log = file_friend_log
    ; file_update_log = file_update_log
    ; file_forum = file_forum
    ; file_config = file_config
    ; file_history = file_history
    ; file_particles = file_particles
    ; file_cache_info = file_cache_info
    ; file_cache_visited = file_cache_visited
    ; file_base = String.concat Filename.dir_sep [dir_bases; (bname ^ ".gwb"); "base"]
    ; file_ts = Filename.concat dir_my_base "tstab"
    ; file_ts_visitor = Filename.concat dir_my_base "tstab_visitor"
    ; file_snames_dat = Filename.concat dir_my_base "snames.dat"
    ; file_snames_inx = Filename.concat dir_my_base "snames.inx"
    ; file_fnames_dat = Filename.concat dir_my_base "fnames.dat"
    ; file_fnames_inx = Filename.concat dir_my_base "fnames.inx"
    ; file_names_inx = Filename.concat dir_my_base "names.inx"
    ; file_names_acc = Filename.concat dir_my_base "names.acc"
    ; file_strings_inx = Filename.concat dir_my_base "strings.inx"
    ; file_base_acc = Filename.concat dir_my_base "base.acc"
    ; file_restrict = Filename.concat dir_my_base "restrict"
    ; file_patches = Filename.concat dir_my_base "patches"
    ; file_synchro_patches = Filename.concat dir_my_base "synchro_patches"
    ; file_cmd = Filename.concat dir_my_base "command.txt"
    }
  end else
  begin
    Printf.eprintf "**** Running in mode NORMAL\n";
    let bdir = bname ^ ".gwb" in
    let config_name = bname ^ ".gwf" in
    let dir_etc_dist = !dir_etc_r in
    let dir_lang_dist = !dir_lang_r in
    let dir_cnt = !dir_cnt_r in
    let dir_bases = Secure.bases_dir () in (* -bd argument *)
    let dir_my_base = Filename.concat dir_bases bdir in
    let dir_icons = Filename.concat dir_etc_dist "images" in (*icons *)
    let dir_etc_base = String.concat Filename.dir_sep [dir_bases; "etc"; bname] in
    let dir_cnt_base = dir_cnt in
    let dir_lang_base = String.concat Filename.dir_sep [dir_bases; "lang"; bname] in
    let dir_documents = String.concat Filename.dir_sep [dir_bases; "src"; bname] in
    let dir_portraits = String.concat Filename.dir_sep [dir_bases; "images"; bname] in
    let dir_portraits_bak = Filename.concat dir_portraits "old" in
    let dir_images = String.concat Filename.dir_sep [dir_bases; "src"; bname; "images"] in
    let dir_notes = Filename.concat dir_my_base "notes_d" in
    let dir_wiznotes = Filename.concat dir_my_base "wiznotes" in
    let dir_history = Filename.concat dir_my_base "history_d" in
    let file_cache_info = Filename.concat bdir "cache_info" in
    let file_cache_visited = Filename.concat bdir "cache_visited" in
    let file_particles = Filename.concat dir_my_base "particles.txt" in
    let file_wizard_log = Filename.concat dir_cnt_base "wizard.log" in
    let file_friend_log = Filename.concat dir_cnt_base "friends.log" in
    let file_counts = Filename.concat dir_cnt_base "counts.txt" in
    let file_lock = Filename.concat dir_cnt_base "lock" in
    let file_update_log = Filename.concat dir_cnt_base "update.log" in
    let file_notes = Filename.concat dir_my_base "notes" in
    let file_notes_links = Filename.concat dir_my_base "notes_links" in
    let file_notes_aliases = Filename.concat dir_my_base "notes.alias" in
    let file_history = Filename.concat dir_my_base "history" in
    let file_forum = Filename.concat dir_my_base "forum" in
    let file_config = Filename.concat dir_bases config_name in
  (* TODO merge dir_root and dir_my_base *)
    { dir_root = dir_my_base
    ; dir_bases = dir_bases
    ; dir_my_base = dir_my_base
    ; dir_password = dir_bases
    ; dir_documents = dir_documents
    ; dir_portraits = dir_portraits
    ; dir_portraits_bak = dir_portraits_bak
    ; dir_icons = dir_icons
    ; dir_images = dir_images
    ; dir_notes = dir_notes
    ; dir_wiznotes = dir_wiznotes
    ; dir_cnt = dir_cnt
    ; dir_lang_d = dir_lang_dist
    ; dir_lang_b = dir_lang_base
    ; dir_etc_d = dir_etc_dist
    ; dir_etc_b = dir_etc_base
    ; dir_history = dir_history
    ; file_notes = file_notes
    ; file_notes_aliases = file_notes_aliases
    ; file_notes_links = file_notes_links
    ; file_counts = file_counts
    ; file_lock = file_lock
    ; file_wizard_log = file_wizard_log
    ; file_friend_log = file_friend_log
    ; file_update_log = file_update_log
    ; file_forum = file_forum
    ; file_config = file_config
    ; file_history = file_history
    ; file_particles = file_particles
    ; file_cache_info = file_cache_info
    ; file_cache_visited = file_cache_visited
    ; file_base = String.concat Filename.dir_sep [dir_bases; (bname ^ ".gwb"); "base"]
    ; file_ts = Filename.concat dir_my_base "tstab"
    ; file_ts_visitor = Filename.concat dir_my_base "tstab_visitor"
    ; file_snames_dat = Filename.concat dir_my_base "snames.dat"
    ; file_snames_inx = Filename.concat dir_my_base "snames.inx"
    ; file_fnames_dat = Filename.concat dir_my_base "fnames.dat"
    ; file_fnames_inx = Filename.concat dir_my_base "fnames.inx"
    ; file_names_inx = Filename.concat dir_my_base "names.inx"
    ; file_names_acc = Filename.concat dir_my_base "names.acc"
    ; file_strings_inx = Filename.concat dir_my_base "strings.inx"
    ; file_base_acc = Filename.concat dir_my_base "base.acc"
    ; file_restrict = Filename.concat dir_my_base "restrict"
    ; file_patches = Filename.concat dir_my_base "patches"
    ; file_synchro_patches = Filename.concat dir_my_base "synchro_patches"
    ; file_cmd = Filename.concat dir_my_base "command.txt"
    }
  end
