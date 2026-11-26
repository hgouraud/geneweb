open Alcotest
module Image = Geneweb.Image

let in_temp_bases f () =
  let prev = Secure.bases_dir () in
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("gw_saved_portraits_test_" ^ string_of_int (Random.bits ()))
  in
  Secure.set_bases_dir dir;
  Fun.protect
    ~finally:(fun () ->
      Mutil.rm_rf dir;
      Secure.set_bases_dir prev)
    f

let touch file content =
  Filesystem.create_dir ~parent:true (Filename.dirname file);
  Secure.with_open_out_bin file (fun oc -> output_string oc content)

let read file = Secure.with_open_in_bin file In_channel.input_all
let sorted dir = Array.to_list (Sys.readdir dir) |> List.sort compare

let tm =
  {
    Unix.tm_sec = 0;
    tm_min = 42;
    tm_hour = 0;
    tm_mday = 26;
    tm_mon = 10;
    tm_year = 126;
    tm_wday = 0;
    tm_yday = 0;
    tm_isdst = false;
  }

let stem () =
  let dir = Filename.concat (Secure.bases_dir ()) "saved" in
  Filesystem.create_dir ~parent:true dir;
  (check string) "portrait" "portrait-20261126-004200"
    (Image.saved_portrait_stem dir "portraits" tm);
  touch (Filename.concat dir "portrait-20261126-004200.png") "";
  (check string) "taken" "portrait-20261126-004200-1"
    (Image.saved_portrait_stem dir "portraits" tm);
  (check string) "blason" "blason-20261126-004200"
    (Image.saved_portrait_stem dir "blasons" tm)

let move () =
  let layout = Layout.of_bname ~mode:Layout.Reorg "b" in
  let old = Filename.concat (Layout.portraits layout) "saved" in
  touch (Filename.concat old "jean.0.dupont.jpg") "portrait";
  touch (Filename.concat old "jean.0.dupont.txt") "note";
  touch (Filename.concat old "jean.0.dupont.blason.png") "blason";
  touch (Filename.concat old "tempfile.tmp") "";
  (check int) "dry run" 2 (Image.move_saved_portraits ~dry_run:true layout);
  (check int) "nothing moved" 4 (List.length (sorted old));
  (check int) "moved" 2 (Image.move_saved_portraits ~dry_run:false layout);
  let saved =
    String.concat Filename.dir_sep
      [ Layout.images layout; "jean.0.dupont"; "saved" ]
  in
  let files = sorted saved in
  (check (list string))
    "kinds" [ ".png"; ".jpg"; ".txt" ]
    (List.map Filename.extension files);
  let file prefix ext =
    List.find
      (fun f -> String.starts_with ~prefix f && Filename.extension f = ext)
      files
  in
  (check string) "portrait" "portrait"
    (read (Filename.concat saved (file "portrait-" ".jpg")));
  (check string) "note with its portrait"
    (Filename.remove_extension (file "portrait-" ".jpg"))
    (Filename.remove_extension (file "portrait-" ".txt"));
  (check string) "blason" "blason"
    (read (Filename.concat saved (file "blason-" ".png")));
  (check (list string)) "left" [ "tempfile.tmp" ] (sorted old)

let v =
  [
    ( "saved-portraits",
      [
        test_case "stem" `Quick (in_temp_bases stem);
        test_case "move" `Quick (in_temp_bases move);
      ] );
  ]
