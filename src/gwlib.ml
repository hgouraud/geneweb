let prefix =
  try Sys.getenv "GWPREFIX" with
    Not_found ->
    let bin_name = Sys.executable_name in
    let bin_dir = Filename.dirname bin_name in
    let prefix_dir = Filename.dirname bin_dir in
    prefix_dir
