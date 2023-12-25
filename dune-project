(lang dune 2.8)

(name geneweb)
(version v6.08-207-g8b66933)
(formatting (enabled_for ocaml reason))

(package
 (name geneweb)
 (synopsis "Genealogy software with web interface")
 (description
 "Geneweb allows keeping track of ancestral data. It is a powerful
 system for maintaining a set of data about your family history. It
 supports much of the GEDCOM tag system for data storage, and can be
 used either as your primary system for archiving genealogical data,
 or as a web service (through a CGI interface) for publishing your
 data for others to use.")
 (depends
   (ocaml (>= 4.10.0))
   (camlp5 (= 7.14))
   ocamlfind
  )
 )
