
# Geneweb

## Installation de Geneweb 608

Dépendances:
* Camlp5 7.14
* ocamlfind


* OCaml version 4.10.0: erreur de compilation dans pa_macro.ml

```
opam switch link 4.10.0
eval $(opam env)
```

* Camlp5

```
opam install camlp5
```

* Build sequence

```
./configure
```

Edit tools/Makefile.inc and tools/Makefile.ocaml
* replace +camlp5 by +../camlp5
* echo PREFIX=/opt/geneweb-608 >> tools/Makefile.inc

```
make
```

```
sudo make install
```

## Installation Architecture

Dans `${PREFIX}/bin`:

* `gwc`: copy of `gwc1`
* `gwc1`: creates a starter database for use by GeneWeb.
* `gwc2`: creates a database for use by GeneWeb.
* `consang`: calculates  the consanguinities in the EGeneweb databas
* `gwb2ged`: dumps a GeneWeb database to a GEDCOM file
* `gwd`: launches the GeneWeb "daemon" process
* `gwu`: dumps the contents of the GeneWeb database to standard output.
* `ged2gwb`: creates a starter database for use by GeneWeb , from a GEDCOM file.
* `ged2gwb2`: creates a starter database for use by GeneWeb , from a GEDCOM file.


Dans `${PREFIX}/share/geneweb`:

* `etc/` : templates
* `images/` : images
* `lang/` : translations

## Contributions/TODOs:

* Faire un paquet `opam` pour Geneweb
* Utiliser `dune` pour builder
* Passer en syntaxe normale:

## Conversion from Revised Syntax to OCaml Syntax


Juste to check the syntax for one file:
```
camlp5r pr_o.cmo FILE.ml
```

A generic command looks like
```
for i in $(cat ml_files ); do
base=$(basename $i .ml)
dir=$(dirname $i)
camlp5r pr_o.cmo src/pa_html.cmo src/pa_lock.cmo pa_extend.cmo wserver/pa_macro5.cmo -DUNIX -impl $dir/$base.ml4 > $i || (rm -f $i && echo "ERROR with $i")
done
```

Errors may come from:
* undefined macro variables, such as `-DUNIX`
* undefined macros (need to add more .cmo files, including some built in the project)

Commands that have to be run manually:

```
camlp5r pr_o.cmo src/q_codes.cmo wserver/pa_macro5.cmo -DUNIX -impl src/iovalue.ml4 > src/iovalue.ml
camlp5r pr_o.cmo src/pa_lock.cmo wserver/pa_macro5.cmo -DUNIX -impl wserver/wserver.ml4 > wserver/wserver.ml
camlp5r pr_o.cmo src/pa_lock.cmo wserver/pa_macro5.cmo -DUNIX -impl gwtp/gwtp.ml4 > gwtp/gwtp.ml
camlp5r pr_o.cmo src/q_codes.cmo wserver/pa_macro5.cmo -DUNIX -impl src/db2out.ml4 > src/db2out.ml
camlp5r pr_o.cmo src/q_codes.cmo wserver/pa_macro5.cmo -DUNIX -impl src/db2link.ml4 > src/db2link.ml
camlp5r pr_o.cmo src/pa_lock.cmo wserver/pa_macro5.cmo -DUNIX -impl src/mk_consang.ml4 > src/mk_consang.ml
camlp5r -I src pr_o.cmo wserver/pa_macro5.cmo -DUNIX -impl contrib/gwbase/etc/gwck.ml4 > contrib/gwbase/etc/gwck.ml
camlp5r -I src pr_o.cmo wserver/pa_macro5.cmo -DUNIX -impl contrib/gwbase/etc/selroy.ml4 > contrib/gwbase/etc/selroy.ml
```

Some errors that are output:
* ERROR with ./contrib/lex/lex_utils.ml
* ERROR with ./contrib/gwbase/etc/gwck.ml
* ERROR with ./wserver/pa_macro5.ml
* ERROR with ./src/pa_html.ml
* ERROR with ./src/pr_transl.ml
* ERROR with ./src/pr_html.ml
* ERROR with ./src/pr_dep.ml
* ERROR with ./src/pr_lock.ml
* ERROR with ./src/pa_lock.ml

Also for interface files:

```
for i in $(cat mli_files ); do
base=$(basename $i .mli)
dir=$(dirname $i)
mv $dir/$base.mli $dir/$base.mli4
camlp5r pr_o.cmo src/pa_html.cmo src/pa_lock.cmo pa_extend.cmo  wserver/pa_macro5.cmo -DUNIX -intf $dir/$base.mli4 > $i
done
```

* Camlp5 grammars seems to be used in:
  * `src/srcfile.ml`
     Parse a date DAY/MONTH/YEAR
  * `ged2gwb/ged2gwb.ml` (format GEDCOM)
  * `ged2gwb/ged2gwb2.ml` (format GEDCOM)

## Structure of the `gwc2` database

Creation d'une base:

```
./gwc2 -f -o base-simple
```

The following files were generated for this command:


* `base-simple.gwb/base_d/person_of_name/person_of_name.hta`
* `base-simple.gwb/base_d/person_of_name/person_of_name.ht` with equivalent type
  `[ (string,int) Hashtbl.t ]`:
  (a list of names is generated for every person, and then associated to
   that person identifier)


* `base-simple.gwb/base_d/particles.txt`


* `base-simple.gwb/base_d/patches` : modifications on the database are
  performed asynchronously by adding patches, and a new database is
  generated once a day by applying these patches to the former database


* `base-simple.gwb/base_d/person_of_key/iper_of_key.ht`
* `base-simple.gwb/base_d/person_of_key/iper_of_key.hta`  with equivalent type
  `(Db2.key2, iper) Hashtbl.t`


* base-simple.gwb/base_d/person_of_key/istr_of_string.ht
* base-simple.gwb/base_d/person_of_key/istr_of_string.hta  with equivalent type
  (string, Adef.istr) Hashtbl.t


* base-simple.gwb/base_d/wiznotes_d/<wizid>.txt [STRING CONTENT]


(* person_d *)
* base-simple.gwb/base_d/person/access/access
* base-simple.gwb/base_d/person/access/data          [FIELD Def.access]
  [type access = IfTitles | Public | Private | Friend | Friend_m]


* base-simple.gwb/base_d/person/aliases/access
* base-simple.gwb/base_d/person/aliases/data         [FIELD string list]
* base-simple.gwb/base_d/person/aliases/data2.ext


* base-simple.gwb/base_d/person/birth_place/access
* base-simple.gwb/base_d/person/birth_place/data     [FIELD string]


* base-simple.gwb/base_d/person/parents/access
* base-simple.gwb/base_d/person/parents/data         [VALUE int]



* base-simple.gwb/base_d/person/first_name/access
* base-simple.gwb/base_d/person/first_name/data         [FIELD string]


* base-simple.gwb/base_d/person/first_name/string_of_crush.ht
* base-simple.gwb/base_d/person/first_name/string_of_crush.hta
   (string,int) Hashtbl.t
   (`string` to string position in `data` file)


* base-simple.gwb/base_d/person/first_name/person_of_string.ht
* base-simple.gwb/base_d/person/first_name/person_of_string.hta
   (int,int) Hashtbl.t
   (string position in `data` file to person identifier)


* base-simple.gwb/base_d/person/first_name/index.dat
  (ONE VALUE (string * int) array)
* base-simple.gwb/base_d/person/first_name/index.acc
  (positions of values in `index.dat`)
* base-simple.gwb/base_d/person/first_name/index.ini
  [ONE VALUE (string*int) list, where `int` is the index (not position)
    in the array `index.dat`. Used as Index Of First Char for faster
    lookup]


* base-simple.gwb/base_d/person/baptism_place/access
* base-simple.gwb/base_d/person/baptism_place/data     [FIELD string]

* base-simple.gwb/base_d/person/occ/access
* base-simple.gwb/base_d/person/occ/data               [FIELD int]

* base-simple.gwb/base_d/person/image/access
* base-simple.gwb/base_d/person/image/data             [FIELD string]

* base-simple.gwb/base_d/person/burial/access
* base-simple.gwb/base_d/person/burial/data            [FIELD Def.burial]
  [type burial = UnknownBurial | Buried of codate | Cremated of codate]

* base-simple.gwb/base_d/person/occupation/access
* base-simple.gwb/base_d/person/occupation/data       [FIELD string]

* base-simple.gwb/base_d/person/surnames_aliases/access
* base-simple.gwb/base_d/person/surnames_aliases/data      [FIELD string list]
* base-simple.gwb/base_d/person/surnames_aliases/data2.ext

* base-simple.gwb/base_d/person/notes/access
* base-simple.gwb/base_d/person/notes/data        [VALUE string]

* base-simple.gwb/base_d/person/birth/access
* base-simple.gwb/base_d/person/birth/data      [FIELD Adef.codate]
  [type codate = ... [complex] ]
  
* base-simple.gwb/base_d/person/rparents/access
* base-simple.gwb/base_d/person/rparents/data  [VALUE (Def.iper, int) Def.gen_relation list]

* base-simple.gwb/base_d/person/death_src/access
* base-simple.gwb/base_d/person/death_src/data      [FIELD string]

* base-simple.gwb/base_d/person/burial_place/access
* base-simple.gwb/base_d/person/burial_place/data  [FIELD string]

* base-simple.gwb/base_d/person/surname/access
* base-simple.gwb/base_d/person/surname/data       [FIELD string]

* base-simple.gwb/base_d/person/surname/string_of_crush.ht
* base-simple.gwb/base_d/person/surname/string_of_crush.hta
   (string,int) Hashtbl.t

* base-simple.gwb/base_d/person/surname/person_of_string.ht
* base-simple.gwb/base_d/person/surname/person_of_string.hta
   (int,int) Hashtbl.t

* base-simple.gwb/base_d/person/surname/index.dat
* base-simple.gwb/base_d/person/surname/index.ini [VALUE (string*int) list]
* base-simple.gwb/base_d/person/surname/index.acc

* base-simple.gwb/base_d/person/birth_src/access
* base-simple.gwb/base_d/person/birth_src/data  [FIELD string]

* base-simple.gwb/base_d/person/death_place/access
* base-simple.gwb/base_d/person/death_place/data  [FIELD string]

* base-simple.gwb/base_d/person/death/access
* base-simple.gwb/base_d/person/death/data         [FIELD Def.death]

* base-simple.gwb/base_d/person/first_names_aliases/access
* base-simple.gwb/base_d/person/first_names_aliases/data      [FIELD string list]
* base-simple.gwb/base_d/person/first_names_aliases/data2.ext

* base-simple.gwb/base_d/person/burial_src/access
* base-simple.gwb/base_d/person/burial_src/data  [FIELD string]

* base-simple.gwb/base_d/person/baptism_src/access
* base-simple.gwb/base_d/person/baptism_src/data  [FIELD string]

* base-simple.gwb/base_d/person/sex/access
* base-simple.gwb/base_d/person/sex/data           [FIELD Def.sex]
  [type sex = Male | Female | Neuter]

* base-simple.gwb/base_d/person/related/access
* base-simple.gwb/base_d/person/related/data       [NOVALUE int, NOVALUE int]
  (linked-list of ints, second int is the position of the next cell or -1)
  (NOVALUE = value_no_header)

* base-simple.gwb/base_d/person/qualifiers/access
* base-simple.gwb/base_d/person/qualifiers/data        [FIELD string list]
* base-simple.gwb/base_d/person/qualifiers/data2.ext

* base-simple.gwb/base_d/person/family/access
* base-simple.gwb/base_d/person/family/data [unions]
  This file is created in 2 steps.
  Incrementally as:
  [VALUE int, VALUE int)
  (linked-list of ints, second int is the position of the next cell or -1)
  Then, it is "reordered" to become a standard [FIELD int array] file.

* base-simple.gwb/base_d/person/titles/access
* base-simple.gwb/base_d/person/titles/data   [FIELD string Def.gen_title list]
* base-simple.gwb/base_d/person/titles/data2.ext
  (contains only non-nil lists, others have -1 as position in access)

* base-simple.gwb/base_d/person/psources/access
* base-simple.gwb/base_d/person/psources/data  [FIELD string]

* base-simple.gwb/base_d/person/baptism/access
* base-simple.gwb/base_d/person/baptism/data   [FIELD Adef.codate]

* base-simple.gwb/base_d/person/public_name/access
* base-simple.gwb/base_d/person/public_name/data      [FIELD string]

* base-simple.gwb/base_d/family/fsources/access
* base-simple.gwb/base_d/family/fsources/data     [FIELD string]

* base-simple.gwb/base_d/family/children/access
* base-simple.gwb/base_d/family/children/data      [FIELD Adef.iper array]

* base-simple.gwb/base_d/family/marriage_place/access
* base-simple.gwb/base_d/family/marriage_place/data  [FIELD string]

* base-simple.gwb/base_d/family/relation/access
* base-simple.gwb/base_d/family/relation/data      [FIELD Def.relation_kind]

* base-simple.gwb/base_d/family/origin_file/access
* base-simple.gwb/base_d/family/origin_file/data     [FIELD string]

* base-simple.gwb/base_d/family/divorce/access
* base-simple.gwb/base_d/family/divorce/data       [FIELD Def.divorce]

* base-simple.gwb/base_d/family/marriage/access
* base-simple.gwb/base_d/family/marriage/data      [FIELD Adef.codate]

* base-simple.gwb/base_d/family/witnesses/access
* base-simple.gwb/base_d/family/witnesses/data    [FIELD Def.iper array]

* base-simple.gwb/base_d/family/comment/access
* base-simple.gwb/base_d/family/comment/data        [FIELD string]

* base-simple.gwb/base_d/family/mother/access
* base-simple.gwb/base_d/family/mother/data        [FIELD Adef.iper]

* base-simple.gwb/base_d/family/father/access
* base-simple.gwb/base_d/family/father/data        [FIELD Adef.iper]

* base-simple.gwb/base_d/family/marriage_src/access
* base-simple.gwb/base_d/family/marriage_src/data  [FIELD string]

* base-simple.gwb/command.txt
* base-simple.gwb/cache_person_linked_pages
* base-simple.gwb/notes_links
* base-simple.gwb/tstab
* base-simple.gwf
* base-simple.lck

Gwdb: offre une interface unique au dessus de Db1 et Db2
Mk_consang: optimize database ?

Gwc2:
Db2:
Db2out: fonctions d'output
Db2disk: fonctions d'accès directes aux fichiers
Db2link: génération des fichiers

Gwc:
Db1disk (was Database)
Db1link: génération des fichiers


## FORMAT:

### HASHTBLS:

La table de hachage est lisible directement sur le disque: Le .hta est
la bucketlist (indexé par le hash de la valeur), puis la valeur est
directement lue à la position voulue.

FILE.ht:
*  value_header
*  block_header   ht.repr.length
*  Iovalue.output ht.size
*  block_header    hashtbl.data.length
*  Iovalue.output hashtbl.data.[0..SIZE]
*  hashtbl.seed
*  hashtbl.initial_size

FILE.hta
*  position 0: binary_int = ht.data.length
*  positions: positions de toutes les data


value_header:
*   4 bytes: 0x84; 0x95; 0xA6; 0xBE (* Db2out *)
*   4 bytes: block length
*   4 bytes: obj counter
*   4 bytes: size_32
*   4 bytes: size_64

### FIELDS

DIR/FILE/access: position in the [data] file of every field
DIR/FILE/data: value_header + VALUE + VALUE + VALUE ...

Sometimes, `data2.ext` is for list values: it contains only non-`nil`
values, while the `nil` values have position -1 in the `access` file.
If the `data2.ext` file is present, the offsets in `access` are to be
used with it, not with `data`.

If a `data` file contains strings, it is compressed by storing
only once each string.

### NON FIELDS

Same as FIELDS, but without the value_header.
*   g_person_parents : Iochan.t * out_channel;
*   g_person_unions : Iochan.t * out_channel;
*   g_person_rparents : Iochan.t * out_channel;
*   g_person_related : Iochan.t * Iochan.t;
*   g_person_notes : Iochan.t * out_channel }

### Sum up of types

In general, we have:

* `'a array` where 'a can be encoded/decoded to `string`. These arrays can
  be compressed by storing each encoded string only once.

  Current format:
  * RECORD/FIELD/access
  * RECORD/FIELD/data      (data)
  * RECORD/FIELD/data2.ext (compressed version)

* `('a, 'b) Hashtbl.t` where 'a and 'b can be encoded/decoded to `string`

  * RECORD/FIELD/hashtbl.ht
  * RECORD/FIELD/hashtbl.hta

* `'a array` where 'a can be encoded/decoded to `string` AND the array is
   sorted in increasing order, and provides easy dichotomy lookup

   * RECORD/FIELD/index.dat (array (string -> pos in data))
   * RECORD/FIELD/index.acc (index -> pos in index.dat)
   * RECORD/FIELD/index.ini (Index of First Char)

### Undocumented (yet)

* base-simple.gwb/base_d/particles.txt
* base-simple.gwb/base_d/patches
* base-simple.gwb/base_d/wiznotes_d/<wizid>.txt [STRING CONTENT]


* base-simple.gwb/base_d/person/first_name/index.dat
  (ONE VALUE (string * int) array)
* base-simple.gwb/base_d/person/first_name/index.acc
  (positions of values in `index.dat`)
* base-simple.gwb/base_d/person/first_name/index.ini
  [ONE VALUE (string*int) list, where `int` is the index (not position)
    in the array `index.dat`. Used as Index Of First Char for faster
    lookup]

* base-simple.gwb/base_d/person/occupation/access
* base-simple.gwb/base_d/person/occupation/data       [FIELD string]

* base-simple.gwb/base_d/person/surnames_aliases/access
* base-simple.gwb/base_d/person/surnames_aliases/data      [FIELD string list]
* base-simple.gwb/base_d/person/surnames_aliases/data2.ext

* base-simple.gwb/base_d/person/notes/access
* base-simple.gwb/base_d/person/notes/data        [VALUE string]

* base-simple.gwb/base_d/person/birth/access
* base-simple.gwb/base_d/person/birth/data      [FIELD Adef.codate]
  [type codate = ... [complex] ]
  
* base-simple.gwb/base_d/person/rparents/access
* base-simple.gwb/base_d/person/rparents/data  [VALUE (Def.iper, int) Def.gen_relation list]

* base-simple.gwb/base_d/person/death_src/access
* base-simple.gwb/base_d/person/death_src/data      [FIELD string]

* base-simple.gwb/base_d/person/burial_place/access
* base-simple.gwb/base_d/person/burial_place/data  [FIELD string]

* base-simple.gwb/base_d/person/surname/access
* base-simple.gwb/base_d/person/surname/data       [FIELD string]

* base-simple.gwb/base_d/person/surname/string_of_crush.ht
* base-simple.gwb/base_d/person/surname/string_of_crush.hta
   (string,int) Hashtbl.t

* base-simple.gwb/base_d/person/surname/person_of_string.ht
* base-simple.gwb/base_d/person/surname/person_of_string.hta
   (int,int) Hashtbl.t

* base-simple.gwb/base_d/person/surname/index.dat
* base-simple.gwb/base_d/person/surname/index.ini [VALUE (string*int) list]
* base-simple.gwb/base_d/person/surname/index.acc

* base-simple.gwb/base_d/person/birth_src/access
* base-simple.gwb/base_d/person/birth_src/data  [FIELD string]

* base-simple.gwb/base_d/person/death_place/access
* base-simple.gwb/base_d/person/death_place/data  [FIELD string]

* base-simple.gwb/base_d/person/death/access
* base-simple.gwb/base_d/person/death/data         [FIELD Def.death]

* base-simple.gwb/base_d/person/first_names_aliases/access
* base-simple.gwb/base_d/person/first_names_aliases/data      [FIELD string list]
* base-simple.gwb/base_d/person/first_names_aliases/data2.ext

* base-simple.gwb/base_d/person/burial_src/access
* base-simple.gwb/base_d/person/burial_src/data  [FIELD string]

* base-simple.gwb/base_d/person/baptism_src/access
* base-simple.gwb/base_d/person/baptism_src/data  [FIELD string]

* base-simple.gwb/base_d/person/sex/access
* base-simple.gwb/base_d/person/sex/data           [FIELD Def.sex]
  [type sex = Male | Female | Neuter]

* base-simple.gwb/base_d/person/related/access
* base-simple.gwb/base_d/person/related/data       [NOVALUE int, NOVALUE int]
  (linked-list of ints, second int is the position of the next cell or -1)
  (NOVALUE = value_no_header)

* base-simple.gwb/base_d/person/qualifiers/access
* base-simple.gwb/base_d/person/qualifiers/data        [FIELD string list]
* base-simple.gwb/base_d/person/qualifiers/data2.ext

* base-simple.gwb/base_d/person/family/access
* base-simple.gwb/base_d/person/family/data [unions]
  This file is created in 2 steps.
  Incrementally as:
  [VALUE int, VALUE int)
  (linked-list of ints, second int is the position of the next cell or -1)
  Then, it is "reordered" to become a standard [FIELD int array] file.

* base-simple.gwb/base_d/person/titles/access
* base-simple.gwb/base_d/person/titles/data   [FIELD string Def.gen_title list]
* base-simple.gwb/base_d/person/titles/data2.ext
  (contains only non-nil lists, others have -1 as position in access)

* base-simple.gwb/base_d/person/psources/access
* base-simple.gwb/base_d/person/psources/data  [FIELD string]

* base-simple.gwb/base_d/person/baptism/access
* base-simple.gwb/base_d/person/baptism/data   [FIELD Adef.codate]

* base-simple.gwb/base_d/person/public_name/access
* base-simple.gwb/base_d/person/public_name/data      [FIELD string]

* base-simple.gwb/base_d/family/fsources/access
* base-simple.gwb/base_d/family/fsources/data     [FIELD string]

* base-simple.gwb/base_d/family/children/access
* base-simple.gwb/base_d/family/children/data      [FIELD Adef.iper array]

* base-simple.gwb/base_d/family/marriage_place/access
* base-simple.gwb/base_d/family/marriage_place/data  [FIELD string]

* base-simple.gwb/base_d/family/relation/access
* base-simple.gwb/base_d/family/relation/data      [FIELD Def.relation_kind]

* base-simple.gwb/base_d/family/origin_file/access
* base-simple.gwb/base_d/family/origin_file/data     [FIELD string]

* base-simple.gwb/base_d/family/divorce/access
* base-simple.gwb/base_d/family/divorce/data       [FIELD Def.divorce]

* base-simple.gwb/base_d/family/marriage/access
* base-simple.gwb/base_d/family/marriage/data      [FIELD Adef.codate]

* base-simple.gwb/base_d/family/witnesses/access
* base-simple.gwb/base_d/family/witnesses/data    [FIELD Def.iper array]

* base-simple.gwb/base_d/family/comment/access
* base-simple.gwb/base_d/family/comment/data        [FIELD string]

* base-simple.gwb/base_d/family/mother/access
* base-simple.gwb/base_d/family/mother/data        [FIELD Adef.iper]

* base-simple.gwb/base_d/family/father/access
* base-simple.gwb/base_d/family/father/data        [FIELD Adef.iper]

* base-simple.gwb/base_d/family/marriage_src/access
* base-simple.gwb/base_d/family/marriage_src/data  [FIELD string]

* base-simple.gwb/command.txt
* base-simple.gwb/cache_person_linked_pages
* base-simple.gwb/notes_links
* base-simple.gwb/tstab
* base-simple.gwf
* base-simple.lck











## Database format 1

╰─➤ find base-one.gwb                                                   [22:33]
base-one.gwb
base-one.gwb/snames.dat
base-one.gwb/base
base-one.gwb/base.acc
base-one.gwb/snames.inx
base-one.gwb/particles.txt
base-one.gwb/strings.inx
base-one.gwb/command.txt
base-one.gwb/cache_person_linked_pages
base-one.gwb/names.acc
base-one.gwb/patches~
base-one.gwb/notes_links
base-one.gwb/fnames.dat
base-one.gwb/patches
base-one.gwb/fnames.inx
base-one.gwb/notes_links~
base-one.gwb/names.inx





Difficulté: Consang est directement cablé avec Db1 et Db2.


TODO:

* Tests to check iso-functionality
  * Add a command-line tool to test adding/querying the databases
  * Add a set of tests using the command-line tool
* Provide a gwdb with a third format ?
