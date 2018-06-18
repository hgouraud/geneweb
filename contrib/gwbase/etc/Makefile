# $Id: Makefile,v 4.36 2009-03-11 10:58:50 deraugla Exp $

GWB=../../..
include $(GWB)/tools/Makefile.inc

OCAMLI=-I $(GWB)/src
STD_OBJS=$(GWB)/src/adef.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/iovalue.cmo $(GWB)/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/db2disk.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo
CONNEX_OBJS=$(GWB)/src/argl.cmo $(GWB)/src/adef.cmo $(GWB)/src/buff.cmo $(GWB)/src/name.cmo $(GWB)/src/ansel.cmo $(GWB)/src/iovalue.cmo $(GWB)/src/mutil.cmo $(GWB)/src/futil.cmo $(GWB)/src/dutil.cmo $(GWB)/src/btree.cmo $(GWB)/src/secure.cmo $(GWB)/src/database.cmo $(GWB)/src/db2.cmo $(GWB)/src/db2disk.cmo $(GWB)/src/diff.cmo $(GWB)/src/gwdb.cmo $(GWB)/src/checkItem.cmo $(GWB)/src/gutil.cmo $(GWB)/src/lock.cmo connex.cmo
PUBLIC_OBJS=$(STD_OBJS) public.cmo

all:: opt

out:: connex.out public.out

opt:: connex.opt public.opt

public.out: $(PUBLIC_OBJS)
	$(OCAMLC) unix.cma $(LIBUNIX) $(PUBLIC_OBJS) -o public.out

public.opt: $(PUBLIC_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(PUBLIC_OBJS:.cmo=.cmx) -o public.opt

connex.out: $(CONNEX_OBJS)
	$(OCAMLC) -custom unix.cma $(LIBUNIX) $(CONNEX_OBJS) -o connex.out

connex.opt: $(CONNEX_OBJS:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(LIBUNIX) $(CONNEX_OBJS:.cmo=.cmx) -o connex.opt

