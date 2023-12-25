# $Id: Makefile,v 5.7 2009-03-11 10:53:30 ddr Exp $

PREFIX=/usr
LANGDIR=$(PREFIX)/share/geneweb
MANDIR=$(PREFIX)/man/man1
DESTDIR=distribution
MANPAGES=ged2gwb.1 gwb2ged.1 gwc.1 gwc2.1 gwu.1 gwd.1 consang.1 gwsetup.1

all:: exe hd/etc/version.txt

exe::
	dune build

BUILD=_build/default/

install:
	mkdir -p $(PREFIX)/bin
	cp -f $(BUILD)src/gwc.exe $(PREFIX)/bin/gwc$(EXE)
	cp -f $(BUILD)src/gwc.exe $(PREFIX)/bin/gwc1$(EXE)
	cp -f $(BUILD)src/gwc2.exe $(PREFIX)/bin/gwc2$(EXE)
	cp -f $(BUILD)src/mk_consang.exe $(PREFIX)/bin/consang$(EXE)
	cp -f $(BUILD)src/gwd.exe $(PREFIX)/bin/gwd$(EXE)
	cp -f $(BUILD)src/gwu.exe $(PREFIX)/bin/gwu$(EXE)
	cp -f $(BUILD)ged2gwb/ged2gwb.exe $(PREFIX)/bin/ged2gwb$(EXE)
	cp -f $(BUILD)ged2gwb/ged2gwb2.exe $(PREFIX)/bin/ged2gwb2$(EXE)
	cp -f $(BUILD)gwb2ged/gwb2ged.exe $(PREFIX)/bin/gwb2ged$(EXE)
	mkdir -p $(LANGDIR)/lang
	cp -f hd/lang/*.txt $(LANGDIR)/lang/.
	mkdir -p $(LANGDIR)/images
	mkdir -p $(LANGDIR)/images/flags
	cp -f hd/images/flags/*.jpg hd/images/flags/*.png $(LANGDIR)/images/flags/.
	cp -f hd/images/*.jpg hd/images/*.png hd/images/*.ico $(LANGDIR)/images/.
	mkdir -p $(LANGDIR)/etc
	cp -f -R hd/etc/* $(LANGDIR)/etc/.
	find $(LANGDIR)/etc/ -name .svn -type d -prune -exec rm -rf {} \;
	mkdir -p $(MANDIR)
	cd man; cp -f $(MANPAGES) $(MANDIR)/.

uninstall:
	rm -f $(PREFIX)/bin/gwc$(EXE)
	rm -f $(PREFIX)/bin/gwc1$(EXE)
	rm -f $(PREFIX)/bin/gwc2$(EXE)
	rm -f $(PREFIX)/bin/consang$(EXE)
	rm -f $(PREFIX)/bin/gwd$(EXE)
	rm -f $(PREFIX)/bin/gwu$(EXE)
	rm -f $(PREFIX)/bin/ged2gwb$(EXE)
	rm -f $(PREFIX)/bin/gwb2ged$(EXE)
	rm -rf $(PREFIX)/share/geneweb
	cd $(MANDIR); rm -f $(MANPAGES)

distrib: all hd/etc/version.txt new_distrib wrappers

wrappers:
	if test "$(CAMLP5F)" = "-DWIN95"; then \
	  echo -ne 'setlocal enableextensions\r\n' > $(DESTDIR)/gwd.bat; \
	  echo -ne 'md bases\r\n' >> $(DESTDIR)/gwd.bat; \
	  echo -ne 'endlocal\r\n' >> $(DESTDIR)/gwd.bat; \
	  echo -ne 'cd bases\r\n' >> $(DESTDIR)/gwd.bat; \
	  echo -ne 'start /MIN ..\\gw\\gwd -hd ..\\gw\r\n' >> $(DESTDIR)/gwd.bat; \
	  echo -ne 'setlocal enableextensions\r\n' > $(DESTDIR)/gwsetup.bat; \
	  echo -ne 'md bases\r\n' >> $(DESTDIR)/gwsetup.bat; \
	  echo -ne 'endlocal\r\n' >> $(DESTDIR)/gwsetup.bat; \
	  echo -ne 'cd bases\r\n' >> $(DESTDIR)/gwsetup.bat; \
	  echo -ne 'start /MIN ..\\gw\\gwsetup -lang fr -gd ..\\gw\r\n' >> $(DESTDIR)/gwsetup.bat; \
	else \
	  (echo '#!/bin/sh'; \
	   echo 'cd `dirname $$0`'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwd -hd ../gw "$$@"') > $(DESTDIR)/gwd; \
	  (echo '#!/bin/sh'; \
	   echo 'cd `dirname $$0`'; \
	   echo 'mkdir -p bases'; \
	   echo 'cd bases'; \
	   echo 'exec ../gw/gwsetup -gd ../gw "$$@"') > $(DESTDIR)/gwsetup; \
	  chmod +x $(DESTDIR)/gwd $(DESTDIR)/gwsetup; \
	fi
	if test $(shell uname -s) = "Darwin"; then \
	  cp -f etc/MacOSX/GeneWeb.command $(DESTDIR); \
	fi

hd/etc/version.txt:
	@echo "Generating $@..."
	@echo "GeneWeb[:] [compiled on %s from commit %s:::" > $@
	@echo "$$(date '+%Y-%m-%d'):" >> $@
	@echo "$$(git show -s --date=short --pretty=format:'<a href="https://github.com/hgouraud/geneweb/commit/%h">%h (%cd)</a>')]" >> $@
	@echo " Done!"
#.PHONY:hd/etc/version.txt

new_distrib: classical_distrib
	mkdir t
	mv $(DESTDIR) t/gw
	mv t $(DESTDIR)
	mkdir $(DESTDIR)/gw/old
	mkdir $(DESTDIR)/gw/setup
	cp -f setup/intro.txt $(DESTDIR)/gw/setup/.
	mkdir $(DESTDIR)/gw/setup/lang
	if test "$(CAMLP5F)" = "-DWIN95"; then \
	  cp -f setup/lang/intro.txt.dos $(DESTDIR)/gw/setup/lang/intro.txt; \
	else \
	  cp -f setup/lang/intro.txt $(DESTDIR)/gw/setup/lang/intro.txt; \
	fi
	cp -f setup/lang/*.htm $(DESTDIR)/gw/setup/lang/.
	cp -f setup/lang/lexicon.txt $(DESTDIR)/gw/setup/lang/.
	cp -f $(BUILD)setup/setup.exe $(DESTDIR)/gw/gwsetup$(EXE)
	cp -f LICENSE $(DESTDIR)/LICENSE.txt
	cp -f etc/START.htm $(DESTDIR)/.
	cp -f CHANGES $(DESTDIR)/CHANGES.txt
	echo "127.0.0.1" > $(DESTDIR)/gw/only.txt
	echo "-setup_link" > $(DESTDIR)/gw/gwd.arg

classical_distrib:
	$(RM) -rf $(DESTDIR)
	mkdir $(DESTDIR)
	cp -f $(BUILD)src/gwc.exe $(DESTDIR)/gwc$(EXE)
	cp -f $(BUILD)src/gwc.exe $(DESTDIR)/gwc1$(EXE)
	cp -f $(BUILD)src/gwc2.exe $(DESTDIR)/gwc2$(EXE)
	cp -f $(BUILD)src/mk_consang.exe $(DESTDIR)/consang$(EXE)
	cp -f $(BUILD)src/gwd.exe $(DESTDIR)/gwd$(EXE)
	cp -f $(BUILD)src/gwu.exe $(DESTDIR)/gwu$(EXE)
	cp -f $(BUILD)src/update_nldb.exe $(DESTDIR)/update_nldb$(EXE)
	cp -f $(BUILD)ged2gwb/ged2gwb.exe $(DESTDIR)/ged2gwb$(EXE)
	cp -f $(BUILD)ged2gwb/ged2gwb2.exe $(DESTDIR)/ged2gwb2$(EXE)
	cp -f $(BUILD)gwb2ged/gwb2ged.exe $(DESTDIR)/gwb2ged$(EXE)
	mkdir $(DESTDIR)/gwtp_tmp
	mkdir $(DESTDIR)/gwtp_tmp/lang
	cp -f $(BUILD)gwtp/gwtp.exe $(DESTDIR)/gwtp_tmp/gwtp$(EXE)
	cp -f gwtp/README $(DESTDIR)/gwtp_tmp/.
	cp -f gwtp/lang/*.txt $(DESTDIR)/gwtp_tmp/lang/.
	cp -f etc/a.gwf $(DESTDIR)/.
	mkdir $(DESTDIR)/lang
	cp -f hd/lang/*.txt $(DESTDIR)/lang/.
	mkdir $(DESTDIR)/images
	mkdir $(DESTDIR)/images/flags
	cp -f hd/images/flags/*.jpg hd/images/flags/*.png $(DESTDIR)/images/flags/.
	cp -f hd/images/*.jpg hd/images/*.png hd/images/*.ico $(DESTDIR)/images/.
	mkdir $(DESTDIR)/etc
	cp -f -R hd/etc/* $(DESTDIR)/etc/.
	rm -f hd/etc/version.txt
	find $(DESTDIR)/etc/ -name .svn -type d -prune -exec rm -rf {} \;

windows_files:
	@for i in distribution/*.txt distribution/gw/*.txt; do \
	  echo "========================================="; \
	  echo $$i; \
	  cp -f $$i $$i~; \
	  sed -e 's/$$/\r/' $$i~ > $$i; \
	  rm $$i~; \
	done

clean::
	rm -rf _build
	$(RM) -rf $(DESTDIR)
	$(RM) -f *~ .#*
