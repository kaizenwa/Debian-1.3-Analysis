###############################################################################
#
# hfsutils - tools for reading and writing Macintosh HFS volumes
# Copyright (C) 1996, 1997 Robert Leslie
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
###############################################################################

DESTDIR =	/usr/local

BINDEST =	$(DESTDIR)/bin
MANDEST =	$(DESTDIR)/man

LIBDEST =	$(DESTDIR)/lib
INCDEST =	$(DESTDIR)/include

MANEXT =	1

BININSTALL =	install -c -m 755
LIBINSTALL =	install -c -m 644
HARDLINK =	ln -f

CC =		gcc
INCLUDES =	-I/usr/include/tcl

DEFINES =	
LIBS =		

TCLLIBS =	-ltcl7.6 -lm -ldl
TKLIBS =	-ltk4.2
XLIBS =		-L/usr/X11R6/lib -lX11

COPTS =		-g -O2
CFLAGS =	$(COPTS) -Ilibhfs $(INCLUDES) $(DEFINES)
LDFLAGS =	-Llibhfs

###############################################################################

CLITARGETS =	hattrib hcd hcopy hdel hdir hformat hls hmkdir hmount hpwd  \
		hrename hrmdir humount hvol
TCLTARGETS =	hfssh hfs xhfs

LIBHFS =	libhfs/libhfs.a
INCHFS =	libhfs/hfs.h

LIBRSRC =	librsrc/librsrc.a
INCRSRC =	librsrc/rsrc.h

HFSUTIL =	hfsutil
HFSCK =		hfsck/hfsck

GENERALDOCS =	hfsutils.1
CLIDOCS =	hattrib.1 hcd.1 hcopy.1 hdel.1 hdir.1 hformat.1 hls.1  \
		hmkdir.1 hmount.1 hpwd.1 hrename.1 hrmdir.1 humount.1 hvol.1
TCLDOCS =	hfssh.1 hfs.1 xhfs.1

CLIOBJS =	hattrib.o hcd.o hcopy.o hdel.o hformat.o hls.o hmkdir.o  \
		hmount.o hpwd.o hrename.o hrmdir.o humount.o hvol.o
UTILOBJS =	crc.o binhex.o copyin.o copyout.o  \
		darray.o dlist.o dstring.o glob.o

###############################################################################

all :: all_cli all_tcl

misc :: $(LIBRSRC) $(HFSCK)

all_cli :: $(CLITARGETS)

all_tcl :: $(TCLTARGETS)

all_lib :: $(LIBHFS)

install :: install_cli install_tcl

install_cli :: all_cli
	$(BININSTALL) $(HFSUTIL) "$(BINDEST)/."

	for file in $(CLITARGETS); do  \
		$(HARDLINK) "$(BINDEST)/$(HFSUTIL)" "$(BINDEST)/$$file";  \
	done

	rm -f "$(BINDEST)/$(HFSUTIL)"

	for file in $(GENERALDOCS) $(CLIDOCS); do  \
		$(LIBINSTALL) doc/$$file  \
		"$(MANDEST)/man$(MANEXT)/`basename $$file .1`.$(MANEXT)";  \
	done

install_tcl :: all_tcl
	for file in $(TCLTARGETS); do  \
		$(BININSTALL) $$file "$(BINDEST)/.";  \
	done

	if [ -f "$(BINDEST)/hfs" ]; then  \
		sed -e '1d' "$(BINDEST)/hfs" > "$(BINDEST)/hfs.new";  \
		$(BININSTALL) "$(BINDEST)/hfs.new" "$(BINDEST)/hfs";  \
		rm -f "$(BINDEST)/hfs.new";  \
	fi

	for file in $(GENERALDOCS) $(TCLDOCS); do  \
		$(LIBINSTALL) doc/$$file  \
		"$(MANDEST)/man$(MANEXT)/`basename $$file .1`.$(MANEXT)";  \
	done

install_lib :: all_lib
	$(LIBINSTALL) $(LIBHFS) "$(LIBDEST)/."
	$(LIBINSTALL) $(INCHFS) "$(INCDEST)/."

again :: depend clean all

depend ::
	( sed -n '1,/^### DEPEND/p' Makefile;  \
	  echo;  \
	  $(CC) -MM $(CFLAGS) *.c;  \
	) > Makefile.new
	mv -f Makefile.new Makefile
	cd libhfs && $(MAKE) depend
	cd librsrc && $(MAKE) depend
	cd hfsck && $(MAKE) depend

clean ::
	rm -f $(HFSUTIL) $(CLITARGETS) $(TCLTARGETS) xhfs.c *.o core
	cd libhfs && $(MAKE) clean
	cd librsrc && $(MAKE) clean
	cd hfsck && $(MAKE) clean

dist :: depend TAGS clean
	sed -e '/^COPTS/s/-Wall -pedantic/-O2/' Makefile > Makefile.new
	mv -f Makefile.new Makefile

	name=$$(basename $$(pwd)); cd .. && tar cvfz $$name.tar.gz $$name

	sed -e '/^COPTS/s/-O2/-Wall -pedantic/' Makefile > Makefile.new
	mv -f Makefile.new Makefile

###############################################################################

TAGS: *.[ch] */*.[ch]
	etags *.[ch] */*.[ch]

xhfs.c: xhfs.tcl
	( echo "char xhfs[] =";  \
	  sed -e 's|^[	 ]*||' -e 's|^#.*||' -e '/^$$/d'  \
	      -e 's|		*| |g'  \
	      -e 's|[\\"]|\\&|g' -e 's|.*|"&\\n"|' xhfs.tcl;  \
	  echo ";";  \
	) > $@

###############################################################################

$(LIBHFS) ::
	cd libhfs && $(MAKE) CC="$(CC)" COPTS="$(COPTS)" DEFINES="$(DEFINES)"

$(LIBRSRC) ::
	cd librsrc && $(MAKE) CC="$(CC)" COPTS="$(COPTS)" DEFINES="$(DEFINES)"

$(HFSCK) ::
	$(MAKE) $(LIBHFS)
	cd hfsck && $(MAKE) CC="$(CC)" COPTS="$(COPTS)" DEFINES="$(DEFINES)"

$(HFSUTIL): $(LIBHFS) hfsutil.o hcwd.o $(CLIOBJS) $(UTILOBJS)
	$(CC) $(LDFLAGS) hfsutil.o hcwd.o   \
		$(CLIOBJS)  \
		$(UTILOBJS)  \
		-lhfs $(LIBS) -o $@

$(CLITARGETS): $(HFSUTIL)
	ln -f $(HFSUTIL) $@

hfssh: $(LIBHFS) hfssh.o tclhfs.o $(UTILOBJS)
	$(CC) $(LDFLAGS) hfssh.o tclhfs.o  \
		$(UTILOBJS)  \
		-lhfs $(LIBS) $(TCLLIBS) -o $@

hfs: hfs.tcl hfssh
	( echo "#!./hfssh";  \
	  echo "#!/bin/sh";  \
	  echo "# Start hfssh ... \\";  \
	  echo 'exec hfssh "$$0" "$$@"';  \
	  echo;  \
	  cat hfs.tcl;  \
	) > $@
	chmod a+x $@

xhfs: $(LIBHFS) hfswish.o xhfs.o tclhfs.o $(UTILOBJS)
	$(CC) $(LDFLAGS) hfswish.o xhfs.o tclhfs.o  \
		$(UTILOBJS)  \
		-lhfs $(LIBS) $(TKLIBS) $(TCLLIBS) $(XLIBS) -o $@

### DEPENDENCIES FOLLOW #######################################################

binhex.o: binhex.c binhex.h crc.h
copyin.o: copyin.c libhfs/hfs.h libhfs/data.h copyin.h binhex.h crc.h
copyout.o: copyout.c libhfs/hfs.h libhfs/data.h copyout.h binhex.h \
 crc.h
crc.o: crc.c crc.h
darray.o: darray.c darray.h
dlist.o: dlist.c dlist.h
dstring.o: dstring.c dstring.h
glob.o: glob.c dlist.h dstring.h libhfs/hfs.h glob.h
hattrib.o: hattrib.c libhfs/hfs.h hcwd.h hfsutil.h glob.h hattrib.h
hcd.o: hcd.c libhfs/hfs.h hcwd.h hfsutil.h glob.h hcd.h
hcopy.o: hcopy.c libhfs/hfs.h hcwd.h hfsutil.h glob.h hcopy.h copyin.h \
 copyout.h
hcwd.o: hcwd.c hcwd.h
hdel.o: hdel.c libhfs/hfs.h hcwd.h hfsutil.h glob.h hdel.h
hformat.o: hformat.c libhfs/hfs.h hcwd.h hfsutil.h hformat.h
hfssh.o: hfssh.c tclhfs.h
hfsutil.o: hfsutil.c libhfs/hfs.h hcwd.h hfsutil.h version.h hattrib.h \
 hcd.h hcopy.h hdel.h hformat.h hls.h hmkdir.h hmount.h hpwd.h \
 hrename.h hrmdir.h humount.h hvol.h
hfswish.o: hfswish.c tclhfs.h xhfs.h images.h images/macdaemon.xbm \
 images/macdaemon_mask.xbm images/stop.xbm images/caution.xbm \
 images/note.xbm images/floppy.xbm images/harddisk.xbm \
 images/cdrom.xbm images/floppy_mask.xbm images/harddisk_mask.xbm \
 images/cdrom_mask.xbm images/sm_floppy.xbm images/sm_harddisk.xbm \
 images/sm_cdrom.xbm images/folder.xbm images/document.xbm \
 images/application.xbm images/folder_mask.xbm \
 images/document_mask.xbm images/application_mask.xbm \
 images/sm_folder.xbm images/sm_document.xbm images/sm_application.xbm \
 images/help.xbm images/padlock.xbm
hls.o: hls.c libhfs/hfs.h hcwd.h hfsutil.h darray.h dlist.h dstring.h \
 glob.h hls.h
hmkdir.o: hmkdir.c libhfs/hfs.h hcwd.h hfsutil.h glob.h hmkdir.h
hmount.o: hmount.c libhfs/hfs.h hcwd.h hfsutil.h hmount.h
hpwd.o: hpwd.c libhfs/hfs.h hcwd.h hfsutil.h hpwd.h
hrename.o: hrename.c libhfs/hfs.h hcwd.h hfsutil.h glob.h hrename.h
hrmdir.o: hrmdir.c libhfs/hfs.h hcwd.h hfsutil.h glob.h hrmdir.h
humount.o: humount.c libhfs/hfs.h hcwd.h hfsutil.h humount.h
hvol.o: hvol.c libhfs/hfs.h hcwd.h hfsutil.h hvol.h
tclhfs.o: tclhfs.c tclhfs.h libhfs/hfs.h glob.h copyin.h copyout.h \
 version.h
xhfs.o: xhfs.c
