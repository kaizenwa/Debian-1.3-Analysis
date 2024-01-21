# Generated automatically from Makefile.in by configure.
# Makefile for kpathsea --kb@cs.umb.edu.
version = 2.52

# paths.make -- installation directories.
#
# The compile-time paths are defined in kpathsea/paths.h, which is built
# from kpathsea/paths.h.in and these definitions.  See kpathsea/INSTALL
# for a description of how the various path-related files are used and
# created.

# Do not change prefix and exec_prefix in Makefile.in!
# configure doesn't propagate the change to the other Makefiles.
# Instead, give the -prefix/-exec-prefix options to configure.
# (See kpathsea/INSTALL for more details.) This is arguably
# a bug, but it's not likely to change soon.
prefix = /usr/local
exec_prefix = ${prefix}

# Architecture-dependent executables.
bindir = $(exec_prefix)/bin

# Architecture-independent executables.
scriptdir = $(bindir)

# Architecture-dependent files, such as lib*.a files.
libdir = $(exec_prefix)/lib

# Architecture-independent files.
datadir = $(prefix)/lib

# Header files.
includedir = $(prefix)/include

# GNU .info* files.
infodir = $(prefix)/info

# Unix man pages.
manext = 1
mandir = $(prefix)/man/man$(manext)

# TeX & MF-specific directories. Not all of the following are relevant
# for all programs, but it seems cleaner to collect everything in one place.

# The default paths are now in kpathsea/paths.h.in. Passing all the
# paths to sub-makes can make the arg list too long on system V.

# The root of the tree.
texmf = $(datadir)/texmf

# TeX and MF source files.
texinputdir = $(texmf)/tex
mfinputdir = $(texmf)/mf

# MakeTeXPK.site, texmf.cnf, etc.
web2cdir = $(texmf)/web2c

# The top-level font directory.
fontdir = $(texmf)/fonts

# Memory dumps (.fmt and .base).
fmtdir = $(texmf)/ini
basedir = $(fmtdir)

# Pool files.
texpooldir = $(texmf)/ini
mfpooldir = $(texpooldir)

# If install_fonts=true, the PostScript/LaserJet TFM and VF files for
# the builtin fonts get installed in subdirectories of this directory,
# named for the typeface families of these directories. If you don't
# have the default directory setup, you will want to set
# install_fonts=false.  Ditto for install_macros.
install_fonts = true
install_macros = true

# Where the .map files from fontname are installed.
fontnamedir = $(texmf)/fontname

# Where the dvips configuration files get installed, and where
# psfonts.map is.
dvipsdir = $(texmf)/dvips
psheaderdir = $(dvipsdir)

# MakeTeXPK will go here to create dc*.
dcfontdir = $(fontdir)/public/dc

# MakeTeXPK will go here if it exists to create nonstandard CM fonts,
# e.g., cmr11. See ftp.cs.umb.edu:pub/tex/sauter.tar.gz. The Sauter
# files must be in your regular MFINPUTS.
sauterdir = $(fontdir)/public/sauter

# If a font can't be found close enough to its stated size, we look for
# each of these sizes in the order given.  This colon-separated list is
# overridden by the envvar TEXSIZES, and by a program-specific variable
# (e.g., XDVISIZES), and perhaps by a config file (e.g., in dvips).
default_texsizes = 300:600

# End of paths.make.
# makevars.make -- the directory names we pass.
# It's important that none of these values contain [ @%], for the sake
# of kpathsea/texmf.sed.
makevars = prefix=$(prefix) exec_prefix=$(exec_prefix) \
  bindir=$(bindir) scriptdir=$(scriptdir) libdir=$(libdir) \
  datadir=$(datadir) infodir=$(infodir) includedir=$(includedir) \
  manext=$(manext) mandir=$(mandir) \
  texmf=$(texmf) web2cdir=$(web2cdir) \
  texinputdir=$(texinputdir) mfinputdir=$(mfinputdir) \
  fontdir=$(fontdir) \
  fmtdir=$(fmtdir) basedir=$(basedir) \
  texpooldir=$(texpooldir) mfpooldir=$(mfpooldir) \
  install_fonts=$(install_fonts) \
  dvipsdir=$(dvipsdir) psheaderdir=$(psheaderdir) \
  default_texsizes='$(default_texsizes)'
# End of makevars.make.

# Add -DNO_DEBUG to disable debugging, for unnoticeably better performance.
DEFS = $(XDEFS)

# common.make -- used by all Makefiles.
SHELL = /bin/sh

top_srcdir = .
srcdir = .

CPICFLAG = -fPIC

SHLEXT = so

SH_LD = gcc
SH_LDFLAGS = -shared

SHARED_LIBS = false

CC = gcc
# CFLAGS is used for both compilation and linking.
CFLAGS = -g -O2 $(XCFLAGS)

# Do not override CPPFLAGS; change XCPPFLAGS, CFLAGS, XCFLAGS, or DEFS instead.
CPPFLAGS = $(XCPPFLAGS) -I. -I$(srcdir) \
	   -I$(kpathsea_parent) -I$(kpathsea_srcdir_parent) \
	   $(prog_cflags)  $(DEFS) 

%.o : %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

pic/%.o : %.c
	$(CC) $(CPPFLAGS) $(CPICFLAG) $(CFLAGS) -c $< -o $@

# Installation.
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

# This is used to recursively copy a fonts/ or tex/ directory to
# $(fontdir) or $(texinputdir).
# The first arg is `.', and the second is the target directory.
CP_R = cp -r

# This is so kpathsea will get remade automatically if you change
# something in it and recompile from the package directory.
kpathsea_parent = ..
kpathsea_dir = $(kpathsea_parent)/kpathsea
kpathsea_srcdir_parent = $(top_srcdir)/..
kpathsea_srcdir = $(kpathsea_srcdir_parent)/kpathsea
kpathsea = $(kpathsea_dir)/libkpathsea.a

##ifeq ($(CC), gcc)
##XDEFS = -Wall -Wpointer-arith $(warn_more)
##CFLAGS = -g $(XCFLAGS)
##endif
# End of common.make.
# library.make -- stuff only useful for libraries.
AR = ar
ARFLAGS = cq
RANLIB = ranlib
# End of library.make.
# programs.make -- used by Makefiles for executables only.
# Linking. Don't include $(CFLAGS), since ld -g under Linux forces
# static libraries, including libc.a and libX*.a
LDFLAGS =  $(XLDFLAGS)
LIBS = 
# proglib is for web2c; 
# XLOADLIBES is for the installer.
LOADLIBES= $(proglib) $(kpathsea) $(LIBS) -lm $(XLOADLIBES)

# Why separate CCLD from CC?  No particular reason.
CCLD = $(CC)
link_command = $(CCLD) -o $@ $(LDFLAGS) 
# End of programs.make.
# texi.make -- making .dvi and .info from .texi.

MAKEINFO = makeinfo
MAKEINFO_FLAGS = --paragraph-indent=2 -I$(srcdir)
TEXI2DVI = texi2dvi

.SUFFIXES: .info .dvi .texi
.texi.info:
	-$(MAKEINFO) $(MAKEINFO_FLAGS) $< -o $@
.texi.dvi:
	-$(TEXI2DVI) $(TEXI2DVI_FLAGS) $<

# Install these header files (except c-auto.h).
install_headers = *.h

##ifdef HOSTNAME
##warn_more = -Wmissing-prototypes -Wwrite-strings -Wshadow
##MAKEINFO_FLAGS := $(MAKEINFO_FLAGS) --no-split
##malloc = onefile.o # gmalloc.o
##endif

# Put tex-file.o first, because it's what depends on the paths, and may
# reduce frustration if the paths are wrong by doing it first.

objects = tex-file.o absolute.o basename.o cnf.o concat.o concat3.o      \
concatn.o db.o debug.o dir.o elt-dirs.o expand.o extend-fname.o file-p.o \
find-suffix.o fn.o fontmap.o getopt.o getopt1.o hash.o itoa.o kdefault.o \
line.o magstep.o make-suffix.o path-elt.o pathsearch.o proginit.o        \
progname.o putenv.o readable.o rm-suffix.o str-list.o str-llist.o        \
tex-glyph.o tex-make.o tilde.o truncate.o uppercasify.o variable.o       \
version.o xcalloc.o xfopen.o xmalloc.o xopendir.o xputenv.o xrealloc.o   \
xstat.o xstrdup.o $(malloc)

LIBS_TO_INSTALL = libkpathsea.a

SH_LIBS_TO_INSTALL = libkpathsea.$(SHLEXT)

ifeq ($(SHARED_LIBS), true)
  BINDISTFILES = libkpathsea.$(SHLEXT)
  BINDISTLIBS = $(addprefix kpathsea/, libkpathsea.$(SHLEXT))
endif

default all: stamp-picdir kpsewhich MakeTeXPK kpathsea.info stamp-shared

stamp-picdir:
	@if $(SHARED_LIBS); then \
	  if [ -n "$(CPICFLAG)" ]; then \
	    if [ -d pic ]; then \
	      true ; \
	    else \
	      echo "mkdir pic" ; \
	      mkdir pic ; \
	    fi ; \
	  else \
	    true ; \
	  fi ; \
	fi
	touch stamp-picdir

libkpathsea.a: $(objects)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(objects)
	$(RANLIB) $@

ifeq ($(SHARED_LIBS), true)
  ifdef CPICFLAG
    PICOBJ := $(addprefix pic/, $(objects))
  else
    PICOBJ := $(objects)
  endif
endif

stamp-shared: $(PICOBJ)
	if $(SHARED_LIBS); then \
	  $(SH_LD) $(SH_LDFLAGS) -o libkpathsea.$(SHLEXT) $(PICOBJ) ; \
	fi
	touch stamp-shared

kpsewhich: libkpathsea.a kpsewhich.o
	$(link_command) kpsewhich.o $(LOADLIBES)

MakeTeXPK: MakeTeXPK.in $(top_srcdir)/../make/paths.make
	sed -e 's,${prefix}/share,$(datadir),g' \
	    -e 's,@psheaderdir@,$(psheaderdir),g' \
	    -e 's,@fontnamedir@,$(fontnamedir),g' \
	    -e 's,@fontdir@,$(fontdir),g' \
	    -e 's,@dcfontdir@,$(dcfontdir),g' \
	    -e 's,@sauterdir@,$(sauterdir),g' \
	    -e 's,@web2cdir@,$(web2cdir),g' \
	<$(srcdir)/MakeTeXPK.in >$@.tmp
	mv $@.tmp $@
	-chmod a+x $@

# Besides the straight variable substitutions, also change all
# occurrences of `$(texmf)/' to `$TEXMF', so simply setting that envvar
# changes the root of the whole tree.
texmf.cnf: texmf.cnf.in texmf.sed
	sed -f texmf.sed -e 's%$(texmf)/%$$TEXMF/%g' $(srcdir)/texmf.cnf.in >$@

# The idea is to turn each var=value into s%@var@%value%g. Seems simpler
# to put the substitutions in a file than to play shell games.
texmf.sed: $(top_srcdir)/../make/makevars.make
	echo $(makevars) \
	| tr ' ' '\012' \
	| sed -e 's/^/s%@/' -e 's/=/@%/' -e 's/$$/%/' -e 's/$$/g/' \
	>$@

# First null out comments and leading/trailing whitespace, then remove
# lines that define invalid C identifiers, then remove blank lines. Each
# line remaining looks like
# <name> = <value>
# (but = and surrounding spaces are optional, hence can't remove all spaces).
# Change this to #ifndef DEFAULT_name@#define DEFAULT_name "value"@#endif,
# then change the @'s to newlines (sed isn't good at multiline replacements),
# then change $TEXMF back to $(texmf)
# 
# No backslash-newline escapes in the long sed replacement because that
# will turn into a space in the output.
# 
# Without the $TEXMF substitution, if the cnf file was not found for
# some reason, none of the compile-time paths would be of any use, since
# TEXMF wouldn't be defined. Alternatively, we could have a way to
# specify compile-time default values for variables in general, but I
# think it's better to keep the last-resort paths as simple as possible.
# 
# The definition of DEFAULT_TEXMF (and possibly other variables)
# that winds up in the final paths.h will not be used.
paths.h: texmf.cnf
	echo "/* paths.h: Generated from texmf.cnf `date`.  */" >$@
	sed -e 's/%.*//' -e 's/^[ 	]*//' -e 's/[ 	]*$$//' texmf.cnf \
	| grep '^[ 	]*[A-Za-z_]*[ 	=]' \
	| sed '/^$$/d' \
	| sed 's/^\([^ 	=]*\)[ 	]*=*[ 	]*\(.*\)/#ifndef DEFAULT_\1@#define DEFAULT_\1 "\2"@#endif/' \
	| tr @ '\012' \
	| sed 's@\$$TEXMF@$(texmf)@g' \
	>>$@

# A few object files need extra definitions. The dependencies are below.
tex-file.o:
	$(CC) $(CPPFLAGS) $(CFLAGS) \
	  -DDEFAULT_FONT_SIZES='"$(default_texsizes)"' -c $(srcdir)/tex-file.c

pic/tex-file.o:
	$(CC) $(CPPFLAGS) $(CPICFLAG) $(CFLAGS) \
	  -DDEFAULT_FONT_SIZES='"$(default_texsizes)"'\
	  -c $(srcdir)/tex-file.c -o $@

# Need the -DHAVE_CONFIG_H.
gmalloc.o:
	$(CC) $(CPPFLAGS) -DHAVE_CONFIG_H $(CFLAGS) -c $(srcdir)/gmalloc.c

pic/gmalloc.o:
	$(CC) $(CPPFLAGS) $(CPICFLAG) -DHAVE_CONFIG_H $(CFLAGS) \
	  -c $(srcdir)/gmalloc.c -o pic/gmalloc.o

check: 

install:
	for f in $(LIBS_TO_INSTALL); do \
	  if [ -f $$f ]; then $(INSTALL_DATA) $$f $(libdir)/$$f; fi; \
	done
	for f in $(SH_LIBS_TO_INSTALL); do \
	  if [ -f $$f ]; then $(INSTALL_PROGRAM) $$f $(libdir)/$$f; fi; \
	done

uninstall:

# Installation of kpathsea itself is complicated by the fact that we
# want to install texmf.cnf, kpathsea.info, and MakeTeXPK by default,
# but not the headers or the .a file itself.  So we have a separate
# target `install-library' to install the latter stuff.
#install: install-exec install-data
#uninstall: uninstall-exec uninstall-data
#
#install-exec: kpsewhich MakeTeXPK
#	$(SHELL) $(top_srcdir)/../mkdirchain $(bindir) $(scriptdir)
#	if grep 'original MakeTeXPK --' $(scriptdir)/MakeTeXPK >/dev/null 2>&1\
#	   || test ! -r $(scriptdir)/MakeTeXPK; then \
#	  $(INSTALL_PROGRAM) MakeTeXPK $(scriptdir)/MakeTeXPK; \
#	else true; fi
#	$(INSTALL_PROGRAM) kpsewhich $(bindir)/kpsewhich
#uninstall-exec:
#	rm -f $(bindir)/kpsewhich
#
#install-data: texmf.cnf kpathsea.info
#	$(SHELL) $(top_srcdir)/../mkdirchain $(texmf) $(infodir) $(web2cdir)
#	if grep 'original texmf.cnf --' $(web2cdir)/texmf.cnf >/dev/null 2>&1 \
#	   || test ! -r $(web2cdir)/texmf.cnf; then \
#	  $(INSTALL_DATA) texmf.cnf $(web2cdir)/texmf.cnf; \
#	else true; fi
#	$(INSTALL_DATA) $(srcdir)/kpathsea.info $(infodir)/kpathsea.info
#uninstall-data:
#	rm -f $(infodir)/kpathsea.info*
#
#install-library: libkpathsea.a 
#	$(SHELL) $(top_srcdir)/../mkdirchain $(includedir)/kpathsea $(libdir)
#	$(INSTALL_DATA) libkpathsea.a $(libdir)/libkpathsea.a
#	for f in $(install_headers); do \
#	  $(INSTALL_DATA) $$f $(includedir)/kpathsea/$$f; done
#	rm -f $(includedir)/kpathsea/c-auto.h
#uninstall-library:
#	rm -f $(libdir)/libkpathsea.a
#	for f in $(install_headers); do rm -f $(includedir)/$$f; done

# distdir is used by other distributions; they define $(distdir).
distdir: INSTALL TAGS kpathsea.info kpathsea.dvi
	rm -rf $(distdir)
	mkdir -p $(distdir)
	ln $(ln_files) $(distdir)
	cp -p $(HOME)/gnu/gnuorg/*.texi $(distdir)
	touch *.info*
	ln *.info* *.texi kpathsea.aux kpathsea.cps $(distdir)
	ln CONFIGURE HIER common.ac $(distdir)
	cd $(distdir); rm -f paths.h
	cd $(distdir); add-version $(version) version.c

bin-dist:
	if [ -n "$(BINDISTFILES)" ]; then \
	  ln $(BINDISTFILES) ../`cat ../.fname`/kpathsea ; \
	fi
	if [ -n "$(BINDISTLIBS)" ]; then \
	  echo $(BINDISTLIBS) >> ../`cat ../.fname`/LIBRARIES ; \
	fi
.PHONY: bin-dist

# config.make -- autoconf rules to remake the Makefile, c-auto.h, etc.

##ifdef HOSTNAME
##ac_dir = $(gnu)/share/autoconf
##autoconf = $(ac_dir)/acspecific.m4 $(ac_dir)/acgeneral.m4 $(ac_dir)/acsite.m4
##autoheader = $(ac_dir)/acconfig.h
##
### I define $(autoconf) to acgeneral.m4 and the other Autoconf files, so
### configure automatically gets remade in the sources with a new Autoconf
### release.  But it would be bad for installers with Autoconf to remake
### configure (not to mention require Autoconf), so I take out the variable
### $(autoconf) definition before release.
##configure_in = $(srcdir)/configure.in $(kpathsea_srcdir)/common.ac
##$(srcdir)/configure: $(configure_in) $(autoconf)
##	cd $(srcdir) && autoconf
##endif

config.status: $(srcdir)/configure
	$(SHELL) $(srcdir)/configure --no-create --verbose

Makefile: $(srcdir)/Makefile.in config.status $(top_srcdir)/../make/*.make
	$(SHELL) config.status

# This rule isn't used for web2c or the top-level Makefile, but it
# doesn't hurt.  We don't depend on config.status because configure
# always rewrites config.status, even when it doesn't change. Thus it
# might be newer than c-auto.h when we don't need to remake the latter.
c-auto.h: $(srcdir)/stamp-auto
$(srcdir)/stamp-auto: $(srcdir)/c-auto.h.in
	$(SHELL) config.status
	touch $(srcdir)/stamp-auto

##ifdef HOSTNAME
### autoheader reads acconfig.h (and c-auto.h.top) automatically.
##$(srcdir)/c-auto.h.in: $(srcdir)/stamp-auto.in
##$(srcdir)/stamp-auto.in: $(configure_in) $(autoheader) $(srcdir)/acconfig.h
##	cd $(srcdir) && autoheader
##	touch $(srcdir)/stamp-auto.in
##endif

info: kpathsea.info
dvi: kpathsea.dvi
INSTALL: kpathsea.texi install.texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) --no-headers $< -o - \
	  | sed -n -e '/^Installation/,/wrong fonts/'p >$@
	add-info-toc $@

# misc.make -- cleaning, etc.
TAGS: *.c *.h
	if pwd | grep kpathsea >/dev/null; then \
	  etags *.c *.h; else etags -i $(kpathsea_dir)/TAGS *.c *.h; fi

mostlyclean::
	rm -f *.o $(program) $(programs) squeeze $(library).a pic/*.o
	if $(SHARED_LIBS); then rm -f *.$(SHLEXT); fi

clean:: mostlyclean
	rm -f *.dvi *.lj

distclean:: clean
	rm -f Makefile MakeTeXPK *.pool
	rm -f config.status config.log config.cache c-auto.h 
	rm -f stamp-picdir
	-rmdir pic

# Although we can remake configure and c-auto.h.in, we don't remove
# them, since many people may lack Autoconf.  Use configclean for that.
maintainer-clean:: distclean
	rm -f TAGS *.info* configure stamp-auto

extraclean::
	rm -f *.aux *.bak *.bbl *.blg *.dvi *.log *.orig *.pl *.rej
	rm -f *.i *.s *.tfm *.vf *.vpl *\#* *gf *pk *~
	rm -f CONTENTS.tex a.out core mfput.* texput.*

configclean:
	rm -f configure c-auto.h.in c-auto.h

# Prevent GNU make 3.[59,63) from overflowing arg limit on system V.
.NOEXPORT:
# End of misc.make.

mostlyclean::
	rm -f kpsewhich
distclean::
	rm -f paths.h texmf.cnf texmf.sed so_locations
	rm -f libkpathsea.a libkpathsea.$(SHLEXT)

# rdepend.make -- rules for remaking the dependencies.
# Have to use -M, not -MM, since we use <kpathsea/...> instead of
# "kpathsea/..." in the sources.  But that means we have to remove the
# directory prefixes and all the system include files.
# And <kpathsea/paths.h> is generated, not part of the distribution.
depend depend.make:: c-auto.h $(top_srcdir)/../make/rdepend.make
	$(CC) -M $(CPPFLAGS) *.c \
	  | sed -e 's,\.\./kpathsea/,$$(kpathsea_srcdir)/,g' \
	        -e 's,$$(kpathsea_srcdir)/paths.h,paths.h,g' \
	        -e 's,/usr[^ ]* ,,g' \
	        -e 's,/usr[^ ]*$$,,g' \
	        -e 's,dvi2xx.o,dvilj.o dvilj2p.o dvilj4.o dvilj4l.o,' \
	  | grep -v '^ *\\$$' \
	  >depend.make
# End of rdepend.make.
absolute.o: absolute.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/absolute.h $(kpathsea_srcdir)/c-pathch.h
basename.o: basename.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-pathch.h
cnf.o: cnf.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-fopen.h $(kpathsea_srcdir)/c-ctype.h \
  $(kpathsea_srcdir)/cnf.h $(kpathsea_srcdir)/db.h $(kpathsea_srcdir)/str-list.h \
  $(kpathsea_srcdir)/hash.h $(kpathsea_srcdir)/line.h paths.h \
  $(kpathsea_srcdir)/pathsearch.h $(kpathsea_srcdir)/str-llist.h $(kpathsea_srcdir)/tex-file.h \
  $(kpathsea_srcdir)/variable.h
concat.o: concat.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
concat3.o: concat3.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
concatn.o: concatn.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/concatn.h $(kpathsea_srcdir)/c-vararg.h
db.o: db.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-fopen.h $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/db.h \
  $(kpathsea_srcdir)/str-list.h $(kpathsea_srcdir)/hash.h $(kpathsea_srcdir)/line.h \
  $(kpathsea_srcdir)/readable.h $(kpathsea_srcdir)/variable.h
debug.o: debug.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
dir.o: dir.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-dir.h \
  $(kpathsea_srcdir)/c-stat.h \
  $(kpathsea_srcdir)/hash.h
elt-dirs.o: elt-dirs.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/expand.h $(kpathsea_srcdir)/fn.h \
  $(kpathsea_srcdir)/pathsearch.h $(kpathsea_srcdir)/str-llist.h $(kpathsea_srcdir)/xopendir.h \
  $(kpathsea_srcdir)/c-dir.h 
expand.o: expand.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/expand.h $(kpathsea_srcdir)/pathsearch.h \
  $(kpathsea_srcdir)/str-llist.h $(kpathsea_srcdir)/tilde.h $(kpathsea_srcdir)/variable.h
extend-fname.o: extend-fname.c $(kpathsea_srcdir)/config.h ./c-auto.h \
  $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
file-p.o: file-p.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/xstat.h $(kpathsea_srcdir)/c-stat.h 
find-suffix.o: find-suffix.c $(kpathsea_srcdir)/config.h ./c-auto.h \
  $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-pathch.h
fn.o: fn.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h $(kpathsea_srcdir)/fn.h
fontmap.o: fontmap.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-fopen.h $(kpathsea_srcdir)/fontmap.h $(kpathsea_srcdir)/hash.h \
  $(kpathsea_srcdir)/line.h $(kpathsea_srcdir)/pathsearch.h $(kpathsea_srcdir)/str-llist.h \
  $(kpathsea_srcdir)/str-list.h
getopt.o: getopt.c \
  getopt.h
getopt1.o: getopt1.c getopt.h 
hash.o: hash.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/hash.h $(kpathsea_srcdir)/str-list.h
itoa.o: itoa.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
kdefault.o: kdefault.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/default.h
kpsewhich.o: kpsewhich.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/getopt.h $(kpathsea_srcdir)/line.h $(kpathsea_srcdir)/proginit.h \
  $(kpathsea_srcdir)/progname.h $(kpathsea_srcdir)/tex-file.h $(kpathsea_srcdir)/tex-glyph.h \
  $(kpathsea_srcdir)/variable.h
kpsexpand.o: kpsexpand.c \
  tex-file.h $(kpathsea_srcdir)/c-proto.h $(kpathsea_srcdir)/types.h
line.o: line.c 
magstep.o: magstep.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/magstep.h
make-suffix.o: make-suffix.c $(kpathsea_srcdir)/config.h ./c-auto.h \
  $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
path-elt.o: path-elt.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/pathsearch.h $(kpathsea_srcdir)/str-llist.h
pathsearch.o: pathsearch.c $(kpathsea_srcdir)/config.h ./c-auto.h \
  $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-fopen.h $(kpathsea_srcdir)/absolute.h $(kpathsea_srcdir)/expand.h \
  $(kpathsea_srcdir)/db.h $(kpathsea_srcdir)/str-list.h $(kpathsea_srcdir)/pathsearch.h \
  $(kpathsea_srcdir)/str-llist.h $(kpathsea_srcdir)/readable.h $(kpathsea_srcdir)/variable.h 
proginit.o: proginit.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/proginit.h $(kpathsea_srcdir)/progname.h \
  $(kpathsea_srcdir)/tex-file.h
progname.o: progname.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/progname.h
putenv.o: putenv.c ./c-auto.h
readable.o: readable.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-stat.h \
  $(kpathsea_srcdir)/readable.h $(kpathsea_srcdir)/truncate.h
rm-suffix.o: rm-suffix.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
str-list.o: str-list.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/str-list.h
str-llist.o: str-llist.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/str-llist.h
tex-file.o: tex-file.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-vararg.h $(kpathsea_srcdir)/cnf.h $(kpathsea_srcdir)/default.h \
  $(kpathsea_srcdir)/expand.h paths.h $(kpathsea_srcdir)/pathsearch.h \
  $(kpathsea_srcdir)/str-llist.h $(kpathsea_srcdir)/tex-file.h
tex-glyph.o: tex-glyph.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/absolute.h $(kpathsea_srcdir)/expand.h $(kpathsea_srcdir)/fontmap.h \
  $(kpathsea_srcdir)/hash.h $(kpathsea_srcdir)/pathsearch.h $(kpathsea_srcdir)/str-llist.h \
  $(kpathsea_srcdir)/tex-glyph.h $(kpathsea_srcdir)/tex-file.h $(kpathsea_srcdir)/tex-make.h \
  $(kpathsea_srcdir)/variable.h
tex-make.o: tex-make.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-fopen.h $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/concatn.h \
  $(kpathsea_srcdir)/c-vararg.h $(kpathsea_srcdir)/db.h $(kpathsea_srcdir)/str-list.h \
  $(kpathsea_srcdir)/fn.h $(kpathsea_srcdir)/magstep.h $(kpathsea_srcdir)/readable.h \
  $(kpathsea_srcdir)/tex-make.h $(kpathsea_srcdir)/tex-file.h $(kpathsea_srcdir)/variable.h
tilde.o: tilde.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/tilde.h 
truncate.o: truncate.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-namemx.h $(kpathsea_srcdir)/c-pathch.h $(kpathsea_srcdir)/c-pathmx.h \
  $(kpathsea_srcdir)/truncate.h
uppercasify.o: uppercasify.c $(kpathsea_srcdir)/config.h ./c-auto.h \
  $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-ctype.h 
variable.o: variable.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/c-ctype.h $(kpathsea_srcdir)/cnf.h \
  $(kpathsea_srcdir)/fn.h $(kpathsea_srcdir)/variable.h
version.o: version.c
xcalloc.o: xcalloc.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
xfopen.o: xfopen.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
xmalloc.o: xmalloc.c 
xopendir.o: xopendir.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/xopendir.h $(kpathsea_srcdir)/c-dir.h 
xputenv.o: xputenv.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
xrealloc.o: xrealloc.c 
xstat.o: xstat.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h \
  $(kpathsea_srcdir)/xstat.h $(kpathsea_srcdir)/c-stat.h 
xstrdup.o: xstrdup.c $(kpathsea_srcdir)/config.h ./c-auto.h $(kpathsea_srcdir)/c-std.h \
  $(kpathsea_srcdir)/c-unistd.h $(kpathsea_srcdir)/systypes.h \
  $(kpathsea_srcdir)/c-memstr.h \
  $(kpathsea_srcdir)/c-errno.h $(kpathsea_srcdir)/c-minmax.h \
  $(kpathsea_srcdir)/c-limits.h \
  $(kpathsea_srcdir)/c-proto.h \
  $(kpathsea_srcdir)/lib.h $(kpathsea_srcdir)/types.h $(kpathsea_srcdir)/debug.h
