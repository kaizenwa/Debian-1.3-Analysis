# Generated automatically from Makefile.in by configure.

# Common prefix for installation directories.
# NOTE: This directory must exist when you start the install.
prefix		= /usr/local
exec_prefix	= $(prefix)
# Where to put the executables.
bindir		= $(exec_prefix)/bin
# Where to put the man pages.
manext		= 1
mandir		= $(exec_prefix)/man/man$(manext)
# Where to put the Info files.
infodir		= $(prefix)/info
# Where the source is
srcdir		= .

YACC	= yacc
CC	= gcc
CFLAGS	= -g -O
DEFS	= -DHAVE_CONFIG_H
LIBS	=  -ltermlib -lm  -lcurses -L/usr/local/lib  -lX11
INCLUDEDIR = -I. -I$(srcdir)
LN	= ln -s
RM	= rm -f
SHELL	= /bin/sh
INSTALL	= install
INSTALL_PROGRAM	= $(INSTALL)
INSTALL_DATA	= $(INSTALL)
INSTALLFLAGS	= -c
INSTPGMFLAGS	= -s

           DEPEND = makedepend
      DEPENDFLAGS =
           CCLINK = $(CC)
        LDOPTIONS = $(CDEBUGFLAGS) $(CCOPTIONS)  $(EXTRA_LDOPTIONS) $(THREADS_LDFLAGS) $(LOCAL_LDFLAGS) $(LDPRELIB)
     CXXLDOPTIONS = $(CXXDEBUGFLAGS) $(CXXOPTIONS) $(EXTRA_LDOPTIONS) $(THREADS_CXXLDFLAGS) $(LOCAL_LDFLAGS) $(LDPRELIB)
        MKDIRHIER = mkdirhier
             LINT = lint
         LINTOPTS = -axz
        LINTFLAGS = $(LINTOPTS) -DLINT $(ALLDEFINES) $(DEPEND_DEFINES)

# clear out the suffix list, then set it to what we need
.SUFFIXES:
.SUFFIXES: .o .c .y

# Warning - edit config.h if you want to change various things...

# All of the source files
# psc.c - Convert ascii input to SC/XSpread format.

SRCS =	cmds.c color.c crypt.c format.c gram.c graphic_main.c help.c \
	interp.c lex.c matrix.c plot_XY.c plot_bar.c plot_line.c \
	plot_pie.c plot_stk_bar.c range.c sc.c scXstuff.c screen.c \
	search.c sort.c utils.c version.c vi.c vmtbl.c xmalloc.c

OBJS =	cmds.o color.o crypt.o format.o graphic_main.o help.o interp.o \
	matrix.o plot_XY.o plot_bar.o plot_line.o plot_pie.o \
	plot_stk_bar.o range.o sc.o scXstuff.o screen.o search.o sort.o \
	utils.o version.o vi.o vmtbl.o xmalloc.o

HDRS =	config.h graphic_gvar.h pattern.h plot.h sc.h scXstuff.h \
	experres.h statres.h $(YTAB).h

#.c.o: $(HDRS)
#	echo hi mom
#	$(CC) $(INCLUDEDIR) $(CFLAGS) -c $< -o $@

# MS-DOS needs y_tab instead of the normal y.tab
#YTAB=y_tab
YTAB=y.tab

# For OSF(DEC Alpha things) add -ldnet_stub
EXTRA_LIBRARIES = $(XLIB) -lm -lcurses -ltermcap

PROGRAM = xspread

all:: $(PROGRAM) p$(PROGRAM)

$(PROGRAM): $(OBJS) $(DEPLIBS) gram.o lex.o
	$(RM) $@
	$(CCLINK) -o $@ $(LDOPTIONS) $(OBJS) gram.o lex.o $(LOCAL_LIBRARIES) $(LIBS) $(EXTRA_LOAD_FLAGS)

install:: xspread
	$(INSTALL_PROGRAM) $(INSTALLFLAGS) $(INSTPGMFLAGS) xspread $(bindir)/xspread

install:: pxspread
	$(INSTALL_PROGRAM) $(INSTALLFLAGS) $(INSTPGMFLAGS) pxspread $(bindir)/pxspread

uninstall::
	-$(RM) $(bindir)/xspread
	-$(RM) $(bindir)/pxspread


install.man:: xspread.man
	$(INSTALL_DATA) $(INSTALLFLAGS) $(INSTMANFLAGS) xspread.man $(mandir)/xspread.$(manext)
	$(INSTALL_DATA) $(INSTALLFLAGS) $(INSTMANFLAGS) pxspread.man $(mandir)/pxspread.$(manext)

uninstall::
	-$(RM) $(mandir)/xspread.$(manext)
	-$(RM) $(mandir)/pxspread.$(manext)

depend::
	$(DEPEND) $(DEPENDFLAGS) -- $(ALLDEFINES) $(DEPEND_DEFINES) -- $(SRCS)

lint:
	$(LINT) $(LINTFLAGS) $(SRCS) $(LINTLIBS)
lint1:
	$(LINT) $(LINTFLAGS) $(FILE) $(LINTLIBS)

#.c.o:
#	$(CC) $(INCLUDEDIR) $(CFLAGS) -c $< -o $@ 

$(OBJS):	$(HDRS)
	$(RM) $@
	$(CC) $(INCLUDEDIR) -c $(CFLAGS) $(LOCAL_DEFINES) $*.c

experres.h: gram.y eres.sed
	$(RM) experres.h
	sed < $(srcdir)/gram.y > experres.h -f $(srcdir)/eres.sed

statres.h: gram.y sres.sed
	$(RM) statres.h
	sed < $(srcdir)/gram.y > statres.h -f $(srcdir)/sres.sed

gram.o: sc.h $(YTAB).h gram.c experres.h statres.h
	$(CC) $(INCLUDEDIR) ${CFLAGS} -c $(srcdir)/gram.c 

$(YTAB).h: gram.c
gram.c: gram.y
	$(RM) $(YTAB).h $(YTAB).c gram.c
	$(YACC) -d gram.y
	mv $(YTAB).c gram.c

lex.o:	sc.h $(YTAB).h gram.o lex.c
	$(CC) $(INCLUDEDIR) ${CFLAGS} -c lex.c 

# 'psc' Ascii to sc format converter
pvmtbl.o: sc.h vmtbl.c config.h
	-$(RM) pvmtbl.c
	${LN} vmtbl.c pvmtbl.c
	$(CC) $(INCLUDEDIR) ${CFLAGS} -c -DPSC pvmtbl.c 
	$(RM) pvmtbl.c

putils.o: sc.h utils.c config.h
	-$(RM) putils.c
	${LN} utils.c putils.c
	$(CC) $(INCLUDEDIR) ${CFLAGS} -c -DPSC putils.c 
	$(RM) putils.c

pxmalloc.o: sc.h xmalloc.c config.h
	-$(RM) pxmalloc.c
	${LN} xmalloc.c pxmalloc.c
	$(CC) $(INCLUDEDIR) ${CFLAGS} -c -DPSC pxmalloc.c 
	$(RM) pxmalloc.c

p$(PROGRAM):	psc.c pvmtbl.o pxmalloc.o putils.o version.o
	$(CC) $(INCLUDEDIR) $(CFLAGS) ${LDFLAGS} -o $@ psc.c pvmtbl.o pxmalloc.o putils.o version.o ${PSCLIB} $(LIBS)

emptyrule:

clean:
	-$(RM) *.CKP *.ln *.BAK *.bak *.o core errs
	-$(RM) ,* *~ *.a .emacs_* tags TAGS make.log MakeOut "#"*
	-$(RM) $(PROGRAM) p$(PROGRAM)

distclean: clean
	-$(RM) Makefile config.h
	-$(RM) config.log config.cache

maintainerclean: distclean
	-$(RM) gram.c $(YTAB).h experres.h statres.h

tags:
	ctags *.[chy]

TAGS:
	etags *.[chy]

install::
	@echo "install in $(CURRENT_DIR) done"

uninstall::
	@echo "uninstall of $(PROGRAM) done"

install.man::
	@echo "install.man in $(CURRENT_DIR) done"

install.linkkit::
	@echo "install.linkkit in $(CURRENT_DIR) done"

# finish this (like test)
test: check
check: $(PROGRAM)
	cd tests; $(MAKE)

# For an explanation of the following Makefile rules, see node
# `Automatic Remaking' in GNU Autoconf documentation.
# check to make sure configure has been run...
Makefile: $(srcdir)/Makefile.in config.status
	CONFIG_FILES=$@ CONFIG_HEADERS= ./config.status
config.status: configure
	./config.status --recheck
# maintainer things: need to make sure config_h.in & configure are up to date
# next 3 require GNU autoconfig to run
configure: configure.in
	cd $(srcdir) && autoconf
config.h: stamp-h
stamp-h: config_h.in config.status
	CONFIG_FILES= CONFIG_HEADERS=config.h:config_h.in ./config.status
	echo timestamp >stamp-h
config.h.in: acconfig.h configure.in
	cd $(srcdir) && autoheader

includes:

# -----------------------------------------------------------------------
# dependencies generated by makedepend
