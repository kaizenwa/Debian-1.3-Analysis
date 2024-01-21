#
# Makefile for the Yorick interpreter
#
# $Id$
#
# ---------------------------------------------------------------------------

# Yorick Makefiles have a generic portion called Maketmpl, plus
# a section like this which is specific to the particular version.
# The configuration work is all in Maketmpl.
MAKE_TEMPLATE = ./Maketmpl

# This flag should be set to whatever your compiler needs to tell it
# to optimize (-O usually) or debug (-g usually).
# The top level Makefile targets debug: and optimize: can be used to
# set these consistently in all Yorick distribution Makefiles.
# C_OPTIMIZE = -g
C_OPTIMIZE = -O

# Ordinarily you need to feed the loader the same optimization flags
# you've fed the compiler.  However, with some brain-dead systems, the
# flags required to get highly optimized compiles cause the loader to
# fail, so you may need to set separate optimization flags.
LD_OPTIMIZE = $(C_OPTIMIZE)

# ---------------------------------------------------------------------------

CODE_NAME = yorick

CODE_LIBRARY = libyor.a

OBJS = task.o yorick.o yinput.o parse.o ydata.o yio.o fnctn.o array.o \
       range.o ops.o ops0.o ops1.o ops2.o ops3.o opsv.o nonc.o debug.o \
       std0.o std1.o std2.o ascio.o defstr.o defmem.o hash.o sysdep.o \
       yrdwr.o bcast.o binio.o binobj.o binstd.o cache.o convrt.o \
       binpdb.o clog.o $(GRAPH_O) list.o fortrn.o

# Codger actually ignores both std.i and graph.i where Y_INCLUDE appears
# on its execute line.  These just allow the depedencies for the ycode.c
# target to be correct in this Makefile.  Makefiles for custom versions
# of Yorick do not put std.i and graph.i here and work the way you'd
# intuitively expect.
Y_INCLUDE = std.i $(GRAPH_I)

CLEAN_UP = yorick fmcalc codger prmtyp.h grammar
TIDY_UP= y.tab.c y.output M.1 M.2 junk* include/*~ doc/*~

# Unused -- may be defined in other Yorick packages (e.g.- MathC, Drat)
NON_SHARABLE = unused
F_OPTIMIZE =
FORTRAN_STYLE =
Y_OTHERS =
YWRAP_O =
PKG_LIBS =
PKG_OBJS =
SYS_LIBS =

# ---------------------------------------------------------------------------

all:: codger startup main.o $(CODE_LIBRARY) $(CODE_NAME)

codger: codger.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(FORTRAN_LINKAGE) -o codger codger.c

prmtyp.h: fmcalc
	./fmcalc prmtyp.h

grammar: yorick.y
	$(YACC) -v yorick.y
	@echo "*** Expect 1 shift/reduce conflict above ***"
	@$(MV) y.output $@
	@$(RM) y.tab.c

startup::
	../install-sh 0 startup
	cp paths.i std.i stdx.i graph.i startup

check: $(CODE_NAME)
	./yorick -batch check.i
	$(RM) junk*

DESTDIR =

install: codger $(CODE_LIBRARY) main.o installdirs
	../install-sh 2 $(DESTDIR)$(Y_SITE)
	cp paths.i std.i stdx.i graph.i $(DESTDIR)$(Y_SITE)/startup
	cp Maketmpl $(DESTDIR)$(Y_HOME)
	if test -r Make-cxx; then cp Make-cxx $(DESTDIR)$(Y_HOME); fi
	cp *.h main.c $(DESTDIR)$(Y_INCDIR)
	../install-sh 4 $(DESTDIR)$(Y_BINDIR) yorick
	cp libyor.a codger main.o $(DESTDIR)$(Y_LIBDIR)
	$(RANLIB) $(DESTDIR)$(Y_LIBDIR)/libyor.a

uninstall:
	../install-sh 3 $(DESTDIR)$(Y_SITE)
	cd $(DESTDIR)$(Y_SITE)/startup; $(RM) paths.i std.i stdx.i graph.i;
	cd $(DESTDIR)$(Y_INCDIR); $(RM) *.h main.c;
	cd $(DESTDIR)$(Y_LIBDIR); $(RM) libyor.a codger main.o;
	$(RM) $(DESTDIR)$(Y_BINDIR)/yorick
	$(RM) $(DESTDIR)$(Y_HOME)/Maketmpl
	$(RM) $(DESTDIR)$(Y_HOME)/Make-cxx

installdirs:
	../install-sh 0 $(DESTDIR)$(Y_SITE)
	../install-sh 0 $(DESTDIR)$(Y_SITE)/startup
	../install-sh 0 $(DESTDIR)$(Y_SITE)/include
	../install-sh 0 $(DESTDIR)$(Y_SITE)/doc
	../install-sh 0 $(DESTDIR)$(Y_BINDIR)
	../install-sh 0 $(DESTDIR)$(Y_LIBDIR)
	../install-sh 0 $(DESTDIR)$(Y_INCDIR)
	../install-sh 0 $(DESTDIR)$(Y_HOME)

distclean: clean
	$(RM) Maketmpl Make-cxx

CXS_1 = 's/Y_LD = $$(CC)/Y_LD = $$(CXX)/'
CXS_2 = 's/YORMAIN = $$(Y_LIBDIR)/YORMAIN = /'
cxx:
	$(SED) -e 's/fake_//' -e $(CXS_1) -e $(CXS_2) Maketmpl >Make-cxx

# ---------------------------------------------------------------------------

# Note: indirectly included headers separated by three spaces

task.o: ydata.h yio.h defstr.h defmem.h   binio.h hash.h sysdep.h
# Do NOT use yorick.y -- most YACCs have limited parse stack size
yorick.c: yorick.y
	@touch yorick.c
yorick.o: yorick.c parse.h defstr.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_NO_STRTOUL) -c yorick.c
yinput.o: yio.h parse.h defstr.h defmem.h hash.h sysdep.h
parse.o: parse.h ydata.h defmem.h defstr.h   binio.h hash.h
ydata.o: bcast.h defmem.h defstr.h   ydata.h binio.h hash.h
yio.o: yio.h defstr.h defmem.h ydata.h   binio.h hash.h sysdep.h
fnctn.o: ydata.h   binio.h hash.h
array.o: ydata.h   binio.h hash.h
range.o: ydata.h   binio.h hash.h
ops.o: bcast.h defmem.h defstr.h   ydata.h binio.h hash.h
ops0.o: ydata.h   binio.h hash.h
ops1.o: ydata.h   binio.h hash.h
ops2.o: ydata.h defstr.h   binio.h hash.h
ops3.o: bcast.h defstr.h   ydata.h binio.h hash.h
opsv.o: ydata.h defmem.h prmtyp.h   binio.h hash.h
nonc.o: sysdep.h
debug.o: ydata.h yio.h defstr.h   binio.h hash.h sysdep.h
std0.o: std0.c bcast.h yio.h defstr.h   ydata.h binio.h hash.h sysdep.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_Y_VERSION) -c std0.c
std1.o: ydata.h defstr.h   binio.h hash.h
std2.o: ydata.h yio.h defstr.h   binio.h hash.h sysdep.h
ascio.o: ydata.h yio.h defstr.h defmem.h   binio.h hash.h sysdep.h
list.o: ydata.h defmem.h   binio.h hash.h
defstr.o: defstr.h defmem.h
defmem.o: defmem.h
hash.o: hash.h defstr.h defmem.h
SYSDEP_DEFS = $(HAS_BSD_TIMER) $(D_HAS_LIBMSET) $(D_HAS_GETCWD)
sysdep.o: sysdep.c ydata.h yio.h defstr.h defmem.h   binio.h hash.h sysdep.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(SYSDEP_DEFS) -c sysdep.c
yrdwr.o: bcast.h defmem.h   ydata.h binio.h hash.h
bcast.o: bcast.h   ydata.h binio.h hash.h
binio.o: ydata.h yio.h defmem.h defstr.h   binio.h hash.h sysdep.h
binobj.o: bcast.h defmem.h defstr.h   ydata.h binio.h hash.h
binstd.o: binio.h defmem.h   hash.h
cache.o: binio.h defmem.h defstr.h yio.h   hash.h sysdep.h
convrt.o: binio.h defstr.h defmem.h   hash.h
binpdb.o: ydata.h yio.h defstr.h defmem.h   binio.h hash.h sysdep.h
clog.o: ydata.h yio.h defstr.h defmem.h   binio.h hash.h sysdep.h
graph0.o: ydata.h   binio.h hash.h

graph.o: graph.c ydata.h yio.h defstr.h   binio.h hash.h sysdep.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_NO_XLIB) $(GISTINCL) -c graph.c
style.o: style.c ydata.h defstr.h   binio.h hash.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(GISTINCL) -c style.c

fortrn.o: fortrn.c defmem.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(FORTRAN_LINKAGE) -c fortrn.c

# Always compile main.c with -g so new packages can be debugged.
main.o: main.c
	$(CC) $(CPPFLAGS) -g -c main.c

# ---------------------------------------------------------------------------

include $(MAKE_TEMPLATE)
