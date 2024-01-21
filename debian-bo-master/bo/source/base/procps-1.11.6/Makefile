# Makefile for procps.  Chuck Blake.
# Portions of this are highly dependent upon features specific to GNU make

# shared lib versions
VERSION      =  1
SUBVERSION   =  11

PREFIX     =  #proc# prefix for program names
MAN1DIR    =  /usr/man/man1
MAN7DIR    =  /usr/man/man7
MAN8DIR    =  /usr/man/man8
BINDIR     =  /bin
SBINDIR    =  /sbin
USRBINDIR  =  /usr/bin
INSTALLBIN =  install --owner bin --group 0 --mode a=rx --strip
INSTALLMAN =  install --owner man --group 0 --mode a=r
ARCH       =  $(shell arch)

BPROG      =  ps #						-> BINDIR
UPROG      =  uptime tload free w top vmstat watch # sessreg -> USRBINDIR
SPROG      =  psupdate #					-> SBINDIR
MAN1       =  ps.1 uptime.1 tload.1 free.1 w.1 top.1 sessreg.1 watch.1
MAN7       =  ps_color.7 ps_fields.7
MAN8       =  psupdate.8 vmstat.8

SUBDIRS    =  skill psmisc xproc xcpustate # sub-packages to build/install

CC         =  gcc -pipe -O3 -fomit-frame-pointer #easy to command-line override
CFLAGS     =  -I. -Wall -Wunused -fwritable-strings $(PROC_OPTIONS)
SHARED     =  1# build/install both a static and ELF shared library
SHLIBDIR   =  /lib# where to install the shared library

# Compile-time Options for libproc, top, ps and w.
# -DPROC_COLOR        enables color support
# -DNEW_TIME_DEFAULT  makes the new time format the default one

PROC_OPTIONS = -DPROC_COLOR #-DNEW_TIME_DEFAULT


LDFLAGS    =  -Wl,-warn-common #	recommended for ELF systems
#LDFLAGS    =  -qmagic #		recommended for a.out systems
#LDFLAGS    =  -Xlinker -qmagic #	older a.out systems may need this
#LDFLAGS    =  -N #			still older a.out systems use this

LIBCURSES  =  -lncurses# watch is the only thing that needs this
#LIBCURSES  =  -lcurses -ltermcap# BSD Curses requires termcap
LIBTERMCAP =  -lncurses # if you dont't have termcap, use -lncurses
EXTRALIBS  =  # -lshadow

INPLACE_CF = -Wall -Wunused -fwritable-strings $(PROC_OPTIONS)
INPLACE_TGT = -lproc

ifeq ($(ARCH),alpha)
 EXTRALIBS += -lbfd -liberty
endif

ifeq ($(ARCH),sparc)
 EXTRALIBS += -lbfd -liberty
endif

W_SHOWFROM =  -DW_SHOWFROM# don't show remote host users are logged in from.

#----------------------------------------------------#
# End of user-configurable portion of the Makefile.  #
# You should not need to modify anything below this. #
#----------------------------------------------------#
BUILD = $(BPROG) $(UPROG) $(SPROG) $(SUBDIRS)

# BUILD LIBRARIES + PROGRAMS
all: $(BUILD)

inplace:
	make CFLAGS="$(INPLACE_CF)" LIB_TGT=$(INPLACE_TGT) all

# INSTALL PROGRAMS + DOCS
install: $(patsubst %,install_%,$(BUILD) $(MAN1) $(MAN8) $(MAN7))
ifeq ($(SHARED),1)
	install --owner 0 --group 0 --mode a=rx $(LIB_TGT) $(SHLIBDIR)
endif

# INSTALL LIBRARIES + HEADERS (OPTIONAL)
libinstall:
	$(MAKE) -C proc install $(LIBPROCPASS)

clean:
	$(RM) $(OBJ) $(BPROG) $(UPROG) $(SPROG) *~
	for i in proc $(SUBDIRS); do $(MAKE) -C $$i clean; done

distclean: clean
	for i in proc $(SUBDIRS); do $(MAKE) -C $$i clean; done
	$(MAKE) -C psmisc spotless
	$(RM) $(OBJ) $(BPROG) $(UPROG) $(SPROG) xproc/Makefile* xcpustate/Makefile* \
	      proc/signames.h proc/.depend do_install

checkout:
	$(CO) $(RCSFILES)
checkclean:
	$(RM) $(RCSFILES)
realclean: clean checkclean

#-----------------------------------------------------#
# End of user-callable make targets.                  #
# You should not need to read anything below this.    #
#-----------------------------------------------------#

.PHONY:	all install libinstall clean distclean checkout checkclean realclean
.PHONY: $(patsubst %,install_%, $(BPROG) $(UPROG) $(SPROG))
.PHONY: proc psmisc skill xproc xcpustate
.PHONY: $(patsubst %,build_%, proc psmisc skill xproc xcpustate)
.PHONY: $(patsubst %,install_%, proc psmisc skill xproc xcpustate)

LIBPROCPASS  =  SHARED=$(SHARED) SHLIBDIR=$(SHLIBDIR) VERSION=$(VERSION) SUBVERSION=$(SUBVERSION) PROC_COLOR="$(PROC_OPTIONS)" CC="$(CC)"

# libproc setup

ifeq ($(SHARED),1)
    LIB_TGT = proc/libproc.so.$(VERSION).$(SUBVERSION)
else
    LIB_TGT = proc/libproc.a
endif

$(LIB_TGT): $(wildcard proc/*.[ch])
	$(MAKE) -C proc `basename $(LIB_TGT)` $(LIBPROCPASS) all

# component package setup -- the pattern should be obvious: A build rule and
# unified executable+documentation install rule. (An extra makefile rule is
# needed for those packages which use Imake.)

psmisc:             build_psmisc
build_psmisc:		; $(MAKE) -C psmisc LIBCURSES=$(LIBCURSES)
install_psmisc:     psmisc		; $(MAKE) -C psmisc install

skill:              build_skill
build_skill:				; $(MAKE) -C skill
install_skill:      skill		; $(MAKE) -C skill install

xproc:              build_xproc
build_xproc:        xproc/Makefile	; $(MAKE) -C xproc
install_xproc:      xproc		; $(MAKE) -C xproc install
xproc/Makefile:     xproc/Imakefile	; cd xproc; xmkmf -a

xcpustate:          build_xcpustate
build_xcpustate:    xcpustate/Makefile	; $(MAKE) -C xcpustate
install_xcpustate:  xcpustate		; $(MAKE) -C xcpustate install
xcpustate/Makefile: xcpustate/Imakefile	; cd xcpustate; xmkmf -a

# executable dependencies

ps top w uptime tload free vmstat sessreg utmp psupdate : $(LIB_TGT)

# static pattern build/link rules:

%.o : %.c
	$(CC) $(CFLAGS) -c $^

top.o: top.c top.h
	$(CC) $(CFLAGS) -c $<

ps w uptime tload free vmstat sessreg utmp: % : %.o
	$(strip $(CC) $(LDFLAGS) -o $@ $< $(LIB_TGT) $(EXTRALIBS))

# special instances link rules (need extra libraries/objects)

psupdate:%: %.o aoutupdate.o elfupdate.o bfdupdate.o
	$(strip $(CC) $(SLDFLAGS) -o $@ $^ $(EXTRALIBS))

top:   % : %.o
	$(strip $(CC)  $(LDFLAGS) -o $@ $^ $(LIB_TGT) $(LIBTERMCAP) $(EXTRALIBS))

watch:	% : %.o
	$(strip $(CC) $(SLDFLAGS) -o $@ $< $(LIBCURSES) $(EXTRALIBS))

# special instances of compile rules (need extra defines)
w.o:	w.c
	$(strip $(CC) $(CFLAGS) $(W_SHOWFROM) -c $< $(EXTRALIBS))

sessreg.o:	sessreg.c
	$(strip $(CC) $(CFLAGS) -DSYSV -c $<)

# static pattern installation rules

$(patsubst %,install_%,$(BPROG)): install_%: %
	$(INSTALLBIN) $< $(BINDIR)/$(PREFIX)$<
$(patsubst %,install_%,$(UPROG)): install_%: %
	$(INSTALLBIN) $< $(USRBINDIR)/$(PREFIX)$<
$(patsubst %,install_%,$(SPROG)): install_%: %
	$(INSTALLBIN) $< $(SBINDIR)/$(PREFIX)$<
$(patsubst %,install_%,$(MAN1)) : install_%: %
	$(INSTALLMAN) $< $(MAN1DIR)/$(PREFIX)$<
$(patsubst %,install_%,$(MAN8)) : install_%: %
	$(INSTALLMAN) $< $(MAN8DIR)/$(PREFIX)$<
$(patsubst %,install_%,$(MAN7)) : install_%: %
	$(INSTALLMAN) $< $(MAN7DIR)/$(PREFIX)$<

# miscellaneous useful macros

VC_SUF   =  ,v
VC_PFX   =  RCS/
RCSFILES =  $(patsubst $(VC_PFX)%$(VC_SUF),%,$(wildcard $(VC_PFX)*$(VC_SUF)))
SRC      =  $(sort $(wildcard *.c) $(filter %.c,$(RCSFILES)))
OBJ      =  $(SRC:.c=.o)
