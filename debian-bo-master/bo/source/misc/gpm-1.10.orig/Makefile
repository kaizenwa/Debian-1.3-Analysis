# Makefile (gpm-Linux)
#
# Copyright 1994   rubini@ipvvis.unipv.it
#
####
#
# The main configuration parameters are in gpmCfg.h

# To compile with "-g -Wall -DDEBUG" and without stripping
# uncomment the following line.
#DEBUG = t

INSTALL = install -c
INSTALLBIN = $(INSTALL) -m 755
INSTALLLIB = $(INSTALL) -m 644

ifndef PREFIX
  PREFIX = /usr
endif
LIBDIR = $(PREFIX)/lib
BINDIR = $(PREFIX)/bin
INCDIR = $(PREFIX)/include
MANDIR = $(PREFIX)/man/man1
INFODR = $(PREFIX)/info
LSPDIR = $(PREFIX)/lib/emacs/site-lisp

############## no more editing...

RELEASE = 1.10
REL_MAJ = 1

TARGET = gpm
   TOBJS = gpm.o gpn.o mice.o special.o


CLIENTOBJS = mev.o gpm-root.o
CLIENTS = $(CLIENTOBJS:.o=)

EXTRAS = hltest mouse-test


ifdef DEBUG
  # we need -O, since the new includes define inline functions
  CFLAGS = -g -O -DDEBUG -Wall
  LDFLAGS = -L.
else
  CFLAGS = -O3 -fomit-frame-pointer
  LDFLAGS = -L. -s
endif

LOADLIBES	= -lcurses -ltermcap
#LOADLIBES	= -lncurses

LIB = lib$(TARGET).a
   LOBJS = liblow.o libhigh.o libcurses.o libxtra.o


  ISELF = n
#
# Are we using an elf compiler? Use $(CC)
#

ifeq ($(shell $(CC) -E -dM short.c | awk '$$2 == "__ELF__" {print "y"}'),y)

  ISELF = y
  SHLIB = lib$(TARGET).so.$(RELEASE)
  SHLIB0 = lib$(TARGET).so.$(REL_MAJ)
  SHLIB00 = lib$(TARGET).so
  SHLOBJS = $(LOBJS:.o=.sh.o)

  %.sh.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $*.c -o $@

endif


ELFILE = t-mouse.el
ELCFILE = $(ELFILE:.el=.elc)
LFILES = $(ELFILE) $(ELCFILE)

%.elc: %.el
	emacs -batch -l exec.el -exec '(byte-compile-file "$<")'


INCLUDES = $(TARGET).h
INFO = doc/$(TARGET).info
MAN = [gm]*.1

ifeq ($(RANLIB),)
   RANLIB = ranlib
endif

# if not Linux, print a message and die
ifneq ($(shell uname),Linux)
beforeall: check
endif

# force make depend
ifneq (.depend,$(wildcard .depend))
all: depend
endif


# extra options...

   CFLAGS += $(shell ./configure)

all: $(TARGET) $(LIB) $(CLIENTOBJS) $(CLIENTS) $(EXTRAS) $(SHLIB) \
	$(ELCFILE) $(TARGET).1 info

info: $(INFO)

check:
	@if [ `uname` != Linux ]; then \
	  echo "" ;\
	  echo "**** This package can compile only under Linux, by now."; \
	  echo "**** Please read the docs (after \"make -C doc\") "; \
	  echo "****    and compile the portable client " \
	       "under \"sample\", instead"; \
	  echo "" ;\
	  exit 1; fi

$(TARGET): $(TOBJS)
	rm -f $@
	$(CC) $(CFLAGS) $^ -o $@

static::
	$(MAKE) CC="$(CC) -static"

$(TARGET).static: $(TOBJS)
	rm -f $@
	$(CC) -static $(CFLAGS) $^ -o $@
	

$(LIB): $(LOBJS)
	$(AR) r $(LIB) $(LOBJS)
	$(RANLIB) $(LIB)

$(SHLIB): $(SHLOBJS)
	$(CC) -shared -Wl,-soname,$(SHLIB0) \
	 -o $@ $(SHLOBJS) -lcurses
	ln -sf $(SHLIB) $(SHLIB0)
	ln -sf $(SHLIB0) $(SHLIB00)
# Aha! The line above can give problems: "-lcurses", "-lncurses" or neither?


$(CLIENTS): $(LIB)

$(EXTRAS): $(LIB)

mouse-test: mouse-test.o mice.o 
	$(CC) $(CFLAGS) $^ -o $@
	
$(INFO): doc/doc.$(TARGET)
	-@$(MAKE) -C doc info || \
		(echo "Warning: failed to make the info file"; exit 1)

$(TARGET).1: doc/doc.$(TARGET) doc/manpager
	doc/manpager $<
	
############## misc targets

.PHONY: install clean cleanest check distrib asciidoc terse tar bin\
             mpage sample sample-dist depend dep DEPEND DEP

.PRECIOUS: *.o

install: all
	@for i in $(BINDIR) $(LIBDIR) $(INCDIR) $(INFODR) $(MANDIR) $(LSPDIR); do\
		if [ -d $$i ]; then set +x; else (set -x; mkdir -p $$i); fi; \
	done
	$(INSTALLBIN) $(TARGET) $(CLIENTS) $(BINDIR) 
	$(INSTALLLIB) $(LFILES)            $(LSPDIR)
	$(INSTALLLIB) $(LIB)               $(LIBDIR)

### the elf shlib:
ifeq ($(ISELF),y)

	$(INSTALLBIN) $(SHLIB)            $(LIBDIR)
	cd $(LIBDIR) && ln -sf $(SHLIB)  $(SHLIB0)
	cd $(LIBDIR) && ln -sf $(SHLIB0) $(SHLIB00)
	sync
	@echo "Running ldconfig"
	-ldconfig
endif

	$(INSTALLLIB) $(INCLUDES)          $(INCDIR)
	-$(INSTALLLIB) $(INFO)             $(INFODR)
	$(INSTALLLIB) $(MAN)               $(MANDIR)
	@echo ""
	@echo -n "Now update your /etc/gpm-root.conf "
	@echo    "by editing a copy of ./gpm-root.conf"

quickinst:
	(cd bin && tar cf - *) | (cd $(PREFIX) && tar xvf -)
	
terse:
	rm -f *~ */*~ */*.dvi */*.log

clean: terse
	rm -f .depend TAGS Test.log
	$(MAKE) -C doc clean
	rm -f $(TARGET) $(TARGET).static $(LIB) $(SHLIB) \
	     $(SHLIB0) $(SHLIB00) $(CLIENTS) $(EXTRAS) *.o
	rm -f *.1

cleanest: clean
	rm -r bin
printv:
		@grep -n RELEASE *.h doc/* sample/*.c Makefile
	@sed -n -e '/DEBUG =/p' -e '/DEBUG =/q' Makefile
tar:
	n=`basename \`pwd\``; cd ..; tar cvf - $$n | gzip > $$n.tar.gz

suid: gpm gpm-root
	chown root $^; chmod +s $^

mpage:
	$(MAKE) -C doc mpage

asciidoc:
	 $(MAKE) -C doc $(TARGET)doc.txt

sample-dist:
	$(MAKE) -C sample -f Makefile.in distrib

test: $(TARGET)
	csh -c "(time ./$(TARGET) -m ./data -T ) |& tee /dev/tty >> Test.log"

depend dep:
	$(CC) -MM *.c > .depend

DEPEND DEP:
	$(CC) -M *.c > .depend

bin: all
	-mkdir bin bin/bin bin/include bin/lib bin/info
	$(MAKE) install PREFIX=./bin

sample:
	$(MAKE) -C sample

# don't release binaries, since my compiler has become old.
distrib: mpage sample-dist clean info asciidoc tar printv


ifeq (.depend,$(wildcard .depend))
include .depend
endif






