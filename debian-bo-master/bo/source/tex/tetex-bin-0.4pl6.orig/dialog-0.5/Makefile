INSTALL = install -c
prefix	= /usr/local/teTeX
platform= $(shell ../config.guess | sed 's/-.*-/-/')

bindir = $(prefix)/bin/$(platform)
MANDIR = $(prefix)/man
MANEXT = 1

NCURSESDIR=ncurses-1.9.9e
CC = gcc
CPP = $(CC) -E


CFLAGS = -O2
LDFLAGS =
DEFS = -DUSE_COLORS $(CPPFLAGS)

# set USE_NCURSES=false if you want to use your system curses library instead
# of teTeX's ncurses
USE_NCURSES=true

# set HAVE_NCURSES=true if you want to use an already installed ncurses library
HAVE_NCURSES=false

ifeq ($(USE_NCURSES),true)
ifeq ($(HAVE_NCURSES),true)
CPPFLAGS=
LIBS=-lncurses
else
CPPFLAGS=-I$(NCURSESDIR)/include
LIBS=$(NCURSESDIR)/lib/libncurses.a
endif
DEFS += -DHAVE_NCURSES
else
CPPFLAGS=
LIBS=-lcurses
endif

HDRS = dialog.h
SRCS = dialog.c checklist.c inputbox.c menubox.c msgbox.c textbox.c yesno.c radiolist.c guage.c rc.c
OBJS = dialog.o checklist.o inputbox.o menubox.o msgbox.o textbox.o yesno.o radiolist.o guage.o rc.o

all: dialog

config:
ifeq ($(USE_NCURSES),true)
ifneq ($(HAVE_NCURSES),true)
	rm -f $(NCURSESDIR)/config.cache
	(cd $(NCURSESDIR); CC='$(CC)' CFLAGS='$(CFLAGS)' ./configure $(CONFFLAGS))
ifeq (nextstep,$(findstring nextstep, $(platform)))
	(echo "/define HAVE_TERMIOS_H/d"; echo w; echo q) | ed $(NCURSESDIR)/include/config.h || true
	(echo "/define HAVE_REGEX_H/d"; echo w; echo q) | ed $(NCURSESDIR)/include/config.h || true
	(echo 's/^TYPE_OF_BOOL.*/TYPE_OF_BOOL = unsigned/'; echo w; echo q) | ed $(NCURSESDIR)/include/Makefile || true
endif
endif
endif

.c.o:
	$(CC) -c $(CFLAGS) $(DEFS) -o $@ $<

ifeq ($(USE_NCURSES),true)
ifneq ($(HAVE_NCURSES),true)
$(OBJS): $(LIBS)
dialog: $(LIBS)
endif
endif

dialog: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

install: dialog install.man
	$(INSTALL) -m 755 dialog $(bindir)

install.man:

$(NCURSESDIR)/lib/libncurses.a:
	(cd $(NCURSESDIR); $(MAKE) CC='$(CC)' CFLAGS='$(CFLAGS)' INSTALL='$(INSTALL)' LDFLAGS='$(LDFLAGS)')

clean:
	rm -f core *.o dialog
ifeq ($(USE_NCURSES),true)
ifneq ($(HAVE_NCURSES),true)
	(cd $(NCURSESDIR);  $(MAKE) $@)
endif
endif

distclean: cleanall
ifeq ($(USE_NCURSES),true)
ifneq ($(HAVE_NCURSES),true)
	(cd $(NCURSESDIR);  $(MAKE) $@)
endif
endif

cleanall:
	rm -f core *.o dialog

cleanobj:
	rm -f core *.o
