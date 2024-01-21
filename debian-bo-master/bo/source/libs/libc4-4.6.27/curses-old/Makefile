#
# This is Makefile of curses for Linux
#

#override CHECKER=false
override SHARED=true
override STATIC=false
override DEBUG=false
override PROFILE=false

JUMP_LIB=libcurses-old

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

override STATIC_LIB=$(STATIC_DIR)/libcurses-old.a
override SHARED_LIB=$(SHARED_DIR)/libcurses-old.a
override CHECKER_LIB=$(CHECKER_DIR)/libcurses-old.a

INC_CFLAGS= -I.
CURSES_CFLAGS=-DPOSIX -D__USE_BSD_SIGNAL
BASE_CFLAGS := $(BASE_CFLAGS) $(CURSES_CFLAGS)

DIRS:=

SRCS=	addbytes.c addch.c addstr.c box.c clear.c clrtobot.c \
	clrtoeol.c cr_put.c cr_tty.c delch.c deleteln.c \
	delwin.c endwin.c erase.c fullname.c getch.c getstr.c \
	idlok.c id_subwins.c initscr.c insch.c insertln.c \
	longname.c move.c mvprintw.c mvscanw.c mvwin.c \
	newwin.c overlay.c overwrite.c printw.c putchar.c \
	refresh.c scanw.c scroll.c toucholap.c standout.c \
	touchwin.c tstp.c curses.c unctrl.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
