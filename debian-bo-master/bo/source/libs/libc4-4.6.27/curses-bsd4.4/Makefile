#
# This is Makefile of curses for Linux
#

override DEBUG=false
override PROFILE=false
#override CHECKER=false

override SHLIB=libcurses
JUMP_LIB=libcurses

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

override STATIC_LIB=$(STATIC_DIR)/libcurses.a
override SHARED_LIB=$(SHARED_DIR)/libcurses.a
override CHECKER_LIB=$(CHECKER_DIR)/libcurses.a

INC_CFLAGS= -I.
CURSES_CFLAGS=-DPOSIX -D__USE_BSD_SIGNAL -D_CURSES_PRIVATE
BASE_CFLAGS := $(BASE_CFLAGS) $(CURSES_CFLAGS)

DIRS:=

# The old one.
SRCS=   addbytes.c addch.c addstr.c box.c clear.c clrtobot.c \
	clrtoeol.c cr_put.c cr_tty.c delch.c deleteln.c \
	delwin.c endwin.c erase.c fullname.c getch.c getstr.c \
	idlok.c id_subwins.c initscr.c insch.c insertln.c \
	longname.c move.c mvprintw.c mvscanw.c mvwin.c \
	newwin.c overlay.c overwrite.c printw.c putchar.c \
	refresh.c scanw.c scroll.c toucholap.c standout.c \
	touchwin.c tstp.c curses.c unctrl.c

SRCS=	addbytes.c addch.c addnstr.c box.c clear.c clrtobot.c \
	clrtoeol.c cr_put.c ctrace.c cur_hash.c curses.c delch.c \
	deleteln.c delwin.c erase.c fullname.c getch.c getstr.c \
	id_subwins.c idlok.c initscr.c insch.c insertln.c longname.c \
	move.c mvwin.c newwin.c overlay.c overwrite.c printw.c \
	putchar.c refresh.c scanw.c scroll.c setterm.c standout.c \
	toucholap.c touchwin.c tscroll.c tstp.c tty.c unctrl.c
#	compat.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets

#CFLAGS+=#-DTFILE=\"/dev/ttyp0\"
#CFLAGS+=-D_CURSES_PRIVATE -I${.CURDIR}
