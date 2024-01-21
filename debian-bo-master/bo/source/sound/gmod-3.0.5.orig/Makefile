# USE_LOCAL will cause gmod to use a copy of soundcard.h found in the
# current directory.  
# USE_NCURSES will compile gmod with support for special keys (arrows, etc).
# The ncurses library must be installed to compile with this defined.
# USE_X will compile gmod with and X interface.
# Note:  do not define both USE_NCURSES and USE_X at the same time.

# Use the next line for shared libqt
#LIBS = -lX11 -lqt
# Use the next line for static libqt
LIBS = -lX11 /usr/local/qt/lib/libqt.a

DEFINES = -DUSE_X
INCLUDES = -I/usr/local/qt/include
LIBDIRS = -L/usr/X11R6/lib -L/usr/local/qt/lib
CC = g++

CFLAGS = -O2 -m486 -fomit-frame-pointer -fno-strength-reduce
#CFLAGS = -Wall -g

# Where to put the binary and man page
BINDIR = /usr/local/bin
MANDIR = /usr/local/man/man1

CFILES = compress.c cvt_period.c Sequencer.c effects.c \
	gmod.c init.c load_669.c load_mod.c load_mtm.c \
	load_s3m.c load_ult.c load_xm.c misc.c OptShell.c parse.c \
	play_mod.c play_note.c play_voice.c proc_input.c \
	read_rc.c TopShell.c TrackShell.c QueueShell.c SampleShell.c \
	CommentShell.c Envelope.c Sample.c \
	Six69.c mod.c mtm.c s3m.c ult.c xm.c Voice.c Sequencer2.c

OFILES := $(patsubst %.c, %.o, $(CFILES))

MOCFILES = Sequencer.moc OptShell.moc TopShell.moc TrackShell.moc SampleShell.moc CommentShell.moc QueueShell.moc

xgmod: .depend $(MOCFILES) $(OFILES)
	$(CC) -o xgmod $(LIBDIRS) $(OFILES) $(LIBS)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) -c $<

%.moc: %.h
	/usr/local/qt/bin/moc $< -o $@

.depend:
	-$(CC) $(DEFINES) -MM $(INCLUDES) $(CFILES) > .depend 2>/dev/null

#clean added by Peter Federighi
.PHONY: clean
clean:
	rm -f $(OFILES) $(MOCFILES) xgmod core .depend

.PHONY: install
install: xgmod
	install -o bin -g bin -m 755 -s xgmod $(BINDIR)/xgmod
	install -o bin -g info -m 644 xgmod.1 $(MANDIR)/xgmod.1

ifeq (.depend,$(wildcard .depend))
include .depend
endif
