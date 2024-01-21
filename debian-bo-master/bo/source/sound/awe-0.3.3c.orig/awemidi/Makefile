#================================================================
# Makefile for drvmidi with Tcl/Tk interface
# written by Takashi Iwai
#================================================================

.SUFFIXES: .c .h .ptcl .tcl .o

# Target program name
TARGET = drvmidi

# Search path of awe_voice.h.  Comment the lines if not necessary.
# FreeBSD with VoxWare 3.0
#INCLUDES += -I/usr/src/sys/i386/isa/sound

# installation directory
INSTALL_DIR = /usr/local/bin

# compile flags
DEBUG_MODE = -O -Wall

# main objects
MOBJS= midi.o readmidi.o event.o seq.o malloc.o cmpopen.o awe_effect.o\
	signal.o

# default control interface objects
CTRLS = dumb_c.o pipe_c.o

#================================================================
# Tcl/Tk interface:
#    if you don't want to use this, comment out following lines.
#================================================================

# installation directory
TCLDIR = /usr/local/lib/awe

CTRLS += tk_c.o
TCLF = tkawe.tcl tkpanel.tcl browser.tcl misc.tcl

TKPROGPATH = $(TCLDIR)/tkawe.tcl
EXTRAFLAGS += -DINCLUDE_TK_MODE -DTKPROGPATH=\"$(TKPROGPATH)\"

# Tcl/Tk library
EXTRALIBS += -ltk -ltcl
# X11 library; modify for you site.
EXTRALIBS += -L/usr/X11/lib -lX11
# for Linux user
EXTRALIBS += -ldl

#================================================================
# ncurses interface:
#    if you don't want to use this, comment out following lines.
#================================================================

CTRLS += ncurs_c.o insts.o
EXTRAFLAGS += -DINCLUDE_NCURSES_MODE
EXTRALIBS += -lncurses


#================================================================

OBJS = $(MOBJS) $(CTRLS)
CFLAGS = $(DEBUG_MODE) $(INCLUDES) $(EXTRAFLAGS)

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(EXTRALIBS) -lm

tclIndex: $(TCLF)
	echo 'auto_mkindex . *.tcl; exit' | wish

miditext: miditext.o readmidi.o cmpopen.o malloc.o awe_effect.o
	$(CC) $(CFLAGS) -o $@ miditext.o readmidi.o cmpopen.o malloc.o awe_effect.o -lm

txtplay: txtplay.o event.o seq.o malloc.o awe_effect.o dumb_c.o
	$(CC) $(CFLAGS) -o $@  txtplay.o event.o seq.o malloc.o awe_effect.o dumb_c.o -lm

install: $(TARGET) $(TCLF) tclIndex
	install -c -s $(TARGET) $(INSTALL_DIR)
	mkdir -p $(TCLDIR)/BITMAPS
	install -c -m 644 $(TCLF) tclIndex $(TCLDIR)
	install -c -m 644 BITMAPS/* $(TCLDIR)/BITMAPS

.ptcl.tcl:
	sed -e s@%TCLDIR%@$(TCLDIR)@g $< > $@

clean:
	$(RM) *.o tkawe.tcl tclIndex drvmidi miditext txtplay
