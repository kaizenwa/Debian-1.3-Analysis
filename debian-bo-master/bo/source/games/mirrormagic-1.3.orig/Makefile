#
# Makefile fuer "Mirror Magic II -- McDuffins Revenge"
#

PROGNAME = mirrormagic

RM = rm -f
CC = gcc
# CC = cc				# for HP-UX and others

GFX_PATH = -DGFX_PATH=\"./graphics\"	# path where the graphics are
SND_PATH = -DSND_PATH=\"./sounds\"	# path where the sounds are
LEVEL_PATH = -DLEVEL_PATH=\".\"		# path where the level file is
SCORE_PATH = -DSCORE_PATH=\".\"		# path where the score file is
NAMES_PATH = -DNAMES_PATH=\".\"		# path where the names file is
# SOUNDS = -DNO_SOUNDS			# don't play any sounds
# SCORE_ENTRIES = -DONE_PER_NAME	# only one score entry per name
SCORE_ENTRIES = -DMANY_PER_NAME		# many score entries per name

# the XPM-Library is needed to build this program:
XPM_INCLUDE_FILE = -DXPM_INCLUDE_FILE="<X11/xpm.h>"

CONFIG = $(LEVEL_PATH) $(SCORE_PATH) $(NAMES_PATH)		\
	 $(GFX_PATH) $(SND_PATH) $(SOUNDS) $(SCORE_ENTRIES)	\
	 $(XPM_INCLUDE_FILE)

# DEBUG = -DDEBUG -g -ansi -pedantic -Wall
# DEBUG = -DDEBUG -g -Wall
DEBUG = -O6

# SYSTEM = -Aa -D_HPUX_SOURCE -Dhpux	# for HP-UX (obsolete)
# SYSTEM = -DSYSV -Ae			# for HP-UX
# SYSTEM = -DSYSV			# for systems without 'usleep()'
# INCL = -I/usr/include/X11R5		# for HP-UX and others
# LIBS = -lXpm -lX11 -lm
# LIBS = -L/usr/lib/X11R5 -lXpm -lX11 -lm # for HP-UX and others
LIBS = -lXpm -lXpm -lXpm -lX11 -lm	# triple "-lXpm" because of Xpm error

# CFLAGS = -O2 $(CONFIG) $(SYSTEM)
CFLAGS = $(DEBUG) $(CONFIG) $(SYSTEM) $(INCL)

OBJS =	main.o \
	init.o \
	images.o \
	events.o \
	tools.o \
	screens.o \
	misc.o \
	game.o \
	editor.o \
	sound.o

all:	$(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LIBS) -o $(PROGNAME)

.c.o:
	$(CC) $(CFLAGS) -c $*.c

clean:
	$(RM) $(OBJS)
