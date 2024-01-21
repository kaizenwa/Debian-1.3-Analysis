#
# @(#) Makefile for XTrojka
#

#############################################################################
#
#	GENERAL
#
PROGS= xtrojka
#					xtrojka version (do not change!)
VERSION=123
#					where to put the binary
TARGET_DIR=/usr/games
#					where to put the manpage
MANDIR=/usr/local/man/man6
#					the location of the highscorefile
HSFILE=/fill_in_your_scorefile
#					your favourite packer
COMPRESS=gzip -v9
#					your favourite c-compiler
CC=gcc
#					used X libraries
XLIBS= -lXaw -lXmu -lXt -lX11 -lXext
#					some more X libraries
RESGEN=./resgen

##############################################################################
#
#	FILE SETS
#

SRCS=	xtrojka.c tr_core.c game.c menu_tool.c window.c pics.c screen.c \
	sh_main.c sh_slist.c sh_stat.c \
	menu.c scores.c slist.c preferences.c options.c actions.c

OBJS=$(SRCS:.c=.o)

OTHER=Makefile XTrojka.* *.h bitmaps pixmaps AUTHOR COPYRIGHT MANIFEST README \
	resgen xtrojka.man YIKES xtrojka$(VERSION).lsm 

###########################################################################
#
#	SYSTEM OPTIONS
#

HSF=-DSCOREFILE='"$(HSFILE)"'
#
#	comment out your favorite system (the one you use, that is...)
#
#					search the libaries here
LIBPATH=-L/usr/X11/lib
#					doesn't work too well on all systems
#LCK=-DLOCKING
#					add debug info
#DBG=-DDEBUG_INFO
#					use slick graphics (multi-color-xpm)
XPM=-DXPM
XPMLIB=-lXpm

#	Comment out the options for the system you use

#
# Sunos 4.1.3
#
#XDIR=-I/usr/openwin/include
#LIBS=-lm
#SYS=-DSUNOS4
#
# Solaris 
#
#XDIR=-I/usr/openwin/include
#LIBS=-lnsl
#SYS=-DSUNOS5

#
# Linux
#
SYS=-DLINUX

#
#
#
CFLAGS= $(XDIR) $(DBG) $(XPM) $(SYS) $(OPT) $(HSF) $(LCK) $(LIBPATH) 

###########################################################################
#
#	COMMANDS
#

all: 	$(PROGS)

xtrojka: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LIBS) $(XPMLIB) $(XLIBS) -o xtrojka

tag:
	ctags $(SRCS)

tar:
	rm -rf xtrojka$(VERSION);	\
	mkdir xtrojka$(VERSION);	\
	cp -r $(SRCS) $(OTHER) xtrojka$(VERSION);	\
	rm -f xtrojka$(VERSION)/_*.h;		\
	tar cvf xtrojka$(VERSION).tar xtrojka$(VERSION);	\
	$(COMPRESS) xtrojka$(VERSION).tar;	\
	rm -rf xtrojka$(VERSION);

install:
	@echo $(HSFILE)|sed -e 's/\//\\\//g'>/tmp/hsn; \
	sed -e "s/__SCOREDIR__/`cat /tmp/hsn`/g" xtrojka.man > xtrojka.6; \
	cp xtrojka.6 $(MANDIR); chmod go+r xtrojka.6; \
	cp xtrojka $(TARGET_DIR);chmod go+rx xtrojka; \
	touch $(HSFILE); chmod go+rwx $(HSFILE);
	
clean:
	rm -rf tags *.o core xtrojka xtrojka.6 
	
##############################################################################
#
#	DEPENDENCIES
#

_strdefs.h _resdefs.h:	XTrojka 
	$(RESGEN)

actions.o: actions.c actions.h xtrojka.h debug.h
game.o: game.c game.h xtrojka.h sh_main.h sh_stat.h scores.h menu.h menu_tool.h \
  debug.h tr_core.h screen.h
menu.o: menu.c debug.h xtrojka.h tr_core.h menu.h menu_tool.h game.h sh_main.h
menu_tool.o: menu_tool.c xtrojka.h tr_core.h menu_tool.h menu.h debug.h
options.o: options.c xtrojka.h debug.h
pics.o: pics.c xtrojka.h tr_core.h pics.h window.h debug.h pictures.h \
  bitmaps/block0 bitmaps/block1 bitmaps/block2 bitmaps/block3 bitmaps/block4 \
  bitmaps/block5 bitmaps/pat1 bitmaps/pat2 bitmaps/pat3 bitmaps/pat4 \
  bitmaps/pat5 bitmaps/leftpillar bitmaps/rightpillar bitmaps/new \
  bitmaps/title bitmaps/app_icon bitmaps/stat_icon pixmaps/block1 \
  pixmaps/block2 pixmaps/block3 pixmaps/block4 pixmaps/block5 \
  pixmaps/leftpillar pixmaps/rightpillar
preferences.o: preferences.c debug.h xtrojka.h tr_core.h preferences.h 
scores.o: scores.c debug.h xtrojka.h tr_core.h scores.h slist.h sh_slist.h \
  _strdefs.h
screen.o: screen.c screen.h debug.h xtrojka.h tr_core.h
sh_main.o: sh_main.c debug.h xtrojka.h tr_core.h screen.h sh_slist.h sh_main.h \
  window.h pictures.h bitmaps/block0 bitmaps/block1 bitmaps/block2 \
  bitmaps/block3 bitmaps/block4 bitmaps/block5 bitmaps/pat1 bitmaps/pat2 \
  bitmaps/pat3 bitmaps/pat4 bitmaps/pat5 bitmaps/leftpillar \
  bitmaps/rightpillar bitmaps/new bitmaps/title bitmaps/app_icon \
  bitmaps/stat_icon pixmaps/block1 pixmaps/block2 pixmaps/block3 \
  pixmaps/block4 pixmaps/block5 pixmaps/leftpillar pixmaps/rightpillar menu.h \
  menu_tool.h
sh_slist.o: sh_slist.c sh_slist.h xtrojka.h screen.h window.h font.h scores.h \
  pictures.h bitmaps/block0 bitmaps/block1 bitmaps/block2 bitmaps/block3 \
  bitmaps/block4 bitmaps/block5 bitmaps/pat1 bitmaps/pat2 bitmaps/pat3 \
  bitmaps/pat4 bitmaps/pat5 bitmaps/leftpillar bitmaps/rightpillar bitmaps/new \
  bitmaps/title bitmaps/app_icon bitmaps/stat_icon pixmaps/block1 \
  pixmaps/block2 pixmaps/block3 pixmaps/block4 pixmaps/block5 \
  pixmaps/leftpillar pixmaps/rightpillar slist.h debug.h tr_core.h _strdefs.h
sh_stat.o: sh_stat.c sh_stat.h debug.h xtrojka.h tr_core.h screen.h window.h \
  pictures.h bitmaps/block0 bitmaps/block1 bitmaps/block2 bitmaps/block3 \
  bitmaps/block4 bitmaps/block5 bitmaps/pat1 bitmaps/pat2 bitmaps/pat3 \
  bitmaps/pat4 bitmaps/pat5 bitmaps/leftpillar bitmaps/rightpillar bitmaps/new \
  bitmaps/title bitmaps/app_icon bitmaps/stat_icon pixmaps/block1 \
  pixmaps/block2 pixmaps/block3 pixmaps/block4 pixmaps/block5 \
  pixmaps/leftpillar pixmaps/rightpillar _strdefs.h
slist.o: slist.c debug.h xtrojka.h slist.h scores.h sh_slist.h
tr_core.o: tr_core.c tr_core.h
window.o: window.c window.h tr_core.h sh_slist.h sh_stat.h debug.h xtrojka.h
xtrojka.o: xtrojka.c xtrojka.h scores.h options.h preferences.h sh_slist.h \
  sh_stat.h game.h menu_tool.h menu.h window.h pics.h slist.h actions.h debug.h \
  tr_core.h _strdefs.h _resdefs.h

