#Log is a flag to turn logging of two players on/off (default if on)
#Have to include -lbsd on SysV
OBJS= xtet42.o
LIBS= -lX11 -lbsd
# This is for SGI
#LIBS= -lX11_s -lc_s #-lbsd
CC=gcc
#CC=cc
LIBDIR		= /usr/games/lib/xtet42
_Ver		= 2.02
_Finger		= /usr/bin/finger
_Lock		= $(LIBDIR)/.xtet42.lock
_Unlock		= $(LIBDIR)/.xtet42.unlock
_Log		= $(LIBDIR)/.xtet42.log
_Hiscore	= $(LIBDIR)/.xtet42.hiscore
_Hione		= $(LIBDIR)/.xtet42.hiscore.single

DEFS = 	-D_Lock=\"$(_Lock)\" 		-D_Unlock=\"$(_Unlock)\" \
	-D_Hiscore=\"$(_Hiscore)\" 	-D_Hione=\"$(_Hione)\" \
	-D_Log=\"$(_Log)\" -DLog -DFINGER=\"$(_Finger)\" \
	-D_Ver=\"$(_Ver)\" # -DGNUFINGER
BINDIR = /usr/games
MANDIR = /usr/man/man
MANEXT = 6
#The highscore file has to (should) be sgi, so that only one (few) can
#use it
GROUP  = games 
CFLAGS =  -O2 -s $(DEFS)
#CFLAGS = -g $(DEFS) -I/local/include


.c.o:
	$(CC) -c $*.c $(CFLAGS)

xtet42: $(OBJS)
	$(CC) -s -o xtet42 $(OBJS) $(LIBS) 

clean: 
	rm *.o xtet42 *~ core

install: xtet42
	-mkdir -p $(BINDIR)
	cp xtet42 $(BINDIR)
	chgrp $(GROUP) $(BINDIR)/xtet42
	chmod 2755 $(BINDIR)/xtet42
	-mkdir -p $(LIBDIR)
	chgrp $(GROUP) $(LIBDIR)
	chmod 775 $(LIBDIR)
	touch $(_Unlock)
	chgrp $(GROUP) $(_Unlock)
	chmod o-rwx $(_Unlock)
	chmod g+rw $(_Unlock)
	cp xtet42.hiscore $(_Hiscore)
	chgrp $(GROUP) $(_Hiscore)
	chmod o-rwx $(_Hiscore)
	chmod g+rw $(_Hiscore)
	cp xtet42.hiscore.single $(_Hione)
	chgrp $(GROUP) $(_Hione)
	chmod o-rwx $(_Hione)
	chmod g+rw $(_Hione)
	-mkdir $(MANDIR)$(MANEXT)
	cp xtet42.$(MANEXT) $(MANDIR)$(MANEXT)
