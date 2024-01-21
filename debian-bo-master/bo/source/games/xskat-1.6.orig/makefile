
#   xskat - a card game for 1 to 3 players.
#   Copyright (C) 1996  Gunter Gerhardt
#
#   This program is free software; you can redistribute it freely.
#   Use it at your own risk; there is NO WARRANTY.

CFLAGS	=
LDFLAGS	= -L/usr/X11R6/lib -lX11
DEFL    = -DDEFAULT_LANGUAGE=\"german\"
#DEFL   = -DDEFAULT_LANGUAGE=\"english\"

SHELL   = /bin/sh
OBJECTS	= skat.o bitmaps.o xio.o xdial.o text.o

xskat: $(OBJECTS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

clean:
	rm -f $(OBJECTS) xskat bitmaps.h

bitmaps.h: bwcards.xbm colcards.xbm icon.xbm
	@echo Generating $@;\
	(echo "/* Generated file. Don't edit. */";\
	echo "extern unsigned char colcards_pixs[];";\
	for bitmap in bwcards colcards icon;\
	do\
		egrep width"|"height < $$bitmap.xbm;\
		echo "extern unsigned char $${bitmap}_bits[];";\
	done) > $@

text.o: text.c text.h
	$(CC) $(CFLAGS) $(DEFL) -c $< -o $@

skat.o: skat.h text.h
xio.o: xio.h skat.h bitmaps.h text.h
xdial.o: xdial.h xio.h skat.h text.h
bitmaps.o: bwcards.xbm colcards.xbm icon.xbm
