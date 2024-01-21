#! make -f
#

LIBS= -lintl
XLIBS= -L/usr/X11R6/lib -lXt -lXaw -lX11

APDF= Xdialdcost

PROG= dialdcost
XPRG= xdialdcost

XSRC= xdialdcost.c
XOBJ= xdialdcost.o
SRC= dialdcost.c
OBJ= dialdcost.o

all:	$(OBJ) $(XOBJ)
	$(CC) $(CFLAGS) $(OBJ) -o $(PROG) $(LIBS)
	$(CC) $(CFLAGS) $(XOBJ) -o $(XPRG) $(XLIBS) $(LIBS)
	( cd po; make )

install:
	cp $(PROG)    $(DESTDIR)/usr/bin
	cp $(XPRG)    $(DESTDIR)/usr/X11R6/bin
	cp $(PROG).1  $(DESTDIR)/usr/man/man1/$(PROG).1
	cp $(XPRG).1x $(DESTDIR)/usr/X11R6/man/man1/$(XPRG).1x
	cp $(APDF)    $(DESTDIR)/usr/X11R6/lib/X11/app-defaults
	( cd po; make DESTDIR=$(DESTDIR) install )

clean:
	rm -f $(PROG) $(XPRG) *.o  *.bak *~


