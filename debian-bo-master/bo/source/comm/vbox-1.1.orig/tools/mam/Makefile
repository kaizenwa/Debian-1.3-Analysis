INSTALL_AD  = install -c -o root -g root -m 444

INSTALL_MAM = install -c -o root -g root -m 755

LDEFINES    = -DSPOOLDIR=\"$(SPOOLDIR)\" -I. -I/usr/X11R6/include 

LIBEXT      = -L/usr/X11R6/lib -lXpm -lXmt -lXm -lXt -lX11 \
              -Wl,-rpath-link,/usr/X11R6/lib

LIBSTAT     = -L/usr/X11R6/lib -lXpm \
              /usr/X11/lib/libXmt.a /usr/X11/lib/libXm.a \
              -lXt -lX11 -lXext -Wl,-rpath-link,/usr/X11R6/lib
 
DEBUGFLAGS  = # -g

MAMOBJ      = mam.o ../../src/vboxrc.o ../../src/log.o

executables: mam 

mam.ad.h: mam.ad
	ad2c mam.ad > mam.ad.h

mam.o: mam.c mam.ad.h mam.h
	$(CC) $(CFLAGS) $(DEBUGFLAGS) $(LDEFINES) -o $@ -c $<

mam: $(MAMOBJ) 
	$(CC) -o $@ $(MAMOBJ) $(LIBEXT)

static: $(MAMOBJ) 
	$(CC) -o $@ $(MAMOBJ) $(LIBSTAT)
	strip ./mam.static

install: mam
	$(INSTALL_AD)  ./mam.ad /usr/X11R6/lib/X11/app-defaults/MAnswerMashine
	$(INSTALL_MAM) ./mam    /usr/X11R6/bin

clean:
	@rm -f core  *~ .*~ .gdb* *.ad.h

clear:
	@rm -f core  *.o .gdb* *.ad.h mam mam.static

