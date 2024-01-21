#
# This Makefile was originally done with ezmake 1.1
#
# Please edit the different variables to your liking.
#

.SUFFIXES: .c .o

CC	=gcc
RM	=rm
INSTALL	=install
CP	=cp
TAR	=tar
GZIP	=gzip

# Binary directory for make install
BINDIR	=/usr/local/bin/X11

# Library directories for Linux
XLIBDIR	=/usr/X11R6/lib

# Library directories for the Ultrix (probably most Unixes)
#XLIBDIR =/usr/lib/X11

# Comment out the following line if you do not have the XPM library.
XPM     =-DHAS_XPM
XPMLIB  =-lXpm

.c.o:
	$(CC) $(CFLAGS) $(XPM) -c $*.c

# Linux CFLAGS (actually generic gcc)
CFLAGS=-Wall -O2

# Want static binaries?
#CFLAGS=-Wall -O2 -static

# Want debugging?
#CFLAGS=-Wall -pedantic -O0 -g

# Ultrix cflags
#CFLAGS=-O

# Most Linux users won't be troubled by this. Actually almost nobody will
# be troubled by this. It just might save me a couple of emails...
# Just change it to wherever your libX11.a resides.
LDFLAGS=-L$(XLIBDIR)

all: xbanner freetemp random_effect xb_check 

exe: xbanner freetemp random_effect xb_check 


xbanner: xbanner.o effect.o xres.o backg.o pix.o colors.o linger.o util.o \
	plasma.o ripples.o 
	$(CC) $(CFLAGS) $(LDFLAGS) -o xbanner xbanner.o colors.o \
	plasma.o util.o linger.o pix.o effect.o xres.o backg.o \
	ripples.o $(XPMLIB) -lX11 -lm 

freetemp: freetemp.o util.o 
	$(CC) $(CFLAGS) $(LDFLAGS) -o freetemp freetemp.o util.o -lX11 

random_effect: random_effect.o 
	$(CC) $(CFLAGS) -o random_effect random_effect.o 

xb_check: xb_check.o 
	$(CC) $(CFLAGS) -o xb_check xb_check.o 

install: freetemp xbanner xb_check 
	$(INSTALL) --strip --mode "0755" --owner root --group bin freetemp $(BINDIR)/freetemp
	$(INSTALL) --strip --mode "0755" --owner root --group bin xbanner $(BINDIR)/xbanner
	$(INSTALL) --strip --mode "0755" --owner root --group bin xb_check $(BINDIR)/xb_check

kit: realclean Logo.xpm Logo_32_col.xpm samples docs Makefile QuickStart \
	util.c XBanner1.3.lsm backg.c xres.c colors.c effect.c freetemp.c \
	random_effect.c pix.c xbanner.c linger.c xb_config.h xbanner.h \
	plasma.c ToDo VMS index.html 
	$(RM) -f XBanner1.3.tar
	$(RM) -f XBanner1.3.tar.gz
	$(RM) -rf XBanner1.3
	$(INSTALL) --directory XBanner1.3
	$(CP) -r samples XBanner1.3/.
	$(CP) -r docs XBanner1.3/.
	$(CP) -r VMS XBanner1.3/.
	$(CP) Logo.xpm XBanner1.3/.
	$(CP) Logo_32_col.xpm XBanner1.3/.
	$(CP) index.html XBanner1.3/.
	$(CP) Makefile XBanner1.3/.
	$(CP) QuickStart XBanner1.3/.
	$(CP) XBanner1.3.lsm XBanner1.3/.
	$(CP) xb_check.c XBanner1.3/.
	$(CP) xres.c XBanner1.3/.
	$(CP) util.c XBanner1.3/.
	$(CP) linger.c XBanner1.3/.
	$(CP) pix.c XBanner1.3/.
	$(CP) backg.c XBanner1.3/.
	$(CP) effect.c XBanner1.3/.
	$(CP) colors.c XBanner1.3/.
	$(CP) plasma.c XBanner1.3/.
	$(CP) ripples.c XBanner1.3/.
	$(CP) freetemp.c XBanner1.3/.
	$(CP) random_effect.c XBanner1.3/.
	$(CP) xbanner.c XBanner1.3/.
	$(CP) xb_config.h XBanner1.3/.
	$(CP) ToDo XBanner1.3/.
	$(CP) xbanner.h XBanner1.3/.
	$(TAR) -covf XBanner1.3.tar XBanner1.3
	$(GZIP) -9 XBanner1.3.tar

clean:
	$(RM) -f .depend *~ *# *.bak */*.bak build.log core
	$(RM) -f -r XBanner1.3
	$(RM) -f XBanner1.3.tar
	$(RM) -f XBanner1.3.tar.gz
	$(RM) -f *.o

realclean: clean 
	$(RM) -f xbanner
	$(RM) -f random_effect
	$(RM) -f freetemp
	$(RM) -f xb_check
	$(RM) -f */*~
	$(RM) -f */*/*~

# Please do not remove - dependencies

xbanner.o : xbanner.c xbanner.h
effect.o : effect.c xbanner.h xb_config.h
xres.o : xres.c xbanner.h xb_config.h
util.o : util.c xbanner.h xb_config.h
pix.o : pix.c xbanner.h xb_config.h
colors.o : colors.c xbanner.h xb_config.h
plasma.o : plasma.c xbanner.h xb_config.h
ripples.o : ripples.c xbanner.h xb_config.h
linger.o : linger.c xbanner.h xb_config.h
backg.o : backg.c xbanner.h xb_config.h
freetemp.o : freetemp.c xbanner.h xb_config.h
random_effect.o : random_effect.c xbanner.h xb_config.h
xb_check.o : xb_check.c xres.c xbanner.h xb_config.h
