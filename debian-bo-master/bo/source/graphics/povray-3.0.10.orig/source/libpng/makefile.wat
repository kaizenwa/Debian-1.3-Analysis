# Makefile for libpng
# Watcom 10a

# This version of the libpng makefile was adapted by Chris Young for use
# with Watcom 10a 32-bit protected mode flat memory model.  It was created 
# for use with POV-Ray ray tracer and you may choose to edit the CFLAGS to 
# suit your needs but the -DMSDOS is required.
# -- Chris Young 76702.1655@compuserve.com

# To use, do "wmake -f makefile.wat"

# See zconf.h for details about the memory requirements.

# ------------- Watcom 10a -------------
MODEL=-mf 
CFLAGS= $(MODEL) -fpi -fp5 -zp4 -5r -w5 -oneatx -DMSDOS -I..\zlib
CC=wcc386
LD=wcl386
LIB=wlib -b -c 
LDFLAGS= 
O=.obj

# variables
OBJS1 = png$(O) pngrcb$(O) pngrutil$(O) pngtrans$(O) pngwutil$(O) pngmem$(O)
OBJS2 = pngread$(O) pngpread$(O) pngerror$(O) pngwrite$(O) pngrtran$(O) pngwtran$(O) 
OBJS3 = pngrio$(O) pngwio$(O)
OBJSL1 = png$(O) +pngrcb$(O) +pngrutil$(O) +pngtrans$(O) +pngwutil$(O) +pngmem$(O)
OBJSL2 = pngread$(O) pngpread$(O) +pngerror$(O) +pngwrite$(O) +pngrtran$(O) +pngwtran$(O) 
OBJSL3 = pngrio$(O) +pngwio$(O)

all: libpng.lib pngtest.exe test

png$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngrcb$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngread$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngpread$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngrtran$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngrutil$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngerror$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngmem$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngwio$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngrio$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngtest$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngtrans$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngwrite$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngwtran$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

pngwutil$(O): png.h pngconf.h
        $(CC) $(CFLAGS) $*.c

libpng.lib: $(OBJS1) $(OBJS2) $(OBJS3)
        $(LIB) libpng +$(OBJSL1)
        $(LIB) libpng +$(OBJSL2)
        $(LIB) libpng +$(OBJSL3)

pngtest.exe: pngtest.obj libpng.lib 
        $(LD) $(LDFLAGS) pngtest.obj libpng.lib ..\zlib\zlib.lib

test: pngtest.exe
        .\pngtest>test

# End of makefile for libpng
