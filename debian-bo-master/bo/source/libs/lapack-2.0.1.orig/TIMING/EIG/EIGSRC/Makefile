include ../../../make.inc

#######################################################################
#  This is the makefile to create a library of the eigensystem routines
#  from LAPACK that have been instrumented to count operations. 
#  The files are organized as follows:
#
#     SCIGSRC -- Single precision instrumented LAPACK routines used in
#                REAL and COMPLEX
#     DZIGSRC -- Double precision instrumented LAPACK routines used in
#                DOUBLE PRECISION and COMPLEX*16
#
#     SEIGSRC -- Single precision real instrumented LAPACK routines
#     CEIGSRC -- Single precision complex instrumented LAPACK routines
#     DEIGSRC -- Double precision real instrumented LAPACK routines
#     ZEIGSRC -- Double precision complex instrumented LAPACK routines
#
#  The library can be set up to include routines for any combination
#  of the four precisions.  First, modify the FORTRAN and OPTS 
#  definitions to match your compiler and the options to be used.
#  Then to create or add to the library, enter make followed by one or
#  more of the precisions desired.  Some examples:
#       make single
#       make single complex
#       make single double complex complex16
#  Alternatively, the command
#       make
#  without any arguments creates a library of all four precisions.
#  The library is called
#       eigsrc.a
#  and is created at the next higher directory level.
#
#  To remove the object files after the library is created, enter
#       make clean
#  On some systems, you can force the source files to be recompiled by
#  entering (for example)
#       make single FRC=FRC
#
#----------------------------------------------------------------------
#
#  Edward Anderson, University of Tennessee
#  March 26, 1990.
#  Susan Ostrouchov, Last updated September 30, 1994
#
#######################################################################

SCIGSRC = sbdsqr.o slaebz.o slaed0.o slaed1.o slaed2.o slaed3.o \
          slaed4.o slaed5.o slaed6.o slaed7.o slaed8.o \
          slaed9.o slaeda.o slasq1.o slasq2.o slasq3.o slasq4.o \
          sstebz.o sstedc.o ssteqr.o ssterf.o

DZIGSRC = dbdsqr.o dlaebz.o dlaed0.o dlaed1.o dlaed2.o dlaed3.o \
          dlaed4.o dlaed5.o dlaed6.o dlaed7.o dlaed8.o \
          dlaed9.o dlaeda.o dlasq1.o dlasq2.o dlasq3.o dlasq4.o \
          dstebz.o dstedc.o dsteqr.o dsterf.o
 
SEIGSRC = \
   sgghrd.o shgeqz.o shsein.o shseqr.o \
   slaein.o slahqr.o spteqr.o sstein.o stgevc.o strevc.o

CEIGSRC = \
   cbdsqr.o cgghrd.o chgeqz.o chsein.o chseqr.o \
   claed0.o claed7.o claed8.o claein.o clahqr.o \
   cpteqr.o cstedc.o cstein.o csteqr.o ctgevc.o ctrevc.o

DEIGSRC = \
   dgghrd.o dhgeqz.o dhsein.o dhseqr.o \
   dlaein.o dlahqr.o dpteqr.o dstein.o dtgevc.o dtrevc.o

ZEIGSRC = \
   zbdsqr.o zgghrd.o zhgeqz.o zhsein.o zhseqr.o \
   zlaed0.o zlaed7.o zlaed8.o zlaein.o zlahqr.o \
   zpteqr.o zstedc.o zstein.o zsteqr.o ztgevc.o ztrevc.o

all: single complex double complex16

single: $(SEIGSRC) $(SCIGSRC)
	$(ARCH) $(ARCHFLAGS) ../$(EIGSRCLIB) $(SEIGSRC) $(SCIGSRC)
	$(RANLIB) ../$(EIGSRCLIB)

complex: $(CEIGSRC) $(SCIGSRC)
	$(ARCH) $(ARCHFLAGS) ../$(EIGSRCLIB) $(CEIGSRC) $(SCIGSRC)
	$(RANLIB) ../$(EIGSRCLIB)

double: $(DEIGSRC) $(DZIGSRC)
	$(ARCH) $(ARCHFLAGS) ../$(EIGSRCLIB) $(DEIGSRC) $(DZIGSRC)
	$(RANLIB) ../$(EIGSRCLIB)

complex16: $(ZEIGSRC) $(DZIGSRC)
	$(ARCH) $(ARCHFLAGS) ../$(EIGSRCLIB) $(ZEIGSRC) $(DZIGSRC)
	$(RANLIB) ../$(EIGSRCLIB)

$(SCIGSRC): $(FRC)
$(DZIGSRC): $(FRC)
$(SEIGSRC): $(FRC)
$(CEIGSRC): $(FRC)
$(DEIGSRC): $(FRC)
$(ZEIGSRC): $(FRC)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o

.f.o: 
	$(FORTRAN) -c $(OPTS) $<
