# Copyright 1996 University Corporation for Atmospheric Research
#
# OS/2 Makefile for common makefile macros used by the netCDF package.


# Installation directories:
#
# Change as necessary for your system.

# Root of the installation:
DESTDIR	= \netcdf24

# To contain the netCDF programs (ncgen, ncdump):
BINDIR	= $(DESTDIR)\bin

# To contain the netCDF and XDR libraries (libnc.a, libxdr.a):
LIBDIR	= $(DESTDIR)\lib

# To contain the netCDF header files (netcdf.h, netcdf.inc):
INCDIR	= $(DESTDIR)\include


# FORTRAN aplication program interface:
#
# If you do not have AT&T's FORTRAN-to-C translation utility, f2c, then 
# comment-out all of the following:
#
FORTRAN_ALL	= fortran/all
FORTRAN_TEST	= fortran/test
FORTRAN_INSTALL	= fortran/install
FORTRAN_CLEAN	= fortran/clean
FTEST		= ftest


# Filename of the C++ header-file <strstream.h>:
# This will need to be set to strstream.h if the EMX filenames have been
# expanded because they reside on a high-performance disk, which allows
# longer filename.
STRSTREAM_H_SPEC	= <strstrea.h>


# Dummy macros.  Overridden by individual makefiles as necessary.
# They exist only as placeholders for the "rules" makefile.
#
HEADER		= dummy.h
LIBRARY		= libdummy.a
