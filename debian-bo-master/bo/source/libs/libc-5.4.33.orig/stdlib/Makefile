#
# This is Makefile of stdlib functions for Linux.
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I. -I../sysdeps/generic

DIRS:=
SRC1S= __random.c _quicksort.c abort.c atexit.c atof.c \
	atoi.c atol.c bsearch.c exit.c labs.c ldiv.c \
	mblen.c mbstowcs.c mbtowc.c on_exit.c qsort.c rand.c \
	strtol.c strtoul.c wcstombs.c wctomb.c \
	strtoq.c strtouq.c strtod.c strtof.c strtold.c fpioconst.c
SRC2S= abs.c random.c srand.c #strtof.c strtold.c
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)
 
include $(TOPDIR)/Maketargets  
