#
# This is Makefile of the math lib for Linux for the m68k
#

MATH=true

override DEBUG=false
override PROFILE=false

TOPDIR=../../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS = -I.
MATHFLAGS=-mieee-fp

BASE_CFLAGS := $(BASE_CFLAGS) $(MATHFLAGS)

SRC1S= __copysign.c __infnan.c __isinf.c __isnan.c __rint.c \
	acos.c acosh.c asin.c atanh.c ceil.c erf.c floor.c frexp.c \
	j0.c j1.c jn.c lgamma.c log.c log10.c log1p.c pow.c sqrt.c \
	__drem.c asinh.c atan.c atan2.c cos.c cosh.c exp.c \
	expm1.c fabs.c fmod.c hypot.c sin.c sinh.c tan.c tanh.c \
	cbrt.c
SRC2S=

DIRS:=
SRCS=$(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.S=.s)
OBJS= $(SRC1S:.c=.o) $(SRC2S:.S=.o)
ALIASES:=

include $(TOPDIR)/Maketargets
