#
# This is Makefile of the math lib for Linux
#

MATH=true

override DEBUG=false
override PROFILE=false

TOPDIR=../../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS = -I.
MATHFLAGS=-ffast-math -mieee-fp

BASE_CFLAGS := $(BASE_CFLAGS) $(MATHFLAGS)

SRC1S= __copysign.c __infnan.c __isinf.c __isnan.c __rint.c \
	acos.c acosh.c asin.c atanh.c ceil.c erf.c floor.c \
	j0.c j1.c jn.c lgamma.c log.c log10.c log1p.c pow.c sqrt.c \
	cbrt.c __infnanl.c __polevll.c acoshl.c acosl.c asinl.c \
	atanhl.c cbrtl.c ceill.c erfl.c floorl.c fpclassifyf.c \
	frexpl.c j0l.c j1l.c jnl.c ldexpl.c lgammal.c \
	log10l.c log1pl.c log2l.c logl.c powl.c sqrtl.c modfl.c

ifeq ($(ELF),true)
SRC2S= __drem.S asinh.S atan.S atan2.S cos.S cosh.S exp.S expm1.S \
	fabs.S fmod.S hypot.S sin.S sinh.S tan.S tanh.S \
	asinhl.S atan2l.S atanl.S coshl.S cosl.S expl.S expm1l.S \
	fabsl.S fmodl.S hypotl.S sinhl.S sinl.S tanhl.S tanl.S
else
SRC2S= __drem.S asinh.S atan.S atan2.S cos.S cosh.S exp.S expm1.S \
	fabs.S fmod.S frexp.S hypot.S sin.S sinh.S tan.S tanh.S \
	asinhl.S atan2l.S atanl.S coshl.S cosl.S expl.S expm1l.S \
	fabsl.S fmodl.S hypotl.S sinhl.S sinl.S tanhl.S tanl.S
endif

DIRS:=
SRCS=$(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.S=.s)
OBJS= $(SRC1S:.c=.o) $(SRC2S:.S=.o)
ALIASES:=

include $(TOPDIR)/Maketargets
