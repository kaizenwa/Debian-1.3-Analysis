#
# Makefile for m68k specific functions.
#

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I$(TOPDIR)/string -I$(TOPDIR) -I.  -I../generic

GENERIC_DIR=../generic
IEEE754_DIR=../ieee754
IEEE854_DIR=../ieee854

LN=ln -s

DIRS:= setjmp

SRC1S= hton.c modf.c bzero.c ffs.c memchr.c memcmp.c memset.c \
	strcat.c strchr.c strcmp.c strcspn.c strlen.c strncmp.c \
	strpbrk.c strrchr.c strspn.c __stpncpy.c stpcpy.c strstr.c \
	__isinf.c __isinfl.c __isnan.c __isnanl.c add_n.c addmul_1.c \
	cmp.c dbl2mpn.c divmod.c divmod_1.c ldbl2mpn.c lshift.c \
	mpn2dbl.c mpn2flt.c mpn2ldbl.c mul.c mul_1.c mul_n.c \
	rshift.c sub_n.c submul_1.c frexp.c
SRC2S= ntoh.c div.c

# The is used by clean.
LINKS= __stpncpy.c stpcpy.c strstr.c \
       __isinf.c __isnan.c dbl2mpn.c mpn2dbl.c __isinfl.c \
	__isnanl.c ldbl2mpn.c mpn2ldbl.c add_n.c addmul_1.c mul_n.c \
	cmp.c mul.c mul_1.c divmod.c divmod_1.c sub_n.c submul_1.c \
	lshift.c rshift.c

SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SCR2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets

__stpncpy.c: $(GENERIC_DIR)/__stpncpy.c
	$(LN) $? $@

stpcpy.c: $(GENERIC_DIR)/stpcpy.c
	$(LN) $? $@

strstr.c: $(GENERIC_DIR)/strstr.c
	$(LN) $? $@

__isinf.c: $(IEEE754_DIR)/__isinf.c
	$(LN) $? $@

__isnan.c: $(IEEE754_DIR)/__isnan.c
	$(LN) $? $@

dbl2mpn.c: $(IEEE754_DIR)/dbl2mpn.c
	$(LN) $? $@

mpn2dbl.c: $(IEEE754_DIR)/mpn2dbl.c
	$(LN) $? $@

__isinfl.c: $(IEEE854_DIR)/__isinfl.c
	$(LN) $? $@

__isnanl.c: $(IEEE854_DIR)/__isnanl.c
	$(LN) $? $@

ldbl2mpn.c: $(IEEE854_DIR)/ldbl2mpn.c
	$(LN) $? $@

mpn2ldbl.c: $(IEEE854_DIR)/mpn2ldbl.c
	$(LN) $? $@

add_n.c: $(GENERIC_DIR)/add_n.c
	$(LN) $? $@

addmul_1.c: $(GENERIC_DIR)/addmul_1.c
	$(LN) $? $@

cmp.c: $(GENERIC_DIR)/cmp.c
	$(LN) $? $@

divmod.c: $(GENERIC_DIR)/divmod.c
	$(LN) $? $@

divmod_1.c: $(GENERIC_DIR)/divmod_1.c
	$(LN) $? $@

mul.c: $(GENERIC_DIR)/mul.c
	$(LN) $? $@

mul_1.c: $(GENERIC_DIR)/mul_1.c
	$(LN) $? $@

mul_n.c: $(GENERIC_DIR)/mul_n.c
	$(LN) $? $@

sub_n.c: $(GENERIC_DIR)/sub_n.c
	$(LN) $? $@

submul_1.c: $(GENERIC_DIR)/submul_1.c
	$(LN) $? $@

lshift.c: $(GENERIC_DIR)/lshift.c
	$(LN) $? $@

rshift.c: $(GENERIC_DIR)/rshift.c
	$(LN) $? $@

clean::
	$(RM) -f $(LINKS)
