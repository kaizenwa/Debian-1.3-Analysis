##-conf Options for sun-sparc-solaris2
##-conf.simchecker:
##-conf.gccchecker:

ifcommon
# Common section
endifcommon

ifsimchecker
# SimChecker section
endifsimchecker

### makefile fragment for solaris 2
RANLIB = echo > /dev/null
LDFLAGS_FOR_CHKR = -r -Bstatic
SYMCTL_C = $(srcdir)/utils/symctl_elf.c
PL_FLAGS = -DPLCHECKER -DMDCHECKER -D__PIC__ -fPIC
ALL_PLCFLAGS = $(PL_FLAGS) $(ALL_CFLAGS)
ALL_PLASFLAGS = $(PL_FLAGS) $(ALL_ASFLAGS)

# With the -s (--scramble) option, symctl replace the first character of a
# hidden symbol by a `@'.  This prevent `ld.so' to use this symbol. 
# STB_LOCAL is not enough (Perhaps we have to create a DT_FILTER)
#
# -G creates a shared library.
# -B symbolic `binds references to global symbols to their definitions within
#             the object'.
# -z text disps a warning if there are relocs for .text

plchecker.o: $(PLOBJS) $(srcdir)/config/sparc/solaris2/plchecker.symctl ./symctl
	$(LD) -G -z text -B symbolic -e _init -o plchecker.o $(PLOBJS) -lc
	./symctl -s $(srcdir)/config/sparc/solaris2/plchecker.symctl plchecker.o

ALL_CFLAGS = $(CPPFLAGS) $(CFLAGS) -c -fPIC -DSIMCHECKER
ALL_ASFLAGS = $(ACPPFLAGS) -c -D__PIC__ -fPIC

checker.so.1: $(OBJS) $(srcdir)/config/sparc/solaris2/simchecker.symctl ./symctl
	$(LD) -G -z text -B symbolic -e _init -o checker.so.1 $(OBJS) -lc -lm
	./symctl -s $(srcdir)/config/sparc/solaris2/simchecker.symctl checker.so.1
