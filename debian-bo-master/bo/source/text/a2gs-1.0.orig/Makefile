#
# This makefile is way overly complicated for this
# program, but this is the same makefile I use with
# other more complicated packages
#

CC     = gcc
CFLAGS = -Wall -s -O2 -m486
SHELL  = /bin/bash
A2GS   = a2gs.o getallopt.o
MV     = /bin/mv
CP     = /bin/cp
CAT    = /usr/bin/cat
AWKPROG='{if($$2 == "::"){sub($$1 " ::",c,$$0);c=""};print $$0}'
SED    = /usr/bin/sed
GAWK   = /usr/bin/awk
TEE    = /usr/bin/tee
FIND   = /usr/bin/find

.PHONY: all clean dep depend dummy install mrproper


all: .depend a2gs

clean: ; $(RM) core.* *.o .depend~{,~}

mrproper: clean ; $(RM) a2gs .depend .depend~{,~}

#
# If the .depend file doesn't exist, then we must create
# it.  Then recursively call make to create what was asked for.
#

ifneq (.depend,$(wildcard .depend))

a2gs install: .depend
	$(MAKE) $@

DEPEND = dummy

else

include .depend

a2gs: .depend $(A2GS)
	$(CC) $(CFLAGS) $(A2GS) -o $@

install: .depend all
	$(CP) a2gs /usr/bin
	$(CP) a2gs.1 /usr/man/man1

endif

# Ok this is the make rules the one problem with this is that if 
# one of the files is deleted, the makefile will fail.  For that
# reason I define a dummy rule for each of the source/include files.

.depend depend dep:: $(DEPEND)
	$(FIND) . -name '*.c' -maxdepth 1 -exec $(CPP) -M '{}' ';'|\
          $(SED) 's/:/::/'|$(TEE) .depend~|\
	  $(GAWK) -v c="DEPEND = " $(AWKPROG)|\
          ( $(SED) 's/ $$/ \\/';echo ' ';\
          echo '.depend :: $$(sort $$(DEPEND))' )>.depend~~
	$(CAT) .depend~{,~}>.depend
	$(RM) .depend~{,~}

$(sort $(DEPEND)) : ;

