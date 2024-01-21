include ../Makeconfig

# Enable debugging by adding the option  debug  to kerneld: "kerneld debug"
# (Debugging can also be enabled via the kdstat utility: "kdstat debug")
DEBUG=-DDEBUG

# If you have gdbm, and if you want to use it for persistent module storage,
# remove (or comment) the following line:
NO_GDBM=-DNO_GDBM

#----------------------------------------------------------------------

DEFS += $(DEBUG) $(NO_GDBM)

PROGS= kerneld

ifndef NO_GDBM
PROGS += admpersist
LDLIBS = -lgdbm
endif

all: $(PROGS)

kerneld: kerneld.o ../util/libutil.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

install install-bin: all
	for i in $(PROGS); do \
	  $(INSTALL) $$i $(SBINDIR); \
	done

install-scripts:
	$(INSTALL) request-route.sh $(SBINDIR)/request-route

UTILS= kdstat
TESTPROGS= kdsend

#
# Build the fake kerneld message generator: kdsend and the kdstat "utility"
# Used for debugging purposes only
#
test: all $(TESTPROGS)

utils: all $(UTILS)

install-utils: utils
	for i in $(UTILS); do \
	  $(INSTALL) $$i $(SBINDIR); \
	done

realclean clean:
	rm -f $(PROGS) $(TESTPROGS) $(UTILS) *.o .depend

depend dep:
	$(CC) -M $(CFLAGS) $(DEFS) *.c > .depend

check_persist.o: check_persist.c
	$(CC) $(CFLAGS) $(DEFS) -DMODULE -D__KERNEL__ -c check_persist.c

# include a dependency file if one exists

ifeq (.depend,$(wildcard .depend))
include .depend
endif
