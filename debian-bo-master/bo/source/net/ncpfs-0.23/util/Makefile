#
# Makefile for the linux ncp-filesystem routines.
#

USERUTILS = slist pqlist nwfsinfo pserver nprint nsend ncopy
UIDUTILS = ncpmount ncpumount
SBINUTILS = nwmsg

UTIL_EXECS = $(USERUTILS) $(UIDUTILS) $(SBINUTILS)
UTILS = $(addprefix $(INTERM_BINDIR)/,$(UTIL_EXECS))

#CFLAGS = -Wall $(INCLUDES)  $(KERNELD) -g -DNCPFS_VERSION=\"$(VERSION)\"
CFLAGS = -Wall $(INCLUDES) $(KERNELD) -O2 -g -DNCPFS_VERSION=\"$(VERSION)\"

CC = gcc

default:
	make -C ..

all: $(UTILS) ncptest

install: all
	for i in $(UTIL_EXECS); \
	do install -s $(INTERM_BINDIR)/$$i -m 755 $(BINDIR); done
	for i in $(UIDUTILS); \
	do install -s $(INTERM_BINDIR)/$$i -m 4755 $(BINDIR); done
	for i in $(SBINUTILS); \
	do install -s $(INTERM_BINDIR)/$$i -m 755 $(SBINDIR); done

$(UTILS): $(addsuffix .o,$(UTIL_EXECS)) libncp.a
	$(CC) -o $@ $(addsuffix .o,$(notdir $@)) -L. -lncp

ncplib.o: ncplib.c ncplib.h ncplib_err.h
	$(CC) $(CFLAGS) -finline-functions -c ncplib.c

COM_ERR_CFILES = com_err/com_err.c com_err/error_message.c com_err/et_name.c \
	com_err/init_et.c

libncp.a: ncplib.o ncplib_err.o $(COM_ERR_CFILES)
	make -C com_err
	ar r libncp.a ncplib.o ncplib_err.o \
	com_err/com_err.o com_err/error_message.o com_err/et_name.o \
	com_err/init_et.o

ncplib_err.h: ncplib_err.et
	com_err/compile_et ncplib_err

ncplib_err.c: ncplib_err.et
	com_err/compile_et ncplib_err

test: test.o ncplib.o
	$(CC) -o test test.o ncplib.o

ncptest: ncptest.o libncp.a
	$(CC) -o ncptest ncptest.o -L. -lncp

dep: ncplib_err.h
	make -C com_err dep
	$(CPP) -M $(INCLUDES) *.c > .depend

clean:
	make -C com_err clean
	rm -f *.o *~ slist test ncptest ncplib_err.[ch] libncp.a

mrproper: clean
	make -C com_err mrproper
	rm -f $(UTILS) .depend $(DISTFILE)

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif

