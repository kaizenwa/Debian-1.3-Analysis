# makefile for rawkey lib.
# do a 'make install' as root.

# you might want to change these, but probably not.
LIBDIR=/usr/lib
INCLUDEDIR=/usr/include

CFLAGS=-I.	# for 'testprog'

ifeq ($(strip $(TOP_LEVEL)),)
all: err_msg
else
all: librawkey.a 
endif

librawkey.a: rawkey.o
	ar rcs librawkey.a rawkey.o

clean:
	rm -f *.o *~ librawkey.a testprog testvt a b c

err_msg:
	@echo '+-------------------------------------------+'
	@echo '| Please run the top level Makefile instead |'
	@echo '|            of this one                    |'
	@echo '+-------------------------------------------+'