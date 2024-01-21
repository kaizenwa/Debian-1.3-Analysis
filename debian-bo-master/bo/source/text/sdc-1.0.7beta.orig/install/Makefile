#			    -*-makefile-*-
# Custumizable names:

CC=cc
CFLAGS   = # -D_XOPEN_SOURCE
CPPFLAGS =-I.
all:	install

getopt.o: getopt.c getopt.h

install: # build install
install: install.o getopt.o
	$(CC) $(LDFLAGS) -o $@ $?
	strip $@

clean:	# remove redundant files
	rm -f *.o *~

clobber: # remove all generated files (did I catch all?)
clobber: clean
	rm -f install

help:	# Help on make targets
	-@egrep "^[A-Za-z\._]+::?.*#" [Mm]akefile

# Local Variables:
# outline-regexp: ".+:[ \t]+#"
# End:
