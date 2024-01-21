LIB=	../reglib.a
CFLAGS=	-I.

$(LIB): regexp.o regsub.o
	ar rcv $@ regexp.o regsub.o
	ranlib $@

clean:
	rm -f regexp.o regsub.o $(LIB)
