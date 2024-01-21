SHELL=/bin/sh
O=o
.SUFFIXES:	.c .$O
TARG=tcs
# you may need to add strerror if you don't have one
OFILES=tcs.$O conv_jis.$O conv_big5.$O conv_gb.$O utf.$O kuten208.$O \
	gb.$O big5.$O version.$O conv_ksc.$O ksc.$O
#SGIFLAGS= -ansiposix -woff 100			# SGI flags
CFLAGS= -D_POSIX_SOURCE $(SGIFLAGS)

$(TARG):	$(OFILES)
	$(CC) $(CFLAGS) -o $@ $(OFILES)
.c.$O:
	$(CC) $(CFLAGS) -c $*.c

tcs.$O utf.$O:		hdr.h plan9.h
tcs.$O:			jis.h
tcs.$O kuten208.$O:	kuten208.h
tcs.$O big5.$O:		big5.h
tcs.$O:			cyrillic.h
tcs.$O:			conv.h
conv_jis.$O:		conv.h plan9.h
conv_big5.$O:		conv.h plan9.h
conv_gb.$O:		conv.h plan9.h

clean:
	rm -f $(TARG) *.$O *.utf


#example tcs output files

ex01.utf:	tcs ex01.src
	./tcs -sf jis ex01.src > $@
ex02.utf:	tcs ex02.src
	./tcs -sf jis ex02.src > $@
ex03.utf:	tcs ex03.src
	./tcs -sf koi8 < ex03.src > $@
ex04.utf:	tcs ex04.src
	./tcs -sf ucode < ex04.src > $@
ex05.utf:	ex05.src
	./tcs < ex05.src > $@
ex06.utf:	tcs ex06.src
	./tcs -sf big5 < ex06.src > $@
ex07.utf:	tcs ex07.src
	./tcs -sf sf2 < ex07.src > $@
ex08.utf:	tcs ex08.src
	./tcs -sf euc-k < ex08.src > $@
ex09.utf:	tcs ex09.src
	./tcs -sf viet1 < ex09.src > $@
ex10.utf:	tcs ex10.src
	./tcs -sf viet2 < ex10.src > $@
ex11.utf:	tcs ex11.src
	./tcs -sf viscii < ex11.src > $@
ex12.utf:	tcs ex12.src
	./tcs -sf 8859-10 < ex12.src > $@

export:
	echo "char version[] = \"`date`\";" > version.c
	bundle Makefile README *.[ch] regress bundle bbundle tcs.1 tcs.ps > export
	echo "chmod +x regress bundle bbundle" >> export
	bbundle ex*.src ex*.ok >> export
