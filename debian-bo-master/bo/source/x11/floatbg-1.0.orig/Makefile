
floatbg: floatbg.o
	cc -O -o floatbg floatbg.o -lX11 -lm

floatbg.o: floatbg.c
	cc -O -c floatbg.c

clean:
	rm -f floatbg floatbg.o floatbg.shar core a.out

shar:
	shar -h floatbg.shar README Makefile Imakefile floatbg.c floatbg.man
