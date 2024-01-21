WORTE =	worte.txt verben.txt adjektive.txt klein.txt \
	geographie.txt vornamen.txt abkuerz.txt imperat.txt latein.txt \
	informatik.txt infoabk.txt elektronik.txt orgabk.txt marken.txt \
	worte2.txt zusammen.txt technik.txt compeng.txt

all: deutsch.hash deutsch

deutsch.hash: all.words deutsch.aff
	buildhash all.words deutsch.aff $@
 
all.words: $(WORTE)
	sort -u $+ > $@

clean:
	/bin/rm -f all.words all.words.cnt all.words.stat deutsch.hash hk*deutsch.tar.gz
	/bin/rm -f diacrit7to8 deutsch

sort:
	for i in *.txt;do sort -u -o $$i $$i;done

dist:	clean sort
	tar cvf - * | gzip -9 > hk2-deutsch.tar.gz

diacrit7to8: diacrit7to8.l
	flex -8 -f diacrit7to8.l
	gcc -O2 -fomit-frame-pointer -m486 lex.yy.c -lfl -o diacrit7to8
	@/bin/rm lex.yy.c

deutsch: all.words diacrit7to8
	./diacrit7to8 < all.words > $@
