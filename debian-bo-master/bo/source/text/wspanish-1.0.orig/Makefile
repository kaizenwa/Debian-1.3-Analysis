LANG = spanish

all: $(LANG)

diacrit7to8: diacrit7to8.l
	flex -8 -f diacrit7to8.l
	gcc -O2 -fomit-frame-pointer -m486 lex.yy.c -lfl -o diacrit7to8
	@/bin/rm lex.yy.c

# Spanish uses diacriticals *and* two special characters...

spanish: words.spanish diacrit7to8
	cat words.spanish | ./diacrit7to8 >spanish

install: all
	install -o root -g root -m 644 $(LANG) $(prefix)/dict
	install -o root -g root -m 644 words.5 $(prefix)/man/man5/$(LANG).5

clean:
	rm -f $(LANG) diacrit7to8 *~ ./#*
