include ../Makeconfig

#----------------------------------------------------------------------

all: genksyms

genksyms: genksyms.o parse.o lex.o ../util/libutil.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

parse.c: parse.y
	@echo "expect 6 shift/reduce conflicts and 5 reduce/reduce conflicts."
	bison -o $@ -dtv $^

lex.c: lex.l
	flex -o$@ -d $^

crc32.tab: makecrc32.c
	$(CC) $(CFLAGS) -o makecrc32 makecrc32.c
	./makecrc32 > $@.tmp
	mv $@.tmp $@

keywords.c: keywords.gperf
	gperf -a -C -E -g -H is_reserved_hash -k 1,3,$$ -N is_reserved_word -p -t $^ > $@.tmp
	mv $@.tmp $@

clean:
	rm -f *.o *.tmp parse.output genksyms makecrc32

realclean: clean
	rm -f parse.c parse.h lex.c crc32.tab keywords.c

install install-bin: all
	$(INSTALL) genksyms $(SBINDIR)

# auto-generated dependancies are almost redundant once we add all the 
# rules to get the generated files built first.
dep depend:

genksyms.o: genksyms.c crc32.tab genksyms.h \
  ../include/util.h ../include/version.h
lex.o: lex.c keywords.c genksyms.h parse.h ../include/util.h
parse.o: parse.c genksyms.h
