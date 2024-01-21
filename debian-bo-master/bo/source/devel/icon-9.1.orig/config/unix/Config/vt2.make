MAKE=make

OBJS=		cat.o tparse.o tlex.o tmem.o trans.o \
		   tlocal.o tree.o tsym.o vtmain.o util.o variant.o

HDRS=		../h/define.h ../h/config.h ../h/cpuconf.h ../h/gsupport.h \
		   ../h/proto.h ../h/mproto.h ../h/dproto.h \
		   ../h/typedefs.h ../h/cstructs.h tproto.h

COBJS=		../common/getopt.o  ../common/alloc.o \
                   ../common/filepart.o ../common/strtbl.o ../common/ipp.o

../vitran:	$(OBJS)
		$(CC) $(LDFLAGS) -o ../vitran $(OBJS) $(COBJS)

$(OBJS):	$(HDRS)

cat.o:		tree.h ../h/lexdef.h ../cat.c
		$(CC) -c $(CFLAGS) ../cat.c

variant.o:	tree.h ../variant.c
		$(CC) -c $(CFLAGS) ../variant.c

tlex.o:		../h/lexdef.h ../h/parserr.h ttoken.h tree.h \
		   ../common/lextab.h ../common/yylex.h ../common/error.h
tparse.o:	../h/lexdef.h tglobals.h tsym.h tree.h keyword.h
tmem.o:		tglobals.h tsym.h tree.h
toktab.o:	../h/lexdef.h ttoken.h
trans.o:	tglobals.h tsym.h ttoken.h tree.h ../h/version.h
tree.o:		tree.h
tsym.o:		tglobals.h tsym.h ttoken.h keyword.h
util.o:		tglobals.h tree.h ../h/fdefs.h
vtmain.o:	tglobals.h

../common/lextab.h ../common/yacctok.h ../common/fixgram ../common/pscript: \
				../common/tokens.txt ../common/op.txt
			cd ../common; make gfiles

tparse.c ttoken.h:	vgram.g trash ../common/pscript
			yacc -d vgram.g   # expect 218 shift/reduce conflicts
			./trash <y.tab.c | ../common/pscript >tparse.c
			mv y.tab.h ttoken.h
			rm -f y.tab.c

vgram.g:		vgrammar.c ident.h ../h/define.h ../h/grammar.h \
				../common/yacctok.h ../common/fixgram
			$(CC) -E -C vgrammar.c | ../common/fixgram >vgram.g

ident.h:		../bsyms ../usyms ../ident.defs ../variant.defs
			cd ..; ./define ident.defs variant.defs >itran/ident.h

define:			define.icn
			icont -s define.icn

trash:			trash.icn
			icont -s trash.icn
