# This is a Makefile for nmake that makes all the .cxx and .h files that
# are automatically generated.  It's too painful to do in the IDE.
# You'll need m4 and perl to use this.

M4=m4
PERL=perl

GENSRCS=lib\entmgr_inst.cxx \
lib\xentmgr_inst.cxx \
lib\parser_inst.cxx \
lib\app_inst.cxx \
lib\arc_inst.cxx \
lib\ArcEngineMessages.h \
lib\EntityManagerMessages.h \
lib\CatalogMessages.h \
lib\MessageReporterMessages.h \
lib\PosixStorageMessages.h \
lib\URLStorageMessages.h \
lib\StdioStorageMessages.h \
lib\ParserMessages.h \
lib\ParserAppMessages.h \
lib\CmdLineAppMessages.h \
lib\version.h \
nsgmls\nsgmls_inst.cxx \
nsgmls\RastEventHandlerMessages.h \
nsgmls\NsgmlsMessages.h \
spam\SpamMessages.h \
spam\spam_inst.cxx \
spent\spent_inst.cxx \


.SUFFIXES: .m4 .msg

all: $(GENSRCS)


.m4.cxx:
	rm -f $@
	$(M4) lib\instmac.m4 $< >$@
	chmod -w $@

{lib}.msg{lib}.h:
	rm -f $@
	$(PERL) -w msggen.pl -l $<
	chmod -w $@

{nsgmls}.msg{nsgmls}.h:
	rm -f $@
	$(PERL) -w msggen.pl $<
	chmod -w $@

{spam}.msg{spam}.h:
	rm -f $@
	$(PERL) -w msggen.pl -l $<
	chmod -w $@

lib\version.h: lib\mkversion.pl VERSION
	rm -f $@
	$(PERL) -w lib\mkversion.pl VERSION >$@
	chmod -w $@
