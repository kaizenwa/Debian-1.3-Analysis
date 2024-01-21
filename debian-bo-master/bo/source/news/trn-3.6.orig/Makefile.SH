case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting Makefile (with variable substitutions)"
case "$srcdir" in
'') srcdir='.';;
esac
$cat >Makefile <<!GROK!THIS!
# $Id: Makefile.SH,v 3.0 1992/03/01 02:13:32 davison Trn $
#
# This software is Copyright 1991 by Stan Barber. 
#
# Permission is hereby granted to copy, reproduce, redistribute or otherwise
# use this software as long as: there is no monetary profit gained
# specifically from the use or reproduction of this software, it is not
# sold, rented, traded or otherwise marketed, and this copyright notice is
# included prominently in any copy made. 
#
# The author make no claims as to the fitness or correctness of this software
# for any use whatsoever, and it is provided as is. Any use of this software
# is at the user's own risk. 
# 
# optional defines you can add to the CFLAGS
#    -DDEBUG                      compile in support for the -D option
#    -DNO_LISTGROUP               avoids attempting to use LISTGROUP
#    -DREPLYTO_POSTER_CHECKING    makes -p also check Reply-To (slow)

srcdir = $srcdir
VPATH = $vpath

CC = $cc
TOUCH = touch
#YACC = yacc
#LINT = lint
lintflags = -phbvxac $vincludes

CFLAGS = $optimize $ccflags $vincludes
LDFLAGS = $ldflags

rnbin = $installbin
rnlib = $installprivlib
mansrc = $installmansrc
manext = $manext
#NNTPNNTPFLAGS = $nntpflags
ndirc = $ndirc
ndiro = $ndiro
strftimec = $strftimec
strftimeo = $strftimeo
installfilexp = $filexp

libs = $libndir $termlib $jobslib -lm $libs
!GROK!THIS!
$cat >>Makefile <<'!NO!SUBS!'
public = trn newsetup newsgroups Pnews Rnmail trn-artchk $(nntpbin)
private = norm.saver mbox.saver makedir Pnews.header Speller filexp
util = makedepend newsnews
manpages = trn.1 Pnews.1 Rnmail.1 newsetup.1 newsgroups.1
#NNTPnntpbin=nntplist
#NNTPnntpsrc=nntpinit.c nntpclient.c nntpauth.c nntp.c
#NNTPnntpobj1=nntpinit.o nntpclient.o nntpauth.o
#NNTPnntpobj2=nntp.o

h1 = addng.h art.h artio.h artsrch.h autosub.h backpage.h bits.h cache.h 
h2 = charsubst.h common.h decode.h final.h hash.h head.h help.h init.h intrp.h
h3 = kfile.h last.h ng.h ngdata.h ngsrch.h ngstuff.h only.h rcln.h rcstuff.h
h4 = respond.h rthread.h rt-mt.h rt-ov.h rt-page.h rt-process.h rt-select.h
h5 = rt-util.h rt-wumpus.h search.h sw.h term.h trn.h util.h util2.h

h = $(h1) $(h2) $(h3) $(h4) $(h5)

c1 = addng.c art.c artio.c artsrch.c autosub.c backpage.c bits.c cache.c
c2 = charsubst.c decode.c edit_dist.c final.c hash.c head.c help.c init.c
c3 = intrp.c kfile.c last.c $(ndirc) ng.c ngdata.c nghash.c ngsrch.c ngstuff.c
c4 = only.c rcln.c rcstuff.c respond.c rthread.c rt-mt.c rt-ov.c
c5 = rt-process.c rt-page.c rt-select.c rt-util.c rt-wumpus.c search.c
c6 = $(strftimec) sw.c term.c trn.c util.c util2.c unship.c uudecode.c
c7 = $(nntpsrc) charsubst.c

c = $(c1) $(c2) $(c3) $(c4) $(c5) $(c6) $(c7)

obj1 = addng.o art.o artio.o artsrch.o autosub.o backpage.o bits.o cache.o
obj2 = charsubst.o decode.o edit_dist.o final.o hash.o head.o help.o init.o
obj3 = intrp.o kfile.o last.o $(ndiro) ng.o ngdata.o nghash.o ngsrch.o
obj4 = ngstuff.o only.o rcln.o rcstuff.o respond.o rthread.o rt-mt.o rt-ov.o
obj5 = rt-process.o rt-page.o rt-select.o rt-util.o rt-wumpus.o search.o
obj6 = $(strftimeo) sw.o term.o trn.o util.o util2.o unship.o uudecode.o
obj7 = parsedate.o $(nntpobj1) $(nntpobj2)

obj = $(obj1) $(obj2) $(obj3) $(obj4) $(obj5) $(obj6) $(obj7)

addedbymake = $(public) $(private) $(util) Makefile.old config.h\
	parsedate.c cppstdin all

# grrr
SHELL = /bin/sh

.c.o:
	$(CC) -c $(CFLAGS) $<

all: $(public) $(private) $(util) Makefile
	$(TOUCH) all

trn: $(obj)
	$(CC) $(LDFLAGS) $(obj) $(libs) -o trn
#NNTP
#NNTPnntpinit.o: nntpinit.c
#NNTP	$(CC) -c $(CFLAGS) $(NNTPFLAGS) $<
#NNTP
#NNTPnntplist: nntplist.o util2.o $(nntpobj1)
#NNTP	$(CC) $(LDFLAGS) nntplist.o util2.o $(nntpobj1) -o nntplist $(libs)

trn-artchk: trn-artchk.o util2.o $(nntpobj1)
	$(CC) $(LDFLAGS) trn-artchk.o util2.o $(nntpobj1) -o trn-artchk $(libs)

parsedate.c: parsedate.y
	@echo 'Expect 6 shift/reduce conflicts'
	$(YACC) $(srcdir)/parsedate.y
	mv -f y.tab.c parsedate.c
#STRFTIME
#STRFTIME$(strftimeo): $(strftimec)
#STRFTIME	$(CC) -c $(CFLAGS) $(strftimec)

unipatch: unipatch.o
	$(CC) $(LDFLAGS) unipatch.o -o unipatch

Pnews.header: Pnews

# if a .h file depends on another .h file...
$(h):
	-$(TOUCH) $@

install: $(public) $(private) $(manpages)
# won't work with csh
	export PATH || exit 1
	- (cd `./filexp $(rnbin)`; mv trn trn.old)
	- if test `pwd` != `./filexp $(rnbin)`; then cp $(public) `./filexp $(rnbin)`;\
 (cd `./filexp $(rnbin)`; chmod o+x $(public); strip trn trn-artchk $(nntpbin)); fi
	- if test $(installfilexp) = Rnfilexp; then cp ./filexp `./filexp $(rnbin)/Rnfilexp`; fi
	- ./makedir `./filexp $(rnlib)`
	- chmod o+r `./filexp $(rnlib)`
	- if test `pwd` != `./filexp $(rnlib)`; then cp $(srcdir)/INIT $(private) `./filexp $(rnlib)`; fi
	- if test ! -f `./filexp $(rnlib)/newsnews`; then cp newsnews `./filexp $(rnlib)`; fi
	- (cd `./filexp $(rnlib)`; chmod o+x $(private); chmod o+r INIT newsnews)
	- if test "X$(mansrc)" != "X" -a "X`pwd`" != X`./filexp $(mansrc)`; then \
for page in $(manpages); do \
dest=$(mansrc)/`basename $$page .1`.$(manext); \
rm -f $$dest; cp $(srcdir)/$$page $$dest; chmod 444 $$dest; \
done; \
fi

clean:
	@echo 'Use "make realclean" to also remove the Makefile.'
	@echo 'Use "make spotless" to even remove config.sh.'
	rm -rf UU
	rm -f *.o core $(addedbymake)

realclean:
	@echo 'You can use "Configure -S ; make depend ; make" to reverse this.'
	rm -rf UU
	rm -f *.o core $(addedbymake) Makefile

spotless:
	rm -rf UU .config
	rm -f *.o core $(addedbymake) Makefile config.sh

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint: $(c) parsedate.c
	$(LINT) $(lintflags) $(defs) $? > trn.fuzz 2>&1

sabertrn: $(c) parsedate.c
	#load $(c) parsedate.c $(libs)

depend: config.h Makefile makedepend
	./makedepend

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
!NO!SUBS!
if test -f dependencies; then
	$cat dependencies >>Makefile
else
	$cat $srcdir/dependencies >>Makefile
fi
case "$d_nntp" in
define)	sed < Makefile -e '/^#NNTP/s/^#NNTP//' > Makefile.new ;;
*)	sed < Makefile -e '/^#NNTP/d' > Makefile.new ;;
esac
case "$strftimec" in
''|' ')	sed < Makefile.new -e '/^#STRFTIME/d' > Makefile ;;
*)	sed < Makefile.new -e '/^#STRFTIME/s/^#STRFTIME//' > Makefile ;;
esac
rm Makefile.new
$eunicefix Makefile
