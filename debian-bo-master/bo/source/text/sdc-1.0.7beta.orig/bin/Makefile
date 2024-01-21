#			    -*-makefile-*-
all: $(EXE) bbset converters

REMLOC=local

# GRPHPKG determines the graphic package to use for LaTeX
# either "grphcs" or "epsfig"

GRPHPKG=grphcs

STREAMTYPE=v.l			# either v.l or v.d; see stream.ssc

BIGLOO=bigloo
BIGLOOFLAGS= -I ./ -farithmetic -O

#---------------------------------------------------------------------------

.SUFFIXES:	.scm .sch .ssc
.scm.o:	
	$(BIGLOO) $(BIGLOOFLAGS) -c $*.scm 
.c.o:	
	$(CC) $(CCFLAGS) -c $*.c 

OBJS = strings.o control.o files.o chrproc.o procchar.o \
       compile.o rdp.o ns.o stream.o qsort.o dvrtstrm.o

compile.o: compile.scm compile.sch stream.sch
stream.o: stream.scm stream.sch
rdp.o: rdp.scm rdp.sch stream.sch

stream.scm:	# extract the stream module from documentation/source
stream.scm:	# CAUTION: doesn't work without a already made prg!!!
stream.sch: stream.ssc
stream.scm: stream.ssc
	./$(EXE) -V0 -L .. -O literate \
	  -i bmod -i scmprg -i scminc -i $(STREAMTYPE) $<

typeset.o: typeset.scm $(OBJS) 
	sed -e "s%@@LIBDIR@@%$(SR)%;s/@@VERSION@@/$(VERSION)/" \
		typeset.scm > typeset.tma
	if [ `basename $$PARSEPROG` = nsgmls ];                              \
	then sed "/HACK SGMLS/,/END HACK SGMLS/d" typeset.tma > typeset.tmb; \
	else mv typeset.tma typeset.tmb;                                     \
	fi
	$(BIGLOO) $(BIGLOOFLAGS) -c typeset.tmb
	rm -f typeset.tma typeset.tmb

sdc-sgmls: force
	if [ `basename $$PARSEPROG` = nsgmls ] ; \
	then echo $$PARSEPROG -E0 '$$*' > $@      ; \
	else echo $$PARSEPROG     '$$*' > $@      ; \
	fi
	chmod +x $@

typeset: # make the typeset program
$(EXE): sdc-sgmls $(OBJS) typeset.o
	$(BIGLOO) $(BIGLOOFLAGS) -o $(EXE) typeset.o $(OBJS) 
	strip $(EXE)

bbset:	bbset.sgml
	./$(EXE) -V0  -L .. -O literate bbset.sgml
	chmod +x bbset

converters:	# create the converter scripts
converters: ../doc/notations.sgml ../doc/epstogif.sgml
	echo "" > ../sgml/notation.ent
	./$(EXE) -V0 -L .. -O literate \
		-i $(REMLOC) -i mkinc ../doc/epstogif.sgml
	./$(EXE) -V0 -L .. -O literate -D. \
		-i $(REMLOC) -i $(GRPHPKG) ../doc/notations.sgml
	chmod +x *2* pstogif
	rm epstogif
	mv notation.ent ../sgml
	./$(EXE) -V0 -L .. -O literate bbset.sgml
	chmod +x bbset

install: # install (usually to be called from upper dir. Makefile)
install: e.dd+pwd
	$(CP) $(EXE) sdc-sgmls *2* *.sch \
		pstoppm.ps pstogif bb4gs.ps bbset bbset.sgml \
		$(DD)/bin

clean:	# delete superflous stuff
clean:
	rm -f *.o core

clobber: # delete rebuildable stuff
clobber: clean
	rm -f $(EXE) *2* bbset

e.dd+pwd:
	@if [ ! -d $(DD)/`basename \`pwd\`` ]; \
	then $(MKDIR) $(MKDIROPT) $(DD)/`basename \`pwd\`` ; else true; fi

force:

help:	# help on possible targets
	-@egrep "^[A-Za-z\._]+::?.*#" [Mm]akefile
