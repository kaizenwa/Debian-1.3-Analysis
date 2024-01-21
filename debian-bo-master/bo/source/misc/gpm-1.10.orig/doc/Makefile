#			-*-text-*-
#
# Makefile for the documentation directory (gpm-Linux)
#
# Copyright 1994   rubini@ipvvis.unipv.it (Alessandro Rubini)
#
#################
#
# BE CAREFUL in editing:
#	due to the large number of index files, and my use of a non standard
#	info input file, any file $(TARGET).* is removed by "make clean"
#
#	I chose to use a prefix for the input file ("doc.$(TARGET)"), to ease
#	makeing clean and applying my own rules.
#
###################################################################

%.texinfo: doc.%
	sed -f ./infofilter $< > $@
	chmod 400 $@


# This rule is somewhat a rewrite of texi2dvi. I like make more than sh :-)

%.dvi: %.texinfo

# create a spurious  index file to please silly sh (bash will work anyway
	touch $*.oo

# get the index list
	if test "x`ls $*.?? $*.aux`" != "x"; then \
	  for i in `ls $*.?? $*.aux`; do \
	    cp $$i $$i~; \
	  done; \
	fi
	tex $<

#check the file list, the file and if needed run TeX again
	old="`ls $*.??~ $*.aux~ | sed 's/~//g'`"; \
	new="`ls $*.?? $*.aux`"; \
	need="n"; \
	if test "$$old" != "$$new"; then need="y"; \
	else \
	  for i in  `ls $*.?? $*.aux`; do \
	    cmp -s $$i $$i~; if test $$? -ne 0; then need="y" break; fi; \
	  done; \
	fi; \
	if test "$$need" = "y"; then \
	  texindex $*.?? && tex $<; \
	fi


%.ps: %.dvi
	dvips -f $< > $@


%.info: %.texinfo
	makeinfo $< -o $@


#%.man: doc.%
#		manpages are created by the toplevel Makefile

%doc.txt: %.info
	./mktxt $< > $@

##############################################


TARGET = gpm
ALL    = $(TARGET).ps  $(TARGET).info $(TARGET)doc.txt

all: $(ALL)

info: $(TARGET).info

$(TARGET)doc.txt: mktxt

check: _err.ps
	gs -sDEVICE=linux -r320x200x16 $<

gs: $(TARGET).ps
	gs -sDEVICE=linux -r640x480x2 $<

gv: $(TARGET).ps
	ghostview $(TARGET).ps -magstep -1 -a4

mpage: all
	mv $(TARGET).ps $(TARGET)1.ps
	mpage -2AP $(TARGET)1.ps > $(TARGET)2.ps && rm $(TARGET)1.ps

terse:
	rm -f *~ *.dvi *.log *.aux \
	      $(TARGET).*.bak $(TARGET).??? $(TARGET).texinfo
	-mv $(TARGET).ps PS; rm -f $(TARGET).??; mv PS $(TARGET).ps

clean: terse
	rm -f $(ALL)

