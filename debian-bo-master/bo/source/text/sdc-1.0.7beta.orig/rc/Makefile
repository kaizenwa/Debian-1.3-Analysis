#			    -*-makefile-*-

MKDIR=mkdir -p
CP=cp
MAKE=make

install: # install (usually to be called from upper dir. Makefile)
	$(MAKE) -e e.dir DIRECTORY=$(DD)/rc
	$(CP) `find * ! -path "CVS" ! -name "*~" -print` $(DD)/rc

clean:	# remove redundant files
	@touch core
	rm -f `find . \( -name "*~" -o -name core \) -print`

clobber: # remove all generated files (did I catch all?)
clobber: clean
	#rm -f $(NONSOURCE)

purge:	# same as clean & removes files containing only "deleted"
purge:  clean
	@$(ECHO) -n deleted	>deleted7
	@$(ECHO) deleted	>deleted8
	@rm -f `find . \
	       \( \( -size 8c -exec cmp -s {} deleted8 \; \)	\
	       -o \( -size 7c -exec cmp -s {} deleted7 \; \) \)	\
	       -print`
	@echo purged

wipe:	# use: make wipe file=<file> # prepares file for purge
	mv -f $(file) $(file)~
	echo deleted >$(file)

e.dir:
	@if [ ! -d $(DIRECTORY) ];\
	then $(MKDIR) $(MKDIROPT) $(DIRECTORY) ; else true; fi

help:	# Help on make targets
	@echo Warning! Targets in this Makefile are intented to be called
	@echo from the Makefile in the upper directory!
	@echo ------------------------------
	-@egrep "^[A-Za-z\._]+::?.*#" [Mm]akefile
