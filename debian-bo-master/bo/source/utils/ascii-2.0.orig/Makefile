# ascii -- interactive ASCII reference

# Note: When the version changes, you also have to change
#  * the name of the containing directory
#  * the RPM spec file
V=2.0

ascii: ascii.c

CFLAGS = -O

clean:
	rm -f ascii ascii.tar* ascii.shar

ascii-$(V).tar: Makefile ascii.c ascii.1 ascii.lsm
	cd ..; tar cvf ascii-$(V)/ascii-$(V).tar ascii-$(V)/Makefile ascii-$(V)/ascii.c ascii-$(V)/ascii.1 ascii-$(V)/ascii.lsm ascii-$(V)/ascii.spec 

ascii-$(V).tar.gz: ascii-$(V).tar
	gzip -f ascii-$(V).tar

ascii-$(V).shar:
	shar Makefile ascii.c ascii.1 ascii.lsm ascii-$(V)-1.spec >ascii-$(V).shar

rpm: ascii-$(V).tar.gz
	cp ascii-$(V).tar.gz /usr/src/SOURCES
	cp ascii.spec /usr/src/SPECS/ascii-$(V)-1.spec
	rpm -ba ascii-$(V)-1.spec
