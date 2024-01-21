
DIRS = tools example doc

INSTALL_DIRS = tools

all:
	for i in $(DIRS); do \
		(cd $$i && make); \
	done

clean:
	rm -f *~ \#*\#
	for i in $(DIRS); do \
		(cd $$i && make clean); \
	done

dist:	clean
	(mydir=`basename \`pwd\``;\
	cd .. && tar cvvf - $$mydir | gzip -9 > $${mydir}.tar.gz)

install:
	for i in $(INSTALL_DIRS); do \
		(cd $$i && make install); \
	done
