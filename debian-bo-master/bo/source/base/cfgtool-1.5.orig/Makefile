CC=gcc
CFLAGS=-Wall -O2 -D__publib__ 
LDFLAGS=-s
LIBS=-lpub

baseversion=$(shell echo ${version} | sed 's/-.*//')
cvsversion=$(shell echo ${version} | tr .- __)

cfgtool: cfgtool.o
	$(CC) $(LDFLAGS) -o $@ cfgtool.o $(LIBS)

cfgtool.o: cfgtool.c
	$(CC) $(CFLAGS) -c -o $@ $*.c

clean:
	rm -f core cfgtool *.o

export: CVS/Entries
	if test -z "${version}"; then echo 'No $$version.'; exit 1; fi
	sudo rm -rf cfgtool-${baseversion}
	cvs export -d cfgtool-${baseversion} -r version_${cvsversion} cfgtool

dist: export
	tar cf - cfgtool-${baseversion} | gzip -9 > cfgtool-${version}.tar.gz
	rm -rf cfgtool-${baseversion}

deb: export
	cp -a cfgtool-${baseversion} cfgtool-${baseversion}.orig
	cd cfgtool-${baseversion} && dpkg-buildpackage -rsua
