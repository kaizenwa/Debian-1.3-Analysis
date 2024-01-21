ifeq ($(HOSTNAME),wild-wind)
LINUXTREE = /home/maz/linux
CFLAGS = -O2 -Wall -I$(LINUXTREE)/include
else
CFLAGS = -O2 -Wall

# Change the following line if your linux tree doesn't live in /usr/src...
LINUXTREE = /usr/src/linux

endif

SBIN = /sbin
MAN = /usr/man
LINKS = mdrun mdstop

all : mdadd mdcreate

mdcreate : mdcreate.o mdparse.o crc.o

mdadd : mdadd.o mdparse.o crc.o

install : all
	install mdadd $(SBIN)
	install mdcreate $(SBIN)
	install mdadd.8 $(MAN)/man8
	install mdcreate.8 $(MAN)/man8
	install mdtab.5 $(MAN)/man5
	for i in $(LINKS);\
	do\
		if [ ! -e $(SBIN)/$$i ]; then\
			ln -s $(SBIN)/mdadd $(SBIN)/$$i;\
		fi;\
		ln -sf $(MAN)/man8/mdadd.8 $(MAN)/man8/$$i.8;\
	done
	maj=`grep MD_MAJOR $(LINUXTREE)/include/linux/major.h | tr -s ' ' | cut -d' ' -f3`;\
	for i in 0 1 2 3;\
	do\
		if [ ! -e /dev/md$$i ]; then\
			mknod -m 0600 /dev/md$$i b $$maj $$i;\
		fi;\
	done

clean :
	rm -f mdadd mdcreate *.o core* *~ *.bak

dist : clean
	cd ..;tar cf - md | gzip -9c >md_current.tar.gz

patch :
	- cd ..;diff -ru --new-file /usr/src/linux linux > md/mdpatch
