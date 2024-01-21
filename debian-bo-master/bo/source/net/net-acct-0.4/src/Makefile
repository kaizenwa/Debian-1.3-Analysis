ifeq ($(ARCH),)
ARCH = linux
endif
VPATH = ../
EXEC = nacctd
CFLAGS = -Wall -Wstrict-prototypes
OBJECTS = main.o daemon.o process.o utils.o config.o
LIBS =
CC = gcc

ifeq ($(ARCH),linux)
OBJECTS := $(OBJECTS) capture-linux.o 
endif

ifeq ($(ARCH),sun-solaris)
OBJECTS := $(OBJECTS) capture-sun-solaris.o read-kern.o
LIBS := $(LIBS) -lelf -lkvm -lnsl -ldl -lsocket
endif

all:	$(EXEC)

depend dep:
	for i in *.c;do $(CPP) -M $$i;done > .tmpdepend
	mv .tmpdepend .depend

$(EXEC):	$(OBJECTS)
		$(CC) -o $(EXEC) $(CFLAGS) $^ $(LIBS)


install:	all
	install -d $(prefix)/sbin
	install -o root -g root nacctd $(prefix)/sbin/

distclean:	clean
	rm -f *~ *.orig DEADJOE

clean:
	rm -f *.o $(EXEC)

ifeq (.depend,$(wildcard .depend))
include .depend
endif
