# -D_GNU_SOURCE is needed because environ is used in init.c
CFLAGS= -O2 -fomit-frame-pointer -fno-builtin -D_GNU_SOURCE

LDFLAGS= -s
STRIP= strip --remove-section=.note --remove-section=.comment busybox
# CFLAGS= -g -Wall -D_GNU_SOURCE

LIBRARIES=

OBJECTS=  $(patsubst %.c,%.o,$(wildcard *.c))

all: busybox $(LINKS)

busybox: $(OBJECTS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o busybox $(OBJECTS) $(LIBRARIES)
	$(STRIP)

clean:
	- rm -f $(OBJECTS) busybox *~

distclean: clean
	- rm -f busybox
