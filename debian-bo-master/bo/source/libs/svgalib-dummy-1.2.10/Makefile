CC     = gcc
CFLAGS = -O2 -fomit-frame-pointer -fPIC
DESTDIR = /

MAJOR_VER = 1
MINOR_VER = 2.10
VERSION   = $(MAJOR_VER).$(MINOR_VER)

LIBBASE = libvga libvgagl
LIBS = $(addsuffix .so.$(VERSION),$(LIBBASE))

HEADERS = vga.h vgagl.h vgamouse.h vgakeyboard.h

all: $(LIBS)

libvga.so.$(VERSION): vga.o
	$(CC) -s -shared -Wl,-soname,libvga.so.$(MAJOR_VER) \
	  -o libvga.so.$(VERSION) $^

libvgagl.so.$(VERSION): vgagl.o
	$(CC) -s -shared -Wl,-soname,libvgagl.so.$(MAJOR_VER) \
	  -o libvgagl.so.$(VERSION) $^

install: $(LIBS)
	for lib in $(LIBBASE); do \
		install -m755 -o root -g root $$lib.so.$(VERSION) $(DESTDIR)/usr/lib; \
		ln -s $$lib.so.$(VERSION) $(DESTDIR)/usr/lib/$$lib.so.$(MAJOR_VER); \
		ln -s $$lib.so.$(VERSION) $(DESTDIR)/usr/lib/$$lib.so; \
	done
	for f in $(HEADERS); do \
		install -m644 -o root -g root $$f $(DESTDIR)/usr/include; \
	done

clean:
	rm -f *.o lib*.so*
