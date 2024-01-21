fdflush: fdflush.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o fdflush $<

install: fdflush fdflush.1
	install -o root -g root -m 755 fdflush $(prefix)/bin/fdflush
	install -o root -g root -m 644 fdflush.1 \
		$(prefix)/usr/man/man8/fdflush.8

clean distclean:
	rm -f fdflush *.o
