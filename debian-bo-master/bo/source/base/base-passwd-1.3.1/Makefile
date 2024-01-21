all:

install: passwd group
	install -m 644 -o root -g root passwd debian/tmp/etc
	install -m 644 -o root -g root group debian/tmp/etc

clean:
