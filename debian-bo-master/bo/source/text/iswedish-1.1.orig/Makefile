installdir=/usr/lib/ispell

all: svenska.hash

install: svenska.hash svenska.aff
	install -o root -g root -m 0644 svenska.hash $(installdir)
	install -o root -g root -m 0644 svenska.aff $(installdir)

svenska.hash: svenska.aff svenska.0
	buildhash svenska.0 svenska.aff svenska.hash

clean:
	rm svenska.0.stat svenska.hash svenska.0.cnt   

