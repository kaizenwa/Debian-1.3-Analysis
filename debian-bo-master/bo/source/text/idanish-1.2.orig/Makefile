installdir=/usr/lib/ispell

all: dansk.hash

install: dansk.hash dansk.aff
	install -o root -g root -m 0644 dansk.hash $(installdir)
	install -o root -g root -m 0644 dansk.aff $(installdir)

dansk.hash: dansk.aff dansk.0
	buildhash dansk.0 dansk.aff dansk.hash

clean:
	rm -f dansk.0.stat dansk.hash dansk.0.cnt *~

#dansk.0: dansk.aff dansk.zip
#	unzip -p dansk.zip | tr -d '\015' | munchlist -l dansk.aff -v > dansk.0

