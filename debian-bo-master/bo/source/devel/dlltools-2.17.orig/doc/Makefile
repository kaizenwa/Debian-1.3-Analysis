all: README.ps

README.ps: README.tr
	groff -Tps -mgs -p -t README.tr >$@

README.txt: README.tr
	groff -Tascii -mgs -p -t README.tr >$@

README.dvi: README.tr
	groff -Tdvi -mgs -p -t README.tr >$@

README.tr: table.tr

table.tr: table_description ../tools/gentable
	../tools/gentable table_description >$@

../tools/gentable: ../tools/gentable.c
	cd ../tools; make gentable

clean:
	/bin/rm -f *~ table.tr
