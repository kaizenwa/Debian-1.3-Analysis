
all: pwdb.html pwdb.txt pwdb.ps

pwdb.html: pwdb.sgml
	sgml2html pwdb.sgml
	if [ ! -d ./html ]; then mkdir -p ./html ; fi
	mv *.html html

pwdb.txt: pwdb.sgml
	sgml2txt pwdb.sgml

pwdb.ps: pwdb.sgml
	sgml2ps pwdb.sgml

clean:
	rm *~ -rf html *.txt *.ps
